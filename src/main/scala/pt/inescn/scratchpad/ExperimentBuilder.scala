package pt.inescn.scratchpad

import scala.language.higherKinds

import _FCT._

/*
 * Cake pattern
 * Mario
 * Spark MLLib pipes
 * Scalding
 * Cascading
 * https://github.com/mandar2812/DynaML
 * 
 * T - Transform
 * D - Data
 * P - Parameters
 * R - Results (prediction, regression, clusters, etc.)
 * M - Metrics for evaluation
 */

trait PreProcessor[ T, D ] {
  def preProcess( prcessing: T, data: D ): D = ???

}

trait Learner[ T, P, D, R, M ] {

  def fit( params: P, data: D ): R = ???
  def evaluate( params: P, testData: D ): M = ???

}

class ModelA[ T, P, D, R, M ]( p: PreProcessor[ T, D ] ) extends Learner[ T, P, D, R, M ] {

}

class MLPipe[ D ]( data: D ) {
  def execute(): D = ???
}

/*
 * @see https://github.com/prnicolas/ScalaMl 
 * @see https://github.com/prnicolas/ScalaMl/tree/master/src/main/scala/org/scalaml/workflow
 * @see http://www.lihaoyi.com/post/StrategicScalaStylePracticalTypeSafety.html
 */

trait PipeOperator[ -T, +U ] {
  def |>( data: T ): Option[ U ]
}

/*
 *  core, stats, workflow
 * 
 */

/**
 * <p>Generic definition of a Monad used as a template for creating transforms.</p>
 * @author Patrick Nicolas
 * @since December 21, 2013
 * @note Scala for Machine Learning Chapter 2 Hello World!/Designing a workflow /
 * Monadic data transformation
 */
trait Monad[ M[ _ ] ] {
  def apply[ T ]( t: T ): M[ T ]
  def map[ T, U ]( m: M[ T ] )( f: T => U ): M[ U ]
  def flatMap[ T, U ]( m: M[ T ] )( f: T => M[ U ] ): M[ U ]
}

/**
 * <p>Monadic container which implements/adapter the most commonly used
 * Scala higher order methods.This class should not be used directly as they
 * do not validate any method argument and internal state.</p>
 * @param _fct element contained and managed by the monadic wrapper
 * @constructor Create a monadic container for data transformation.
 * @author Patrick Nicolas
 * @since December 23, 2013
 * @note Scala for Machine Learning Chapter 2 Hello World!/Designing a workflow /
 * Monadic data transformation
 * 
 * @see https://github.com/carrot-garden/quant_ScalaMl/blob/master/src/main/scala/org/scalaml/core/Monad.scala
 * @see https://github.com/prnicolas/ScalaMl/blob/master/src/main/scala/org/scalaml/core/functional/Monad.scala
 */
class _FCT[ +T ]( val _fct: T ) {

  /**
   * Access the element of type T contained in this instance
   * @return element of type T
   */
  def apply: T = _fct
  /**
   * Implementation of the map method
   * @param  c function that converts from type T to type U
   */
  def map[ U ]( c: T => U ): _FCT[ U ] = new _FCT[ U ]( c( _fct ) )
  /**
   * Implementation of flatMap
   * @param  c function that converts from type T to a monadic container of type U
   */
  def flatMap[ U ]( f: T => _FCT[ U ] ): _FCT[ U ] = f( _fct )
  /**
   * Implementation of filter for the monadic container
   * @param  p function that test a condition on the element
   */
  def filter( p: T => Boolean ): _FCT[ T ] = if ( p( _fct ) ) new _FCT[ T ]( _fct ) else zeroFCT( _fct )
  /**
   * implementation of the reduce method
   * @param f reducer/aggregator/accumulator function applied to the element
   */
  def reduceLeft[ U ]( f: ( U, T ) => U )( implicit c: T => U ): U = f( c( _fct ), _fct )
  /**
   * implementation of fold
   * @param f reducer/aggregator/accumulator function applied to the element
   */
  def foldLeft[ U ]( zero: U )( f: ( U, T ) => U )( implicit c: T => U ): U = f( c( _fct ), _fct )
  /**
   * implementation of the foreach loop
   * @param p immutable method that process the element of the monadic container
   */
  def foreach( p: T => Unit ): Unit = p( _fct )
}

/**
 * Companion object for _FCT that define the constructor apply and the zero
 * value. Its main purpose is to define a constructor and the Zero method.
 */
object _FCT {
  /**
   * Define the zero value for the FCT monad.
   * @param fct contained (i.e. data transformation) element
   * @return an instance of the container with a contained element, fct
   */
  def zeroFCT[ T ]( fct: T ): _FCT[ T ] = new _FCT[ T ]( fct )

  /**
   * Generic constructor for the container class.
   * @param item wrapped in the monadic container
   * @return an instance of the monadic container.
   */
  def apply[ T ]( t: T ): _FCT[ T ] = new _FCT[ T ]( t )
}

class Transform[ -T, +U ]( val op: PipeOperator[ T, U ] ) extends _FCT[ Function[ T, Option[ U ] ] ]( op.|> ) {
  def |>( data: T ): Option[ U ] = _fct( data )
}

/**
 *  Cross validation
 *  . split data -> call learner -> collect learner evaluation -> aggregate -> evaluate aggregate evaluations
 *
 *  Parameter Search
 *  . generate combinations of parameters -> fit : CV, Learner -> aggregate -> evaluate aggregate evaluations
 *
 *  Learner
 *  . fit -> evaluate
 *
 */
object ExperimentBuilder {

  def main( args: Array[ String ] ) {
    val op = new PipeOperator[ Int, Double ] {
      def |>( n: Int ): Option[ Double ] = Some( Math.sin( n.toDouble ) )
    }

    def g( f: Int => Option[ Double ] ): ( Int => Long ) = {
      ( n: Int ) =>
        {
          f( n ) match {
            case Some( x ) => x.toLong
            case None      => -1L
          }
        }
    }
    
    val gof = new Transform[ Int, Double ]( op ).map( g( _ ) )
    println(gof)
    gof.foreach(println)
  }

}

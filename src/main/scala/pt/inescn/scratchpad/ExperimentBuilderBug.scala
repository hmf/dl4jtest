package pt.inescn.scratchpad

import scala.language.higherKinds

trait APipeOperator[ -T, +U ] {
  def |>( data: T ): Option[ U ]
}

import A_FCT._

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
class A_FCT[ +T ]( val _fct: T ) {

  /**
   * Access the element of type T contained in this instance
   * @return element of type T
   */
  def apply: T = _fct
  /**
   * Implementation of the map method
   * @param  c function that converts from type T to type U
   */
  def map[ U ]( c: T => U ): A_FCT[ U ] = new A_FCT[ U ]( c( _fct ) )
  /**
   * Implementation of flatMap
   * @param  c function that converts from type T to a monadic container of type U
   */
  def flatMap[ U ]( f: T => A_FCT[ U ] ): A_FCT[ U ] = f( _fct )
  /**
   * Implementation of filter for the monadic container
   * @param  p function that test a condition on the element
   */
  def filter( p: T => Boolean ): A_FCT[ T ] = if ( p( _fct ) ) new A_FCT[ T ]( _fct ) else AzeroFCT( _fct )
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
 * Companion object for A_FCT that define the constructor apply and the zero
 * value. Its main purpose is to define a constructor and the Zero method.
 */
object A_FCT {
  /**
   * Define the zero value for the FCT monad.
   * @param fct contained (i.e. data transformation) element
   * @return an instance of the container with a contained element, fct
   */
  def AzeroFCT[ T ]( fct: T ): A_FCT[ T ] = new A_FCT[ T ]( fct )

  /**
   * Generic constructor for the container class.
   * @param item wrapped in the monadic container
   * @return an instance of the monadic container.
   */
  def apply[ T ]( t: T ): A_FCT[ T ] = new A_FCT[ T ]( t )
}

class ATransform[ -T, +U ]( val op: APipeOperator[ T, U ] ) extends A_FCT[ Function[ T, Option[ U ] ] ]( op.|> ) {
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
object ExperimentBuilderBug {

  def main( args: Array[ String ] ) {
    val op = new APipeOperator[ Int, Double ] {
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
    
    val gof = new ATransform[ Int, Double ]( op ).map( g( _ ) )
    println(gof._fct(0))
    gof.foreach(println)
  }

}

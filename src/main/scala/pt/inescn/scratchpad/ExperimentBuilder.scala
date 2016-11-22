package pt.inescn.scratchpad

import scala.language.higherKinds

/**
 * Generic definition of a Monad used as a template for creating implicit and explicit data
 * transformations
 * @tparam M Type of the data transformation or container
 * @author Patrick Nicolas
 * @since December 21, 2013 0.98.1
 * @version 0.98.1
 * @see Scala for Machine Learning Chapter 1 Getting started / Monads and higher kinds
 */
trait _Monad[ M[ _ ] ] {
  def unit[ T ]( t: T ): M[ T ]
  def map[ T, U ]( m: M[ T ] )( f: T => U ): M[ U ]
  def flatMap[ T, U ]( m: M[ T ] )( f: T => M[ U ] ): M[ U ]
}

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

import scala.util.Try //Success or Failure

/*
 * http://stackoverflow.com/questions/9594358/how-to-elegantly-implement-the-pipeline-pattern-using-scala
 * https://github.com/pthariensflame/scala-pipes
 * 
 */

trait Data[ D ]
trait PrePorcessedData[ D ]
//trait Model[P[ _, _ ] <:< PreProcessor[_,_] ]

/**
 * Normalization, Sampling, MovingAverage
 */
trait PreProcessor[ T, D ] {
  def preProcess( prcessing: T, data: D ): Try[ D ] = ???
}

trait Learner[ T, P, D, R, M ] {
  def fit( params: P, data: D ): Try[ R ] = ???
  def predict( params: P, testData: D ): Try[ M ] = ???
}

trait Evaluator[ T, P, D, R, M ] {
  def evaluate( params: P, testData: D ): Try[ M ] = ???
}

class ModelA[ T, P, D, R, M ]( p: PreProcessor[ T, D ] ) extends Learner[ T, P, D, R, M ] {
}

import scala.collection.GenTraversableOnce

/**
 * @see https://medium.com/@anicolaspp/operator-in-scala-cbca7b939fc0#.c29s6e8w3
 * @see https://github.com/loverdos/scalapipes
 *         Uses "extend" + apply instead of an implicit conversion
 * @see http://stackoverflow.com/questions/34646526/pipeline-operator-in-scala
 */
class Pipe[ A ]( a: A ) {
  def |>[ B ]( f: A => B ) = f( a )
  def $$[B](f: A => B): A = {f(a); a}
  def #!(str: String = ""): A = {print(str) ; println(a); a}
}

/**
 * @see https://medium.com/@anicolaspp/operator-in-scala-cbca7b939fc0#.c29s6e8w3
 */
object Pipe {
  def apply[ A ]( v: A ) = new Pipe( v )
  
  def map[A, B](f: A => B)(items: Seq[A]): Seq[B] = { items.map(f) }
  def flatMap[A, B](f: A => GenTraversableOnce[B])(items: Seq[A]): Seq[B] = items.flatMap(f)
  def filter[A](f: A => Boolean)(items: Seq[A]): Seq[A] = items.filter(f)
}

/**
 * https://medium.com/@anicolaspp/operator-in-scala-cbca7b939fc0#.c29s6e8w3
 */
object PipeOps {
  implicit def toPipe[ A ]( a: A ) = Pipe( a )
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
 *   sbt "run-main pt.inescn.scratchpad.ExperimentBuilder"
 */
object ExperimentBuilder {
   
  def modN(n: Int)(x: Int) = ((x % n) == 0)
  val t =  modN(2) _
  def s(x:Int) : Boolean =  modN(2) (x)
  def u = (x:Int) =>  modN(2) (x)
  
  def f = Pipe.filter { x: Int => x % 2 == 0 } _
  def g = (y:Seq[Int]) => Pipe.filter { x: Int => x % 2 == 0 } (y)

  def main( args: Array[ String ] ) {
    
    import PipeOps._
    
     val r1 = 5 |> (x => x + 1)
     println(r1)
     
     // TODO: no pipe conversion here. 
      val r2 = List(1,2,3,4,5) map (x => x + 1)
      println(r2)
      List(6,7,8,9) map (x => x + 1) $$ (println _)
      
      val r3 = List(1,2,3,4,5) filter (x => x % 2 == 0)
      println(r3)
      val r3_ = List(1,2,3,4,5) filter { x : Int => x % 2 == 0 }  #! ("r3 = ") //doesn't work, prints a lambda ID
      println(r3_)
      
      val r4 = List(1,2,3,4,5) |> f $$ (println)
      println(r4)
      
      val r5 = List(1,2,3,4,5) |> g #! ("r5 = ") //doesn't work, prints a lambda ID
      println(r5)
      
      val e1 = List(1,2,3,4).flatMap { x => if ((x % 2) == 0) Some(x) else None }
      println(e1)
      
  }
}

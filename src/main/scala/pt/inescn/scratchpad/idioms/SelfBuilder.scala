package pt.inescn.scratchpad.idioms

/**
 * Shows how we can use an inner polymorphic higher-kinded type to create a
 * concrete type of its self. We can do this in a type-safe way without recourse to
 * the use of reflection.  
 * 
 * @see  http://stackoverflow.com/questions/14729996/scala-implementing-method-with-return-type-of-concrete-instance
 * @see  http://stackoverflow.com/questions/4313139/how-to-use-scalas-this-typing-abstract-types-etc-to-implement-a-self-type
 * 
 */

//abstract class  Inner[T] {
trait Inner[T] {
  type Self <: Inner[T]
  
  def apply(t:T) : Self
  def extract : T
}

class InnerA[T](v: T) extends Inner[T] {
  type Self = InnerA[T]
  def apply(t:T) : Self = new InnerA[T](t)
  def extract = v
}

class InnerB[T](v: T) extends Inner[T]{
  type Self = InnerB[T]
  def apply(t:T) : Self = new InnerB[T](t)
  def extract = v
}

class InnerC[T](v: T) extends Inner[T]{
  type Self = InnerB[T]                                           // IMPORTANT: BUG, we don't get the correct concrete type
  def apply(t:T) : Self = new InnerB[T](t)
  def extract = v
}

import scala.language.higherKinds
import pt.inescn.scratchpad.StreamBuilder._

/**
 * This is a container class higher-kind. It allows us to "dynamically" construct new instances of P[T]
 * In order to ensure the return of concrete types, we use the declaration of type `Self`. Note that
 * this solution is not robust.  We may inadvertently return the wrong inner types.  See BUG above.
 * 
 * IMPORTANT: the `def make(t: T) : P[T]` will fail. The compiler creates an existential type. We
 * can leave this so but the IDE will not show the expanded type. See main's examples. 
 */
class Outer[T, P[T] <: Inner[T]](val u: P[T]) {
//class Outer[T, P[T]](u: P[T]) {
  def convertTo[Q[S]<: Inner[S],S](v: Q[S]) : Outer[S, Q] = new Outer(v)
  def extract : P[T] = u 
  def make(t: T)  = u(t) // : P[T] type mismatch; found : Outer.this.u.Self required: P[T]
  /* // In the case we do not use the constraint P[T] <: Inner[T]
     def duplicate(v:T) : P[T] = u(v) // This is not possible because u has type P[T], 
                                                     // its not a class so we do not know what constructors are available*/
  
  type Gen[U] = (T, T, U) => Stream[ T ]
  
  // type mismatch; found : scala.collection.immutable.Stream[Outer.this.u.Self] required: Stream[P[T]]
  //def toStream[U](to: T, by: U, g: Gen[U])  : Stream[P[T]]= {
  // Ok
  //def toStream[U](to: T, by: U, g: Gen[U]) = {
  // Ok 
  def toStream[U](to: T, by: U, g: Gen[U]) : Stream[P[T]#Self] = {
    val st = g(u.extract, to, by)
    val r = st.map{ x => u( x ) }
    r
  }
}


/**
 *  sbt "run-main pt.inescn.scratchpad.idioms.SelfBuilder"
 */
object SelfBuilder {

  def main( args: Array[ String ] ) {
    
    def linSearch(from: Double, to: Double, by: Double) : Stream[ Double ] = {
      val len = ( ( to - from ) / by).ceil.toInt
      linspace( from, by ).take(  len + 1)
    }

    def iLinSearch(from: Int, to: Int, by: Int) : Stream[ Int ] = {
      val len = ( ( to - from ) / by).ceil.toInt
      linspace( from, by ).take(  len + 1).map(_.toInt)
    }
    
    // Pack the inner type into the outer type
    val u = new InnerA(100)                                       // InnerA[Int]
    val u1 = new InnerA("100")                                  // InnerA[String]
    // We can use any inner type we want
    val u2 = new Outer(u)                                          // Outer[Int, Inner]
    val u3 = u2.convertTo(u1)                                     // Outer[String, Inner]
    val u4 = u3.extract                                              //  InnerA[String]
    // We can also convert the inner types
    val u5 = new InnerB("100")                                   // InnerB[String]
    val u6 = u3.convertTo(u5)                                     // Outer[String, InnerB]
    val u7 = u6.extract                                               //  InnerB[String]
    // We can also create new instances of the inner type 
    val u8 :  InnerB[String] = u6.make("101")              //  InnerB[String]
    val u9 = new InnerC(101)                                       // InnerC[Int]
    val u10 = new Outer(u9)                                        // Outer[Int, InnerC]
    // Careful, we might have an unexpected type
    val u11 : InnerB[Int] = u9(102)    
    // We can check for the correct type
    //val u11 : InnerC[Int] = u9(102)                          // type mismatch; found : pt.inescn.scratchpad.SParameterTest.u9.Self (which expands to) 
                                                                                // pt.inescn.scratchpad.InnerB[Int] required: pt.inescn.scratchpad.InnerC[Int]
    //val u12 :  InnerC[Int] = u10.make(1001)             // But we have to expand the type explicitly, same compile erro as above
    // Important: the types in the IDE are not expanded so we cannot see them 
    val u13 = u9(102)                                                  //  Important: we get type `u9.Self`
    
    val u14 = u2.toStream(100, 1,iLinSearch)
    val u15 : Stream[_] = u2.toStream(100, 1,iLinSearch)
    
  }
  
}
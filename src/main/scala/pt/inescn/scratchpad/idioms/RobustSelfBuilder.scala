package pt.inescn.scratchpad.idioms

/**
 * Shows how we can use an inner polymorphic higher-kinded type to create a
 * concrete type of its self. We can do this in a type-safe way without recourse to
 * the use of reflection.  Unlike the SelfBuilder pattern, we enforce the concrete type 
 * and check that it conforms to the correct class. To do this we use an additional
 * inheritance layer with `StrictSelf`. See comments on compilation errors One issue
 * we have is that the IDE does not expand the self types. . 
 * 
 * @see  http://stackoverflow.com/questions/14729996/scala-implementing-method-with-return-type-of-concrete-instance
 * @see  http://stackoverflow.com/questions/4313139/how-to-use-scalas-this-typing-abstract-types-etc-to-implement-a-self-type
 * 
 * TODO: (parts 2,3 and 4)
 * @see http://jim-mcbeath.blogspot.pt/2009/09/type-safe-builder-in-scala-using-church.html
 * @see http://jim-mcbeath.blogspot.pt/2009/09/type-safe-builder-in-scala-part-2.html
 * @see http://jim-mcbeath.blogspot.pt/2009/09/type-safe-builder-in-scala-part-3.html
 * @see http://jim-mcbeath.blogspot.pt/2009/09/type-safe-builder-in-scala-part-4.html
 */

trait StrictSelf[T <: StrictSelf[T]] { self: T =>
  type Self >: self.type <: T
  //def apply() : Self
}

abstract class XInner[T] { self:StrictSelf[_] =>
  def apply(t:T):Self
  def extract : T
}

class B[T](v: T) extends XInner[T] with StrictSelf[B[T]] {
  type Self = B[T]
  def apply(t:T) = new B(t)
  def extract = v
}

class C[T](v: T) extends XInner[T] with StrictSelf[C[T]] {
  type Self = C[T]   // NOTE: if we err here, we can still return an unexpected type
  def apply(t:T) = new C(t)
  def extract = v
}

/**
 * Compilation errors:
 * 
 * - If we change the StrictSelf[D[T]] to StrictSelf[C[T]]:
 *    illegal inheritance;
 *    [error]  self-type pt.inescn.scratchpad.idioms.D[T] does not conform to pt.inescn.scratchpad.idioms.StrictSelf[pt.inescn.scratchpad.idioms.C[T]]'s selftype pt.inescn.scratchpad.idioms.C[T]
 *    [error] class D[T](v: T) extends XInner[T] with StrictSelf[C[T]] {
 *                                                                              ^
 *    
 *  - If we change type Self = D[T]  to type Self = C[T]:
 *    overriding type Self in trait StrictSelf with bounds >: D.this.type <: pt.inescn.scratchpad.idioms.D[T];
 *       [error]  type Self has incompatible type
 *       [error]   type Self = C[T]   // NOTE: if we err here, we can still return an unexpected type
 *       [error]           ^
 * 
 */
class D[T](v: T) extends XInner[T] with StrictSelf[D[T]] {
  type Self = D[T]   // NOTE: if we err here, we can still return an unexpected type
  def apply(t:T) = new D(t)
  def extract = v
}

import scala.language.higherKinds
import pt.inescn.scratchpad.StreamBuilder._

/**
 * This is a container class higher-kind. It allows us to "dynamically" construct new instances of P[T]
 * In order to ensure the return of concrete types, we use the declaration of type `Self`. Note that
 * this solution is not robust.  We may inadvertently return the wrong inner types.  See BUG above.
 * 
 * IMPORTANT: the `def make(t: T) : P[T]` will fail. This is not possible because u has type P[T],  
 * its not a class so we do not know what constructors are available. So we use the constraint 
 * P[T] <: Inner[T] to provide that information. For this same function he compiler creates an existential 
 * type if we do not indicate the type explicitly. We can leave this so but the IDE will not show the expanded type. 
 * See main's examples. However we can use the type alias `Self`to ensure the type is visible. Same for the case of 
 * `toStream`
 */
class ROuter[T, P[T] <: XInner[T] with StrictSelf[_]](val u: P[T]) {
//class Outer[T, P[T]](u: P[T]) {
  def convertTo[Q[S]<: XInner[S] with StrictSelf[_],S](v: Q[S]) : ROuter[S, Q] = new ROuter(v)
  def extract : P[T] = u 
  // Note: we could either use the type Self as is or leave the compiler to use existential typing
  def make(t: T) : P[T]#Self = u(t) // : P[T] type mismatch; found : ROuter.this.u.Self required: P[T]
  
  type Gen[U] = (T, T, U) => Stream[ T ]
  
  // type mismatch; found : scala.collection.immutable.Stream[Outer.this.u.Self] required: Stream[P[T]]
  //def toStream[U](to: T, by: U, g: Gen[U])  : Stream[P[T]]= {
  // Ok
  //def toStream[U](to: T, by: U, g: Gen[U]) = {
  // Fails !!
  //def toStream[U](to: T, by: U, g: Gen[U]) : Stream[P[T]#Self] = {
  def toStream[U](to: T, by: U, g: Gen[U]) : Stream[P[T]#Self] = {
    val st = g(u.extract, to, by)
    val r = st.map{ x => u( x ) }
    r
  }
}

/**
 *  sbt "run-main pt.inescn.scratchpad.idioms.RobustSelfBuilder"
 */
object RobustSelfBuilder {

  def main( args: Array[ String ] ) {
    
    def linSearch(from: Double, to: Double, by: Double) : Stream[ Double ] = {
      val len = ( ( to - from ) / by).ceil.toInt
      linspace( from, by ).take(  len + 1)
    }

    def iLinSearch(from: Int, to: Int, by: Int) : Stream[ Int ] = {
      val len = ( ( to - from ) / by).ceil.toInt
      linspace( from, by ).take(  len + 1).map(_.toInt)
    }
    
  
    val v1 = new B(200)                                             // B[Int]
    val v2 = new B("200")                                         // B[Int]
    val v3 = new C(200)                                            // B[Int]
    val v4 = new C("200")                                         // C[String]
    val v5 = new D("202")                                         // D[String]
    
    // We can use any inner type we want
    val v6 = new ROuter(v1)                                       // ROuter[Int, B]
    val v7 = v6.convertTo(v2)                                     // ROuter[String, B]
    val v8 = v6.extract                                               //  B[Int]
    // We can also convert the inner types
    val v9 = v7.convertTo(v4)                                     // ROuter[String, C]
    val v10 = v9.extract                                               //  InnerB[String]
    // We can also create new instances of the inner type 
    //val v11 : C[String] = v9.make("101")                       //  v9u.Self = C[String], we cannot see the type in the IDE
    val v11  = v9.make("101")                       //  v9u.Self = C[String], we cannot see the type in the IDE
    val v12 = v5("2002")                                             // D[String]
    
      
    val u14 = v6.toStream(200, 1,iLinSearch)
    val u15 : Stream[_] = v6.toStream(100, 1,iLinSearch)    
}
  
}
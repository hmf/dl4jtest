package pt.inescn.scratchpad

import StreamBuilder._

// Parameters are value classes

/**
 * This file holds the class that describe a Parameter and a Parameter Range. The code uses
 * several standard type constraints and implicit constraints. We want to ensure that:
 * 1. The `ParameterRange` uses the same `Parameter` for its `begin` and `end` of range.
 * 2. Make sure that when we do create a `Parameter` of a given type, only that type will be returned
 * 3. Make sure that any type you do create will always require the explicit use of the polymorphic type `T`
 *    This has an important advantages. It allows the IDE to show the type in an expanded form. If
 *    this were not so, a `ParameterRange` containing generic `Parameter`s could be created .
 *
 * (1) The `ParameterRange` constructor has the additional parameter  (implicit ev1: P[T] =:= Q[T]).
 *       This ensures range limit compatibility.
 *
 * (2) See RobustSelfBuilder which ensure that any `Paramater`a client user defines will be of that type.
 *       We enforce that a Parameter be also a`StrictSelf[ T ]`.  This type uses a lower and upper bounds
 *       to ensure the type is exactly of the extending class (see the `Parameter` self).
 *
 *  (3) We add the restriction  `P[ T ] <: Parameter[ T ] with StrictSelf[_]` to the `ParameterRange`.
 *       This ensures that a Parameter must also have polymorphic StrictSelf[_]. So if a generic `Parameter`
 *       is inferred, this constraint will cause a compilation failure.
 *       Note that the use of  (implicit ev1: P[T] =:= Q[T], ev2: P[T] <:< Parameter[ T ]) will not ensure this
 *       constraint.
 *
 * @see http://blog.bruchez.name/2015/11/generalized-type-constraints-in-scala.html
 * @see http://stackoverflow.com/questions/19642053/when-to-use-val-or-def-in-scala-traits
 */

import scala.{ Option, Some, None }
import scala.language.higherKinds

trait StrictSelf[ T <: StrictSelf[ T ] ] { self: T =>
  type Self >: self.type <: T
}

trait Parameter[ T ] { self: StrictSelf[ _ ] =>
  type Self <: Parameter[ T ]

  def apply( v: T ): Self
  def value: T // must be a def or var so that we can override and initialize from the extended class
}

// Note <: and >: for polymorphic types
// =:= and <:< are types and used in implicits
//class ParameterRange[ P[ T ] <: Parameter[ T ] with StrictSelf[ _ ], Q[ T ] <: Parameter[ T ], T, U ](
class ParameterRange[ P[ T ] <: Parameter[ T ], Q[ T ] <: Parameter[ T ], T, U ](
    val begin: P[ T ],
    val end: Q[ T ],
    val config: U,
    //val generator: ( T, T, U ) => Stream[ T ] )  (implicit ev1: P[T] =:= Q[T], ev2: P[T] <:< Parameter[ T ]) {
    val generator: ( T, T, U ) => Stream[ T ] )( implicit ev1: P[ T ] =:= Q[ T ] ) {

  type Gen = ( T, T, U ) => Stream[ T ]

  //def toStream (implicit ev: P[T] =:= Q[T]): Stream[ P[ T ]#Self ] = {
  def toStream: Stream[ P[ T ]#Self ] = {
    val st = generator( begin.value, end.value, config )
    val o = begin
    val r = st.map{ x => o( x ) }
    r
  }
}

/**
 * sbt "run-main pt.inescn.scratchpad.ParameterExample"
 */
object ParameterExample {

  def linSearch( from: Double, to: Double, by: Double ): Stream[ Double ] = {
    val len = ( ( to - from ) / by ).ceil.toInt
    linspace( from, by ).take( len + 1 )
  }

  def linSearchI( from: Int, to: Int, by: Int ): Stream[ Int ] = {
    val len = ( ( to - from ) / by ).ceil.toInt
    linspace( from, by ).take( len + 1 ).map( x => x.toInt )
  }

  /**
   *  An example of a parameter.
   */
  case class ParamOne[ T ]( val value: T = 0.0 ) extends Parameter[ T ] with StrictSelf[ ParamOne[ T ] ] {
    type Self = ParamOne[ T ]

    def apply( v: T ): Self = new ParamOne( v )
  }

  /**
   * Should not define these classes without the polymorphic parameter. If we do
   * then the types will only show the generic Parameter. See examples below.
   */
  case class XSlearningRate( val value: Double = 0.0 ) extends Parameter[ Double ] with StrictSelf[ XSlearningRate ] {
    type Self = XSlearningRate

    def apply( v: Double ): Self = new XSlearningRate( v )
  }

  case class ParamTwo[ T ]( val value: T = 0.0 ) extends Parameter[ T ] with StrictSelf[ ParamTwo[ T ] ] {
    type Self = ParamTwo[ T ]

    def apply( v: T ): Self = new ParamTwo( v )
  }

  /**
   * Example of enforcing the type. Removed StrictSelf[_] in ParameterRange[ P[ T ] <: Parameter[ T ] with StrictSelf[_], ...)
   * This lets us check for the correct types in a given Parameter.
   */
  case class ParamThree( val value: Int = 0 ) extends Parameter[ Int ] with StrictSelf[ ParamThree ] {
    type Self = ParamThree

    def apply( v: Int ): Self = new ParamThree( v )
  }

  def main( args: Array[ String ] ) {

    val t0 = new ParameterRange( ParamOne( 0.0 ), ParamOne( 1.0 ), 0.1, linSearch )
    val t1 = t0.toStream
    t1.foreach { x => println( x ) }

    //val p = ParamThree( 0.0 ) // compilation fails, must be integer
    val x0 = new ParameterRange( ParamThree( 0 ), ParamThree( 10 ), 1, linSearchI ) // compilation fails as expected, must all be integers 
    val x1 = x0.toStream
    x1.foreach { x => println( x ) }

    /*val x0 = new ParameterRange( ParamOne( 0.0 ), ParamOne( 1 ), 0.1, linSearch ) // compilation fails as expected 
    val x1 = x0.toStream
    x1.foreach { x => println( x ) }*/

    // NOT WORKING. Prefer having safe contained type
    // This will return a generic Parameter if we do not use the ParameterRange[ P[ T ] <: Parameter[ T ] with StrictSelf[_], ...) constraint
    /*val s0  = new ParameterRange( XSlearningRate( 0.0 ), XSlearningRate( 1.0 ), 0.2, linSearch ) // ParameterRange[Parameter, Parameter, Double, Double]
    val s1 = s0.toStream
    s1.foreach { x => println( x ) }*/

    /*val u0 = new ParameterRange( XSlearningRate( 0.0 ), ParamOne( 1.0 ), 0.2, linSearch ) // compilation fails as expected 
    val u1 = u0.toStream
    u1.foreach { x => println( x ) }*/

    /*val v0 = new ParameterRange( ParamOne( 0.0 ), XSlearningRate( 1.0 ), 0.2, linSearch ) // compilation fails as expected 
    val v1 = v0.toStream
    v1.foreach { x => println( x ) }*/

    /*val w0  = new ParameterRange( ParamOne( 0.0 ), ParamTwo( 1.0 ), 0.2, linSearch ) // compilation fails as expected 
    val w1 = w0.toStream
    w1.foreach { x => println( x ) }*/
  }

}
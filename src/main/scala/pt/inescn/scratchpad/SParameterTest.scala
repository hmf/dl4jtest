package pt.inescn.scratchpad


import StreamBuilder._

// Parameters are value classes

/**
 *
 * @see http://stackoverflow.com/questions/19642053/when-to-use-val-or-def-in-scala-traits
 */
trait SParameter[ T ] {
  def value: T // must be a def or var so that we can override and initialize from the extended class
  
  def apply( v: T ): SParameter[ T ]
}

import scala.{ Option, Some, None }
import scala.language.higherKinds

trait SParameterRange[ P[T] <: SParameter[ T], T, U, B, E, C ] {
  
  type Gen = (T, T, U) => Stream[ P[T] ]
  
  val begin:        Option[P[T]]
  val end:           Option[P[T]]
  val config:       Option[ U ]
  val generator : Option[  Gen ]

  def apply( begin: P[T]): SParameterRange[ P, T, U, TBEGIN, TMISSING, TMISSING]

  def from[Q[S] <: SParameter[S],S]( nbegin: Q[S] ): SParameterRange[ Q, S, U, TBEGIN, E, C ]
  def to( nend: P[T] ): SParameterRange[ P, T, U, B, TEND, C ]
  def by( nconfig: U ): SParameterRange[ P, T, U, B, E, TCONFIG ]
  def using (gen: Gen) : SParameterRange[ P, T, U, B, E, TCONFIG ]
  
  // TODO: remove and hide
   def toStream: Stream[ P[T] ]
}

// TODO: remove extends and use self
trait SStreamableParameterRange[ P[T] <: SParameter[T], T, U, B, E, C ] extends SParameterRange[ P, T, U, B, E, C ]  {
  // TODO; this : SParameter[ T ] => 
  def toStream: Stream[ P[T] ]
}

// Can also use `abstract class`
trait SBEGIN
trait SEND
trait SCONFIG
trait SMISSING

object SParameterRange {

  type SPDouble = SParameter[ Double ]
  type SGen = (SParameterRange.SPDouble, SParameterRange.SPDouble, Option[Double]) => Stream[SParameterRange.SPDouble]
}

private class SLinearParameterRange[ P[T] <: SParameter[T], T, B, E, C ]( 
                                                                          val begin:       Option[P[T]], 
                                                                          val end:          Option[P[T]], 
                                                                          val config:      Option[ Double ],
                                                                          val generator: Option[ (T,T,Double) => Stream[P[T]]] )
    extends SParameterRange[ P, T, Double, B, E, C ] {
  
  def this( ){
    this( None, None, None, None )
  }
  
  def this( nbegin: P[T] ){
    this( Some(nbegin), None, None, None )
  }
  
  def apply( nbegin: P[T]): SParameterRange[ P, T, Double, TBEGIN, TMISSING, TMISSING] = 
    new SLinearParameterRange[ P, T, TBEGIN, TMISSING, TMISSING ]( Some(nbegin), None, None, None )
  
  def from[Q[S] <: SParameter[ S],S]( nbegin: Q[S] ): SParameterRange[ Q, S, Double, TBEGIN, E, C ] 
    = new SLinearParameterRange[ Q, S, TBEGIN, E, C ]( Some(nbegin), None, config, None)
    
  def to( nend: P[T] ): SParameterRange[ P, T, Double, B, TEND, C ] 
    = new SLinearParameterRange[ P,T,  B, TEND, C ]( begin, Some(nend), config, generator )

  def by( nconfig: Double ): SParameterRange[ P, T, Double, B, E, TCONFIG ] 
    = new SLinearParameterRange[ P, T, B, E, TCONFIG ]( begin, end, Some( nconfig ), generator )
  
  def using( gen : (T, T, Double) => Stream[ P[T] ] ): SParameterRange[ P, T, Double, B, E, TCONFIG ] =  {
    new SLinearParameterRange[ P, T, B, E, TCONFIG ]( begin, end, config, Some(gen) )
  }

  // a to b with length = l
  // from a  by delta (no b!!)
  // a to b by delta ?

 /*
  // TODO: make parameterizable by StreamBuilder
  def toStream: Stream[ P ] = {
    val len = ( ( end.get.value - begin.get.value  ) / config.get ).ceil.toInt
    linspace( begin.get.value, end.get.value ).map{ x => begin.get.apply( x ) }.take(  len )
  }
 */
  def toStream: Stream[ P[T] ] = { 
    val f = generator.get
    f(begin.get.value, end.get.value, config.get)
  }
  
}


case class SlearningRate( override val value: Double = 0.0) extends SParameter[ Double ] { 
  def apply( v: Double ) = new SlearningRate( v ) 
  }


/**
 * A basic container type that can hold any primitive or complex type
 */
case class AP[T](val v: T) {
  type I = T
}

import scala.language.higherKinds

//class AS[P <: AP[_],T <: P#I](r : AP[T]) {
//class AS[P <: AP[_],T <: P#I](r : AP[P#I]) {
//class AS[P <: AP[_],T <: P#I](r : P) {
//class AS[P <: AP[T],T <: P#I](r : P) {
/**
 * Use a higher-kinded type `P[T]` that allows us to extract the contained
 * type `T` from the container `AP[T]`. In this way we can declare new types 
 * based on the contained type `T`. We can also use a contained type `U` that
 * also enables run-time type conversion (checked at compilation-time of course). 
 * 
 * NOTE: this version uses unsafe `var` for the generator - a call to `map`.
 * may result in a null pointer exception.  Consider using the Safe Builder pattern. 
 */
class AS[P[T] <: AP[T],T,U](r : P[T], var generator : T => U) {
  
  type Gen = T => U
  
  def this(r : P[T]) {
    this(r, null) // Unsafe use of Null
  }
  
  def make[Q[S] <: AP[S],S](nv:  Q[S] ) : AS[Q,S,U] = new AS[Q,S,U](nv) 
  def to[V](g: T => V) = { val t = new AS[P,T,V](r) ; t.generator = g; t }
  def from : T           = r.v
  def map : U            = generator( from )
}

/**
 * Same as the `AS` type above but uses a safer Option types. This avoids
 * the null pointer exception however  an exception will still be possible
 * when we use the `Option.get` methods when `Nothing`is set.  
 */
class AR[P[T] <: AP[T],T,U](val r : Option[P[T]], val generator : Option[T => U]) {
  
  /* Possible
  def this() {
    this(None, None)
  }*/
  
  def this(nr : Option[P[T]]) {
    this(nr, None)
  }
  
  def make[Q[S] <: AP[S],S](nv:  Q[S] ) : AR[Q,S,U] = new AR[Q,S,U]( Some(nv)) 
  def to[V](g: T => V) = { new AR[P,T,V](r, Some(g))  }
  def from : T           = r.get.v
  def map : U            = generator.get( from )
}


object SParameterTest {
    
  /*
    def linSearch(from: SlearningRate, to: SlearningRate, by: Double) : Stream[ SlearningRate ] = {
      val len = ( ( to.value - from.value ) / by).ceil.toInt
      linspace( from.value, by ).map{ x => from.apply( x ) }.take(  len + 1)
    }*/
   // TODO: can we get rid of SlearningRate ?
    def linSearch(from: Double, to: Double, by: Double) : Stream[ SlearningRate ] = {
      val len = ( ( to - from ) / by).ceil.toInt
      linspace( from, by ).map{ x => SlearningRate.apply( x ) }.take(  len + 1)
    }
    
// TODO: Add phantom type G for the generator
// TODO: How to make generators independent from type P 

  def main( args: Array[ String ] ) {
    
    val t0 = SlearningRate()
    val t  = new SLinearParameterRange( t0 ) // TODO: Hide constructor and use companion object with apply
    // TODO: how can we get the specific Paramater[T] and not the base trait?  
    val t1 = t from SlearningRate( 0.0 )
    val t2 = t1 using( linSearch )
    val t3 = t2 to SlearningRate( 1.0 )
    val t4 = t3 by 0.1
    val t5 = t4.toStream
    t5.foreach { x => println( x ) }
    
    val s = new SLinearParameterRange
    val s1 = s from t0
    val s2 = s1 to SlearningRate( 1.0 )
    val s3 = s2 by 0.1
    val s4 = s3 using( linSearch )
    val s5 = s4.toStream
    t5.foreach { x => println( x ) }

    val ai = new AP(100)
    val as = new AP("1000")
    
    
    // Example of morphing higher-kined types in order to set and extract basic inner types
    // We set the contained type with an Int, so the higher type holds an Int
    val w = new AS(ai)                                              // AS[AP, Int, Nothing]
    val a = w.from                                                    // Int
    println(s"AP[Int] = $a")
    // We change the contained type to a String, so the higher type holds a String
    val w1 = w make as                                             // AS[AP, String, Nothing]
    val a1 = w1.from                                                 // String
    println(s"AP[String] = $a1")
    // We set the mapping contained type to a String
    val w2 = w1 to { x => x + "!"}                                // AS[AP, String, String]
    val a2 = w2.map                                                  // Changed string
    println(s"Mapped AP[String to String] = $a2")
    // We set the mapping contained type to an Int
    val w3 = w2 to { x => x.toInt * 10 }                     // AS[AP; String, Int]
    val a3 = w3.map
    println(s"Mapped AP[String to Int] = $a3")

    // Same as above but using the `Option` instead of a `var`
    // We set the contained type with an Int, so the higher type holds an Int
    val v = new AR(Some(ai))                                    // AR[AP, Int, Nothing]
    val b = v.from                                                    // Int
    println(s"AR[Int] = $b")
    // We change the contained type to a String, so the higher type holds a String
    val v1 = v make as                                              // AR[AP, String, Nothing]
    val b1 = v1.from                                                 // String
    println(s"AR[String] = $b1")
    // We set the mapping contained type to a String
    val v2 = v1 to { x => x + "!"}                                // AR[AP, String, String]
    val b2 = v2.map                                                  // Changed string
    println(s"Mapped AR[String to String] = $a2")
    // We set the mapping contained type to an Int
    val v3 = v2 to { x => x.toInt * 10 }                     // AR[AP; String, Int]
    val b3 = v3.map
    println(s"Mapped AP[String to Int] = $a3")

    
    //val w2 = t.from(SlearningRate( 0.0 )).to(SlearningRate( 0.01 )).by(0.001 )
    //val w3 = w2 build()
    //val w4 = w3.toStream
    //val w4 = w2.toStream
    //w4.foreach { x => println( x ) }
  }
 
  
}
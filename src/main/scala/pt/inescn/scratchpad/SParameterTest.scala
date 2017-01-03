package pt.inescn.scratchpad


import StreamBuilder._

// Parameters are value classes

/**
 *
 * @see http://stackoverflow.com/questions/19642053/when-to-use-val-or-def-in-scala-traits
 */
trait SParameter[ T ] {
  type Self <: SParameter[T]
  
  def apply( v: T ): Self
  def value: T // must be a def or var so that we can override and initialize from the extended class
}

import scala.{ Option, Some, None }
import scala.language.higherKinds

trait SParameterRange[ P[T] <: SParameter[ T], T, U, B, E, C, G ] {
  
  type Gen = (T, T, U) => Stream[ T ]
  
  val begin:        Option[P[T]]
  val end:           Option[P[T]]
  val config:       Option[ U ]
  val generator : Option[  Gen ]

  //def apply( begin: P[T]): SParameterRange[ P, T, U, TBEGIN, TMISSING, TMISSING, TMISSING]
  def makeStreamable: SStreamableParameterRange[ P, T, U, B, E, C, G]

  def from[Q[S] <: SParameter[S],S]( nbegin: Q[S] ): SParameterRange[ Q, S, U, TBEGIN, E, C, G ]
  def to( nend: P[T] ): SParameterRange[ P, T, U, B, TEND, C, G ]
  def by( nconfig: U ): SParameterRange[ P, T, U, B, E, TCONFIG, G ]
  def using (gen: Gen) : SParameterRange[ P, T, U, B, E, C, TGEN ]
  
  // TODO: remove and hide
   //def toStream
  // ?? def toStream: Stream[P[T]#Self]
   //def toStream: Stream[ _ ]
   //def toStream: Stream[ SParameter[T] ]
   //def toStream: Stream[ P[T] ]
}

// TODO: remove extends and use self
trait SStreamableParameterRange[ P[T] <: SParameter[T], T, U, B, E, C, G ] extends SParameterRange[ P, T, U, B, E, C, G ]  {
  // TODO; this : SParameter[ T ] =>
  def toStream : Stream[P[T]#Self] 
  //def toStream: Stream[ SParameter[T] ]
  //def toStream: Stream[ P[T] ]
}

trait SSafeBuild[ P[T] <: SParameter[T], T] {
  def build(): SStreamableParameterRange[ P, T, Double, TBEGIN, TEND, TCONFIG, TGEN ]
}

// Can also use `abstract class`
/* TOOD;: remove
 trait SBEGIN
trait SEND
trait SCONFIG
trait SGEN
trait SMISSING
*/
import scala.language.implicitConversions

object SParameterRange {

  implicit def enableBuild[P[T] <: pt.inescn.scratchpad.SParameter[T],T,U]( range: SParameterRange[ P, T, Double, TBEGIN, TEND, TCONFIG, TGEN ] ) = 
    new SSafeBuild[P, T] {
      def build() : SStreamableParameterRange[ P, T, Double, TBEGIN, TEND, TCONFIG, TGEN ] = 
      /*new SLinearParameterRange[ P, T, TBEGIN, TEND, TCONFIG, TGEN ]( range.begin, range.end, range.config, range.generator ) 
             with SStreamableParameterRange[ P, T, Double, TBEGIN, TEND, TCONFIG, TGEN ]*/
      range.makeStreamable
      
  }
  
}

private class SLinearParameterRange[ P[T] <: SParameter[T], T, B, E, C, G ]( 
                                                                          val begin:       Option[P[T]], 
                                                                          val end:          Option[P[T]], 
                                                                          val config:      Option[ Double ],
                                                                          val generator: Option[ (T,T,Double) => Stream[T]] )
    extends SParameterRange[ P, T, Double, B, E, C, G ] {
  
  def this( ){
    this( None, None, None, None )
  }
  
  def this( nbegin: P[T] ){
    this( Some(nbegin), None, None, None )
  }
  
  def makeStreamable: SStreamableParameterRange[ P, T, Double, B, E, C, G] = 
    new SLinearParameterRange[ P, T, B, E, C, G ]( begin, end, config, generator) with SStreamableParameterRange[ P, T, Double, B, E, C, G]
  
  def from[Q[S] <: SParameter[ S],S]( nbegin: Q[S] ): SParameterRange[ Q, S, Double, TBEGIN, E, C, G ] 
    = new SLinearParameterRange[ Q, S, TBEGIN, E, C, G ]( Some(nbegin), None, config, None)
    
  def to( nend: P[T] ): SParameterRange[ P, T, Double, B, TEND, C, G ] 
    = new SLinearParameterRange[ P,T,  B, TEND, C, G ]( begin, Some(nend), config, generator )

  def by( nconfig: Double ): SParameterRange[ P, T, Double, B, E, TCONFIG, G ] 
    = new SLinearParameterRange[ P, T, B, E, TCONFIG, G ]( begin, end, Some( nconfig ), generator )
  
  def using( gen : (T, T, Double) => Stream[ T ] ): SParameterRange[ P, T, Double, B, E, C, TGEN ] =  {
    new SLinearParameterRange[ P, T, B, E, C, TGEN]( begin, end, config, Some(gen) )
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
  // TODO: how do we return a P[T]
  def toStream : Stream[P[T]#Self] = { 
  //def toStream: Stream[ _ ] = { 
  //def toStream: Stream[ SParameter[T] ] = { 
  //def toStream: Stream[ P[T] ] = { 
    val f = generator.get
    val st = f(begin.get.value, end.get.value, config.get)
    val o = begin.get
    //val r = st.map{ x => begin.get.apply( x ) }
    val r = st.map{ x => o( x ) }
    r
  }
  
}

/** 
 *  An example of a parameter. 
 */
case class SlearningRate[T]( val value: T = 0.0) extends SParameter[ T ] { 
  type Self = SlearningRate[T]

  def apply( v: T ) : Self = new SlearningRate( v ) 
  }

/**
 * Should not define these classes without the polymorphic parameter. If we do
 * then the types will only show the generic SParameter. See examples below. 
 */
case class XSlearningRate( val value: Double = 0.0) extends SParameter[ Double ] { 
  type Self = XSlearningRate

  def apply( v: Double ) : Self = new XSlearningRate( v ) 
  }

object SParameterTest {
  
  /*
    def linSearch(from: SlearningRate, to: SlearningRate, by: Double) : Stream[ SlearningRate ] = {
      val len = ( ( to.value - from.value ) / by).ceil.toInt
      linspace( from.value, by ).map{ x => from.apply( x ) }.take(  len + 1)
    }*/
  /*
   // TODO: can we get rid of SlearningRate ?
    def linSearch(from: Double, to: Double, by: Double) : Stream[ SlearningRate ] = {
      val len = ( ( to - from ) / by).ceil.toInt
      linspace( from, by ).map{ x => SlearningRate.apply( x ) }.take(  len + 1)
    }*/
    def linSearch(from: Double, to: Double, by: Double) : Stream[ Double ] = {
      val len = ( ( to - from ) / by).ceil.toInt
      linspace( from, by ).take(  len + 1)
    }
    
// TODO: Add phantom type G for the generator
// TODO: How to make generators independent from type P 

  def main( args: Array[ String ] ) {
    
    val t0 = SlearningRate()
    val t  = new SLinearParameterRange( t0 ) // TODO: Hide constructor and use companion object with apply
    //val x = t.toStream // TODO: compile should fail 
    val t1 = t from SlearningRate( 0.0 )
    val t2 = t1 using( linSearch )
    val t3 = t2 to SlearningRate( 1.0 )
    val t4 = t3 by 0.1
    //val t5 = t4.toStream // compilation fails
    val t5 = t4.build().toStream 
    t5.foreach { x => println( x ) }
    
    val s = new SLinearParameterRange
    val s1 = s from t0
    val s2 = s1 to SlearningRate( 1.0 )
    val s3 = s2 by 0.1
    val s4 = s3 using( linSearch )
    //val s5 = s4.toStream // compilation fails
    val s5 = t4.build().toStream 
    t5.foreach { x => println( x ) }

    val u0 = XSlearningRate()
    val u1  = new SLinearParameterRange( u0 ) // TODO: Hide constructor and use companion object with apply
    // TODO: should fail compilation
    val u2 = u1.from(XSlearningRate(1.0)).to(XSlearningRate(3)).by(1).using(linSearch).build().toStream
    u2.foreach { x => println( x ) }
    
    //val w2 = t.from(SlearningRate( 0.0 )).to(SlearningRate( 0.01 )).by(0.001 )
    //val w3 = w2 build()
    //val w4 = w3.toStream
    //val w4 = w2.toStream
    //w4.foreach { x => println( x ) }
  }
 
  
}
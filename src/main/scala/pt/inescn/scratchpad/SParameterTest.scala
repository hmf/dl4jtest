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

trait SParameterRange[ P <: SParameter[ _ ], U, B, E, C ] {
  
  type Gen = (P, P, U) => Stream[ P ]
  
  val begin: Option[P]
  val end: Option[P]
  val config: Option[ U ]
  val generator : Option[  Gen ]

  def apply( begin: P): SParameterRange[ P, U, TBEGIN, TMISSING, TMISSING]

  def from[Q<: SParameter[ _ ]]( nbegin: Q ): SParameterRange[ Q, U, TBEGIN, E, C ]
  def to( nend: P ): SParameterRange[ P, U, B, TEND, C ]
  def by( nconfig: U ): SParameterRange[ P, U, B, E, TCONFIG ]
  def using (gen: Gen) : SParameterRange[ P, U, B, E, TCONFIG ]
  
  // TODO: remove and hide
   def toStream: Stream[ P ]
}

trait SStreamableParameterRange[ P <: SParameter[ _ ], U, B, E, C ] extends SParameterRange[ P, U, B, E, C ]  {
  // TODO; this : SParameter[ T ] => 
  def toStream: Stream[ P ]
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

private class SLinearParameterRange[ P <: SParameter[ _ ], B, E, C ]( val begin: Option[P], 
                                                                          val end: Option[P], 
                                                                          val config: Option[ Double ],
                                                                          val generator: Option[ (P,P,Double) => Stream[P]] )
    extends SParameterRange[ P, Double, B, E, C ] {
  //var x: U = _

  //var  generator : (SParameterRange.SPDouble, SParameterRange.SPDouble, Option[Double]) => Stream[ SParameterRange.SPDouble ]
  
  
  def this( ){
    this( None, None, None, None )
  }
  
  def this( nbegin: P ){
    this( Some(nbegin), None, None, None )
  }
  
 def apply( nbegin: P): SParameterRange[ P, Double, TBEGIN, TMISSING, TMISSING] = 
   new SLinearParameterRange[ P, TBEGIN, TMISSING, TMISSING ]( Some(nbegin), None, None, None )
  
  def from[Q<: SParameter[ _ ]]( nbegin: Q ): SParameterRange[ Q, Double, TBEGIN, E, C ] = new SLinearParameterRange[ Q, TBEGIN, E, C ]( Some(nbegin), None, config, None)
  def to( nend: P ): SParameterRange[ P, Double, B, TEND, C ] =  new SLinearParameterRange[ P, B, TEND, C ]( begin, Some(nend), config, generator )
  def by( nconfig: Double ): SParameterRange[ P, Double, B, E, TCONFIG ] = new SLinearParameterRange[ P, B, E, TCONFIG ]( begin, end, Some( nconfig ), generator )
  def using( gen : (P, P, Double) => Stream[ P ] ): SParameterRange[ P, Double, B, E, TCONFIG ] =  {
 //   generator = gen
    new SLinearParameterRange[ P, B, E, TCONFIG ]( begin, end, config, Some(gen) )
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
  def toStream: Stream[ P ] = { 
    val f = generator.get
    f(begin.get, end.get, config.get)
  }
  
}


case class SlearningRate( override val value: Double = 0.0) extends SParameter[ Double ] { 
  def apply( v: Double ) = new SlearningRate( v ) 
  }

class ChangeOption[P](v : Option[P]) {
  
  def this() {
    this(None)
  }
  
  def to[Q](nv:  Q ) : ChangeOption[Q] = new ChangeOption( Some(nv) )
}

/*
trait SParameterRange[ P <: SParameter[ _ ], U, B, E, C ] {
*/

object SParameterTest {
    
    def linSearch(from: SlearningRate, to: SlearningRate, by: Double) : Stream[ SlearningRate ] = {
      val len = ( ( to.value - from.value ) / by).ceil.toInt
      linspace( from.value, by ).map{ x => from.apply( x ) }.take(  len + 1)
    }
    
// TODO: how to change P type when using default constructor 
// TODO: How to make generators independent from type P 

  def main( args: Array[ String ] ) {
    
    val t0 = SlearningRate()
    val t  = new SLinearParameterRange( t0 ) // TODO: Hide constructor and use companion object with apply
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
    
    val u = new ChangeOption
    val u1 = u to 100
    
    val w2 = t.from(SlearningRate( 0.0 )).to(SlearningRate( 0.01 )).by(0.001 )
    //val w3 = w2 build()
    //val w4 = w3.toStream
    //val w4 = w2.toStream
    //w4.foreach { x => println( x ) }
  }
 
  
}
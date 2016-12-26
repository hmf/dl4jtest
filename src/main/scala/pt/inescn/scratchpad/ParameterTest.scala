package pt.inescn.scratchpad

import StreamBuilder._

// Parameters are value classes

/**
 *
 * @see http://stackoverflow.com/questions/19642053/when-to-use-val-or-def-in-scala-traits
 */
trait TParameter[ T ] {
  def value: T // must be a def or var so that we can override and initialize from the extended class
  
  def apply( v: T ): TParameter[ T ]
}

import scala.{ Option, Some, None }

trait TParameterRange[ P <: TParameter[ _ ], U, B, E, C ] {
  var begin: P
  var end: P
  var config: Option[ U ]

  def apply( begin: P): TParameterRange[ P, U, TBEGIN, TMISSING, TMISSING]

  def from( nbegin: P ): TParameterRange[ P, U, TBEGIN, E, C ]
  def to( nend: P ): TParameterRange[ P, U, B, TEND, C ]
  def by( nconfig: U ): TParameterRange[ P, U, B, E, TCONFIG ]
}

trait TStreamableParameterRange[ P <: TParameter[ _ ], U, B, E, C ] extends TParameterRange[ P, U, B, E, C ]  {
  // TODO; this : TParameter[ T ] => 
  def toStream: Stream[ P ]
}

// Can also use `abstract class`
trait TBEGIN
trait TEND
trait TCONFIG
trait TMISSING

// TODO: pimp my class and add toStream
// How do we define and hide the TParameterRange.toStream
trait TSafeBuild[ P <: TParameter[ _ ], U ] {
  def build(): TStreamableParameterRange[ P, U, TBEGIN, TEND, TCONFIG ]
}

import scala.language.implicitConversions

// http://stackoverflow.com/questions/3893274/how-to-mix-in-a-trait-to-instance
// http://stackoverflow.com/questions/15079444/how-to-deep-copy-classes-with-traits-mixed-in
object TParameterRange {

  type TPDouble = TParameter[ Double ]
  
  //class mixedParameterRange[ P, U, B, E, C] extends TParameterRange[ P, U, B, E, C] with TStreamableParameterRange[ P, U, B, E, C]
  
  /*
   * TODO: make the range parametersized by a stream builder (linear, exponential, any function, ...)
   * TODO: make the safe build agnostic to the range.
   */
  implicit def enableBuild( builder: TParameterRange[ TPDouble, Double, TBEGIN, TEND, TCONFIG ] ) = new TSafeBuild[TPDouble,Double] {
    def build() : TStreamableParameterRange[ TPDouble, Double, TBEGIN, TEND, TCONFIG ] = 
      //new TLinearParameterRange[ TBEGIN, TEND, TCONFIG ]( builder.begin, builder.end, builder.config ) with TStreamableParameterRange[ TPDouble, Double, TBEGIN, TEND, TCONFIG ]
      new TLinearParameterRange[ TBEGIN, TEND, TCONFIG ]( builder.begin, builder.end, builder.config ) with TStreamableParameterRange[ TPDouble, Double, TBEGIN, TEND, TCONFIG ]
  }
  
  implicit def enableBuildX( builder: TParameterRange[ TPDouble, Double, TBEGIN, TEND, TCONFIG ] )  : TStreamableParameterRange[ TPDouble, Double, TBEGIN, TEND, TCONFIG ] = {
      new TLinearParameterRange[ TBEGIN, TEND, TCONFIG ]( builder.begin, builder.end, builder.config ) with TStreamableParameterRange[ TPDouble, Double, TBEGIN, TEND, TCONFIG ]
    }
  
}

private class TLinearParameterRange[ B, E, C ]( override var begin: TParameterRange.TPDouble, override var end: TParameterRange.TPDouble, override var config: Option[ Double ] )
    extends TParameterRange[ TParameterRange.TPDouble, Double, B, E, C ] {
  //var x: U = _

  def this( begin: TParameterRange.TPDouble ){
    this( begin, begin, None )
  }

 def apply( nbegin: TParameterRange.TPDouble): TParameterRange[ TParameterRange.TPDouble, Double, TBEGIN, TMISSING, TMISSING] = new TLinearParameterRange[ TBEGIN, TMISSING, TMISSING ]( nbegin )
  
  def from( nbegin: TParameterRange.TPDouble ): TParameterRange[ TParameterRange.TPDouble, Double, TBEGIN, E, C ] = new TLinearParameterRange[ TBEGIN, E, C ]( nbegin, end, config )
  def to( nend: TParameterRange.TPDouble ): TParameterRange[ TParameterRange.TPDouble, Double, B, TEND, C ] = new TLinearParameterRange[ B, TEND, C ]( begin, nend, config )
  def by( nconfig: Double ): TParameterRange[ TParameterRange.TPDouble, Double, B, E, TCONFIG ] = new TLinearParameterRange[ B, E, TCONFIG ]( begin, end, Some( nconfig ) )

  // a to b with length = l
  // from a  by delta (no b!!)
  // a to b by delta ?

  // TODO: make parameterizable by StreamBuilder
  def toStream: Stream[ TParameterRange.TPDouble ] = {
    val len = ( ( end.value - begin.value  ) / config.get ).ceil.toInt
    linspace( begin.value, end.value ).map{ x => begin.apply( x ) }.take(  len )
  }
}

case class TlearningRate( override val value: Double = 0.0) extends TParameter[ Double ] { 
  def apply( v: Double ) = new TlearningRate( v ) 
  }

case class TpValue( override val value: Double ) extends TParameter[ Double ] { 
  def apply( v: Double ) = new TlearningRate( v ) 
 }

object ParameterTest {
  
  def main( args: Array[ String ] ) {
    
    val tt  = new TLinearParameterRange( TlearningRate( 0.0) ) // Hide constructor and use companion object with apply
    val t0 = TlearningRate()
    val t1 = TlearningRate( 0.0) //.apply(100.0)
    val t2 = t1(100.0)
    
    //val t3 = tt.toStream  // TODO: should NOT compile
    
    val t  = new TLinearParameterRange( t0 ) // TODO: Hide constructor and use companion object with apply
    val w2 = t.from(TlearningRate( 0.0 )).to(TlearningRate( 0.01 )).by(0.001 )
    val w3 = w2 build()
    val w4 = w3.toStream
    w4.foreach { x => println( x ) }
    
    val w5 = w2.toStream
    w5.foreach { x => println( x ) }
    
    /*
    println( "????????????" )
    val len = ( ( 0.01 - 0.0 ) / 0.001 ).ceil.toInt
    val v1 = linspace( 0.0, 0.001 ).map{ x => TlearningRate( x ) }.take( len )
    v1.foreach { x => println( x ) }
  */
  }
  
}
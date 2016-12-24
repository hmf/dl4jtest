package pt.inescn.scratchpad

import StreamBuilder._

// Parameters are value classes

/**
 *
 * @see http://stackoverflow.com/questions/19642053/when-to-use-val-or-def-in-scala-traits
 */
trait TParameter[ T ] {
  def value: T // must be a def or var so that we can override and initialize from the extended class
  //val na : TParameter[Nothing] = Naught

  //def from[ T, U](begin : this.type): TParameterRange[ T, U, TBEGIN, TMISSING, TMISSING ] 
  def apply( v: T ): TParameter[ T ]
}

import scala.{ Option, Some, None }

//case object Naught extends TParameter[Nothing] { def value = this}

trait TParameterRange[ P <: TParameter[ _ ], U, B, E, C ] {
  var begin: P
  var end: P
  var config: Option[ U ]

  def apply( begin: P, end: P, config: U ): TParameterRange[ P, U, TBEGIN, E, C]

  def to( nend: P ): TParameterRange[ P, U, B, TEND, C ]
  def by[ V ]( nconfig: V ): TParameterRange[ P, V, B, E, TCONFIG ]

  //def toStream: Stream[ P ]
}

trait TStreamableParameterRange[ P <: TParameter[ _ ], U, B, E, C ] extends TParameterRange[ P, U, B, E, C ]  {
  def toStream: Stream[ P ]
}

/*
trait TStreamableParameterRange[ P <: TParameter[ _ ], U, B, E, C ] extends TParameterRange[ P, U, B, E, C ]  {
  def toStream: Stream[ P ]
}
*/

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

object TParameterRange {

  def apply[ P <: TParameter[ _ ], U ]( begin: P, end: P, config: U ): TParameterRange[ P, U, TBEGIN, TEND, TCONFIG ] =
    new TLinearParameterRange[ P, U, TBEGIN, TEND, TCONFIG ]( begin, end, Some( config ) )

    // TODO: remove B,E and S
  def from[ P <: TParameter[ _ ], U, B, E, S ]( begin: P ): TParameterRange[ P, U, TBEGIN, TMISSING, TMISSING ] =
    new TLinearParameterRange[ P, U, TBEGIN, TMISSING, TMISSING ]( begin )

    // TODO: remove the end and config and make those MISSING
  def create[ R <: TParameterRange[ P, U, B, E, C ], P <: TParameter[ _ ], U, B, E, C ]( range: R, begin: P, end: P, config: U ): TParameterRange[ P, U, TBEGIN, E, C] =
    //new TLinearParameterRange[ P, U, TBEGIN, TEND, TCONFIG ]( begin, end, Some( config ) )
    range.apply(begin, end, config )
    
  implicit def enableBuild[ P <: TParameter[ _ ], U ]( builder: TParameterRange[ P, U, TBEGIN, TEND, TCONFIG ] ) = new TSafeBuild[P,U] {
    // TODO: bug - pass builder.end and builder.cinfig to constructor
    def build() : TStreamableParameterRange[ P, U, TBEGIN, TEND, TCONFIG ] = 
      new TLinearParameterRange[ P, U, TBEGIN, TEND, TCONFIG ]( builder.begin ) with TStreamableParameterRange[ P, U, TBEGIN, TEND, TCONFIG ]
  }
}

private class TLinearParameterRange[ P <: TParameter[ _ ], U, B, E, C ]( override var begin: P, override var end: P, override var config: Option[ U ] )
    extends TParameterRange[ P, U, B, E, C ] {

  //var x: U = _

  def this( begin: P ) {
    this( begin, begin, None )
  }

 def apply( nbegin: P, nend: P, nconfig: U ): TParameterRange[ P, U, TBEGIN, E, C] = new TLinearParameterRange[ P, U, TBEGIN, E, C ]( nbegin, nend, Some(nconfig) )
  
  def to( nend: P ): TParameterRange[ P, U, B, TEND, C ] = new TLinearParameterRange[ P, U, B, TEND, C ]( begin, nend, config )
  def by[ V ]( nconfig: V ): TParameterRange[ P, V, B, E, TCONFIG ] = new TLinearParameterRange[ P, V, B, E, TCONFIG ]( begin, end, Some( nconfig ) )

  // a to b with length = l
  // from a  by delta (no b!!)
  // a to b by delta ?

  def toStream: Stream[ P ] = ??? /* TODO {
    val len = ( ( end.value - begin.value  ) / config.get ).ceil.toInt
    linspace( begin.value, end.value ).map{ x => TlearningRate( x ) }.take(  len )
  }*/
}

case class TlearningRate( override val value: Double ) extends TParameter[ Double ] { def apply( v: Double ) = new TlearningRate( v ) }
case class TpValue( override val value: Double ) extends TParameter[ Double ] { def apply( v: Double ) = new TlearningRate( v ) }

object ParameterTest {
  
  def main( args: Array[ String ] ) {
    val x =TParameterRange( TlearningRate( 0.0 ), TlearningRate( 0.01 ), 0.001 )
  
    val z1 = TParameterRange.from( TlearningRate( 0.0 ) )
    val z2 = z1.to( TlearningRate( 0.01 ) )
    val z3 = z2.by( 0.001 )
    // val z4 = z3.toStream hidden, cannot use
  
    import TParameterRange._
    val y1 = from( TlearningRate( 0.0 ) )
    val y2 = y1.to( TlearningRate( 0.01 ) )
    val y3 = y2.by( 0.001 )
    //val y4 = y2.build()  // compile error
    val y4 = y3.build()
    //val y5 = y3.toStream // compile error
    val y5 = y4.toStream
  
    val t  = new TLinearParameterRange( TlearningRate( 0.0 ), TlearningRate( 0.01 ),Some( 0.001) )
    val w1 = TParameterRange.create(t, TlearningRate( 0.0 ), TlearningRate( 0.01 ),0.001 )
    
    println( "????????????" )
    val len = ( ( 0.01 - 0.0 ) / 0.001 ).ceil.toInt
    val v1 = linspace( 0.0, 0.001 ).map{ x => TlearningRate( x ) }.take( len )
    v1.foreach { x => println( x ) }
  
    //import TParameterRange._
    //val t1 = to( TlearningRate( 0.0 ), TlearningRate( 0.01 ), TlearningRate( 0.001 ) )  
  }
  
}
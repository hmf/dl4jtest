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
  //def apply(): TParameter[ T ]
  def apply( v: T ): TParameter[ T ]
}

import scala.{ Option, Some, None }

//case object Naught extends TParameter[Nothing] { def value = this}

trait TParameterRange[ P <: TParameter[ _ ], U, B, E, C ] {
  var begin: P
  var end: P
  var config: Option[ U ]

  def apply( begin: P): TParameterRange[ P, U, TBEGIN, TMISSING, TMISSING]

  def from( nbegin: P ): TParameterRange[ P, U, TBEGIN, E, C ]
  def to( nend: P ): TParameterRange[ P, U, B, TEND, C ]
  def by( nconfig: U ): TParameterRange[ P, U, B, E, TCONFIG ]

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

  type TPDouble = TParameter[ Double ]
/*
  def apply[ P <: TParameter[ _ ]]( begin: P): TParameterRange[ P, Double, TBEGIN, TMISSING, TMISSING] =
    new TLinearParameterRange[ P, TBEGIN, TMISSING, TMISSING]( begin)

    // TODO: remove B,E and S
  def from[ P <: TParameter[ _ ], B, E, S ]( begin: P ): TParameterRange[ P, Double, TBEGIN, TMISSING, TMISSING ] =
    new TLinearParameterRange[ P, TBEGIN, TMISSING, TMISSING ]( begin )

    // TODO: remove the end and config and make those MISSING
  def create[ R <: TParameterRange[ P, U, B, E, C ], P <: TParameter[ _ ], U, B, E, C ]( range: R, begin: P, end: P, config: U ): TParameterRange[ P, U, TBEGIN, TMISSING, TMISSING] =
    //new TLinearParameterRange[ P, U, TBEGIN, TEND, TCONFIG ]( begin, end, Some( config ) )
    range.apply(begin)
*/
  implicit def enableBuild( builder: TParameterRange[ TParameterRange.TPDouble, Double, TBEGIN, TEND, TCONFIG ] ) = new TSafeBuild[TParameterRange.TPDouble,Double] {
    // TODO: bug - pass builder.end and builder.cinfig to constructor
    def build() : TStreamableParameterRange[ TParameterRange.TPDouble, Double, TBEGIN, TEND, TCONFIG ] = 
      new TLinearParameterRange[ TBEGIN, TEND, TCONFIG ]( builder.begin, builder.end, builder.config ) with TStreamableParameterRange[ TParameterRange.TPDouble, Double, TBEGIN, TEND, TCONFIG ]
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

  def toStream: Stream[ TParameterRange.TPDouble ] = {
    val len = ( ( end.value - begin.value  ) / config.get ).ceil.toInt
    val t = begin(100)
    //linspace( begin.value, end.value ).map{ x => begin.apply( x ) }.take(  len )
    val t1 = linspace( begin.value, end.value )
    val t2 : Stream[TParameterRange.TPDouble] =  t1.map { x => begin(x) }
    t2.take( len )
  }
}

case class TlearningRate( override val value: Double = 0.0) extends TParameter[ Double ] { 
/*  def this() {
    this(0.0)
  }*/
  //def apply( ) = new TlearningRate( 0.0 )
  def apply( v: Double ) = new TlearningRate( v ) 
  }
case class TpValue( override val value: Double ) extends TParameter[ Double ] { 
  //def apply( ) = new TlearningRate( 0.0 )
  def apply( v: Double ) = new TlearningRate( v ) 
 }

object ParameterTest {
  
  def main( args: Array[ String ] ) {
    /*
    val x =TParameterRange( TlearningRate( 0.0 ) )  // TODO: remove all functions that return a specific parameter range by default
  
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
  
    val t0  = new TLinearParameterRange( TlearningRate( 0.0 ), TlearningRate( 0.01 ),Some( 0.001) ) // Hide constructor and use companion object with apply
    val t  = new TLinearParameterRange( TlearningRate( 0.0) ) // Hide constructor and use companion object with apply
    val w1 = TParameterRange.create(t, TlearningRate( 0.0 ), TlearningRate( 0.01 ),0.001 )
    */
    
    val tt  = new TLinearParameterRange( TlearningRate( 0.0) ) // Hide constructor and use companion object with apply
    val t0 = TlearningRate()
    val t1 = TlearningRate( 0.0) //.apply(100.0)
    val t2 = t1(100.0)
    
    
    val t  = new TLinearParameterRange( t0 ) // Hide constructor and use companion object with apply
    val w2 = t.from(TlearningRate( 0.0 )).to(TlearningRate( 0.01 )).by(0.001 )
    val w3 = w2 build()
    val w4 = w3.toStream
    
    println( "????????????" )
    val len = ( ( 0.01 - 0.0 ) / 0.001 ).ceil.toInt
    val v1 = linspace( 0.0, 0.001 ).map{ x => TlearningRate( x ) }.take( len )
    v1.foreach { x => println( x ) }
  
    //import TParameterRange._
    //val t1 = to( TlearningRate( 0.0 ), TlearningRate( 0.01 ), TlearningRate( 0.001 ) )  
  }
  
}
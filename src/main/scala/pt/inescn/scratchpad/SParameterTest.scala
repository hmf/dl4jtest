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
  
  type Gen = (T, T, U) => Stream[ T ]
  
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
   def toStream: Stream[ SParameter[T] ]
   //def toStream: Stream[ P[T] ]
}

// TODO: remove extends and use self
trait SStreamableParameterRange[ P[T] <: SParameter[T], T, U, B, E, C ] extends SParameterRange[ P, T, U, B, E, C ]  {
  // TODO; this : SParameter[ T ] => 
  def toStream: Stream[ SParameter[T] ]
  //def toStream: Stream[ P[T] ]
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
                                                                          val generator: Option[ (T,T,Double) => Stream[T]] )
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
  
  def using( gen : (T, T, Double) => Stream[ T ] ): SParameterRange[ P, T, Double, B, E, TCONFIG ] =  {
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
  // TODO: how do we return a P[T]
  def toStream: Stream[ SParameter[T] ] = { 
  //def toStream: Stream[ P[T] ] = { 
    val f = generator.get
    val st = f(begin.get.value, end.get.value, config.get)
    val o = begin.get
    //val r = st.map{ x => begin.get.apply( x ) }
    val r = st.map{ x => o( x ) }
    r
  }
  
}


// http://stackoverflow.com/questions/14729996/scala-implementing-method-with-return-type-of-concrete-instance
// http://stackoverflow.com/questions/4313139/how-to-use-scalas-this-typing-abstract-types-etc-to-implement-a-self-type

case class SlearningRate( override val value: Double = 0.0) extends SParameter[ Double ] { 
  def apply( v: Double ) = new SlearningRate( v ) 
  }

abstract class  Inner[T] {
  type Self <: Inner[T]
  
  def apply(t:T) : Self
}

class InnerA[T](v: T) extends Inner[T] {
  type Self = InnerA[T]
  def apply(t:T) : Self = new InnerA[T](t)
}

class InnerB[T](v: T) extends Inner[T]{
  type Self = InnerB[T]
  def apply(t:T) : Self = new InnerB[T](t)
}

class InnerC[T](v: T) extends Inner[T]{
  type Self = InnerB[T]                                           // IMPORTANT: BUG, we don't get the correct concrete type
  def apply(t:T) : Self = new InnerB[T](t)
}

// http://jim-mcbeath.blogspot.pt/2009/09/type-safe-builder-in-scala-using-church.html
// https://gist.github.com/jrudolph/182249
// http://www.scala-lang.org/old/node/5124
// http://www.scala-lang.org/old/node/5213 

trait Builder[T] {
  def apply(c: T): T
}

class Outer[T, P[T] <: Inner[T]](val u: P[T]) {
//class Outer[T, P[T]](u: P[T]) {
  //def makeInner() : OuterX[T,P] = new OuterX(new  Inner(100))  // type mismatch; found : pt.inescn.scratchpad.Inner[T] required: P[T]
  //def makeInner[Q[T]](v: Q[T]) : Q[T] = v(u)  //  Q[T] does not take parameters
  def convertTo[Q[S]<: Inner[S],S](v: Q[S]) : Outer[S, Q] = new Outer(v)
  //def convertTo[Q[S],S](v: Q[S]) : Outer[S, Q] = new Outer(v)
  def extract : P[T] = u 
  def make(t: T)  = u(t) // : P[T] type mismatch; found : Outer.this.u.Self required: P[T]
  /*
  def duplicate(v:T) : P[T] = u(v) // This is not possible because u has type P[T], 
                                                  // its not a class so we do not know what constructors are available*/
  //def duplicate(builder : Builder[P[T]])
}

object SParameterTest {
  
  val u = new InnerA(100)                // InnerA[Int]
  val u1 = new InnerA("100")           // InnerA[String]
  val u2 = new Outer(u)                 // Outer[Int, Inner]
  val u3 = u2.convertTo(u1)           // Outer[String, Inner]
  val u4 = u3.extract                     //  InnerA[String]
  val u5 = new InnerB("100")        // InnerB[String]
  val u6 = u3.convertTo(u5)           // Outer[String, InnerB]
  val u7 = u6.extract                     //  InnerB[String]
  val u8 :  InnerB[String] = u6.make("101")               //  InnerB[String]
  val u9 = new InnerC(101)            // InnerC[Int]
  val u10 = new Outer(u9)             // Outer[Int, InnerC]
  //val u11 : InnerC[Int] = u9(102)   // type mismatch; found : pt.inescn.scratchpad.SParameterTest.u9.Self (which expands to) 
                                                    // pt.inescn.scratchpad.InnerB[Int] required: pt.inescn.scratchpad.InnerC[Int]
  //val u12 :  InnerC[Int] = u10.make(1001)            // But we have to expand the type explicitly, same compile rrro as above
  
  
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


    
    //val w2 = t.from(SlearningRate( 0.0 )).to(SlearningRate( 0.01 )).by(0.001 )
    //val w3 = w2 build()
    //val w4 = w3.toStream
    //val w4 = w2.toStream
    //w4.foreach { x => println( x ) }
  }
 
  
}
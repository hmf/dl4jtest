package pt.inescn.scratchpad

import scala.util.Random
import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.distribution.ParetoDistribution;
import org.apache.commons.math3.distribution.UniformIntegerDistribution;
import org.apache.commons.math3.distribution.WeibullDistribution;
import org.apache.commons.math3.random.JDKRandomGenerator;
import org.apache.commons.math3.random.RandomGenerator;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.Period;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import sun.security.util.Length

/**
 *
 * http://blog.dmitryleskov.com/programming/scala/stream-hygiene-i-avoiding-memory-leaks/
 *
 *  import util.Random.nextInt
 *  Stream.continually(nextInt(100)).take(10)
 *
 *
 *  let rands = Stream.iterate(rand.nextInt) {
 *    _ => rand.nextInt
 *  }
 *  val taken = rands take 5
 *
 *  def rands: Stream[Int] = rand.nextInt() #:: rands
 *  val taken = rands take 5
 *
 *
 *  http://jliszka.github.io/2013/08/12/a-frequentist-approach-to-probability.html
 */
object StreamBuilder {

  /**
   * Default random generator. You can use the JDK's as below,
   * us the JDKs default or use commons.math random generator.
   */
  val defr = new JDKRandomGenerator();
  val defj = new Random();

  // Define function with the same input but different return types. 
  // http://stackoverflow.com/questions/17888978/scala-trait-same-method-and-argument-with-different-return-types
  //Not used  http://stackoverflow.com/questions/3307427/scala-double-definition-2-methods-have-the-same-type-erasure
  //def uniform( r: Random, max: Int ): Stream[ Int ] = Stream.continually( r.nextInt( max ) )
  trait DiscreteImpl[ T ] { def apply( r: Random ): T }
  implicit object DiscreteInt extends DiscreteImpl[ Int ] { def apply( r: Random ) = r.nextInt }
  implicit object DiscreteLong extends DiscreteImpl[ Long ] { def apply( r: Random ) = r.nextLong }
  implicit object DiscreteFloat extends DiscreteImpl[ Float ] { def apply( r: Random ) = r.nextFloat }
  implicit object DiscreteDouble extends DiscreteImpl[ Double ] { def apply( r: Random ) = r.nextDouble }
  implicit object DiscreteBoolean extends DiscreteImpl[ Boolean ] { def apply( r: Random ) = r.nextBoolean }
  implicit object DiscreteChar extends DiscreteImpl[ Char ] { def apply( r: Random ) = r.nextPrintableChar }
  def uniform[ T ]( r: Random = defj )( implicit impl: DiscreteImpl[ T ] ): Stream[ T ] = Stream.continually( impl( r ) )

  // Cannot use defaults here
  trait DiscreteLenImpl[ T ] { def apply( r: Random, length: Int ): T }
  // We get any characters from UNICODE page, many Asian characters
  //implicit object DiscreteString extends DiscreteLenImpl[String] { def apply(r: Random, length: Int) =r.nextString(length) }
  implicit object DiscreteString extends DiscreteLenImpl[ String ] {
    def apply( r: Random, length: Int ) = r.alphanumeric.take( length ).mkString
  }
  implicit object DiscreteInteger extends DiscreteLenImpl[ Int ] { def apply( r: Random = defr, length: Int ) = r.nextInt( length ) }
  def uniform[ T ]( r: Random, length: Int )( implicit impl: DiscreteLenImpl[ T ] ): Stream[ T ] = Stream.continually( impl( r, length ) )

  def normal( r: RandomGenerator = defr, mean: Double = 0, sd: Double = 1 ): Stream[ Double ] = {
    val d = new NormalDistribution( r, mean, sd )
    return Stream.continually( d.sample )
  }

  def weibull( r: RandomGenerator = defr, alpha: Double, beta: Double ): Stream[ Double ] = {
    val d = new WeibullDistribution( r, alpha, beta )
    return Stream.continually( d.sample )
  }

  def pareto( r: RandomGenerator = defr )( scale: Double, shape: Double ): Stream[ Double ] = {
    val d = new ParetoDistribution( r, scale, shape )
    return Stream.continually( d.sample )
  }

  def zonedDateTime( start: LocalDateTime, samplingRate: Duration, zone: ZoneId = ZoneId.systemDefault() ): Stream[ ZonedDateTime ] = {
    // ZoneId lisbon = ZoneId.of("Europe/Lisbon");

    //now = LocalDateTime.now();
    val startTime = ZonedDateTime.of( start, zone )
    // sampling rate converted to delta durations
    val sampling = Stream.iterate( samplingRate ) { s => s.plus( samplingRate ) }
    // Add delta to start the to get final sampled date time-stamp
    return sampling.map( s => startTime.plus( s ) )

  }

/**
   * Generates a stream of linearly spaced values between a and b (inclusive).
   * The returned vector will have length elements, defaulting to 100.
   */
  def linspace(a : Double, b : Double, length : Int = 100) : Stream[Double] = {
    val increment = (b - a) / (length - 1)
    Stream.iterate(a) {  acc => acc + increment }.take(length)
    }

/**
   * Generates a stream of linearly spaced by `increment` values starting from a.
   */
  def linspace(a : Double, increment: Double) : Stream[Double] = Stream.iterate(a) {  acc => acc + increment }

  /**
   * Returns stream of Seq containing each element in `s`
   */
  def combine[ T ]( s: Seq[ Stream[ T ] ] ): Stream[ Seq[ T ] ] = s.map( _.head ) #:: combine( s.map( _.tail ) )
  //def combine[T](s:Seq[Stream[T]]):Stream[Seq[T]] = s.flatMap(_.headOption) #:: combine( s.map(_.tail) )

  /*
  def extract[T](s: Stream[T]) = {
    s match {
      case head #:: tail => Some(Tuple2(head, tail))
      case Stream.Empty => None
    }
  }
  
  import scala.reflect.ClassTag
  
  // https://github.com/milessabin/shapeless/#heterogenous-lists
  // http://stackoverflow.com/questions/6849958/arbitrary-size-tuple-with-first-element-type-fixed
  // http://stackoverflow.com/questions/26124101/combining-scala-streams
  def combine[T : ClassTag, C](s1: Stream[T], s2: Stream[T]) : Stream[ Array[T] ]= {
    (extract(s1), extract(s2)) match {
      //case (Some((h1,t1)), Some((h2,t2)) ) => Array(h1, h2) #:: combine(t1,t2)
      case (Some((h1,t1)), Some((h2,t2)) ) => Array[T](h1, h2) #:: combine(t1,t2)
      case (None, None) => Stream.Empty
      case (None, Some(s2)) => Stream.Empty
      case (Some(s1), None) => Stream.Empty
    }
  }*/

  //def combine[ T ]( s1: Stream[ T ], s2: Stream[ T ] ): Stream[ Seq[ T ] ] = combine( Array( s1, s2 ) )
  //def combine[ T1, T2]( s1: Stream[ T1 ], s2: Stream[ T2 ] )(implicit ev: T1=:=T2): Stream[ Seq[ T1 ] ] = combine( Array( s1, s2 ) )
 // http://blog.bruchez.name/2015/11/generalized-type-constraints-in-scala.html
  def combine[ T, T1 <:T, T2 <: T]( s1: Stream[ T1], s2: Stream[ T2 ] )(implicit ev: T1=:=T2): Stream[ Seq[ T ] ] = combine( Array( s1, s2 ) )

  def main( args: Array[ String ] ): Unit = {

    //val r = new Random(1234)
    val r = new JDKRandomGenerator( 1234 );

    val s1 = uniform[ Int ]( r )
    println( s1.take( 10 ).toList.mkString( "{", ",", "}" ) )

    val s2 = uniform[ Boolean ]( r )
    println( s2.take( 10 ).toList.mkString( "{", ",", "}" ) )

    val s3 = uniform[ Int ]( r, 100 )
    println( s3.take( 10 ).toList.mkString( "{", ",", "}" ) )

    val s4 = normal( r, 10, 2 )
    println( s4.take( 10 ).toList.mkString( "{", ",", "}" ) )

    val s5 = weibull( alpha = 10, beta = 2 )
    println( s5.take( 10 ).toList.mkString( "{", ",", "}" ) )

    val paretor = pareto( r )( _, _ )
    val s6 = paretor( 10, 2 )
    println( s6.take( 10 ).toList.mkString( "{", ",", "}" ) )

    val s7 = uniform[ Char ]( r )
    println( s7.take( 10 ).toList.mkString( "{", ",", "}" ) )

    val s8 = uniform[ String ]( r, length = 5 )
    println( s8.take( 10 ).toList.mkString( "{", ",", "}" ) )

    val sampling1 = Duration.ofMillis( 30 )
    val s9 = zonedDateTime( LocalDateTime.now(), sampling1 )
    println( s9.take( 10 ).toList.mkString( "{", ",", "}" ) )

    val sampling2 = Duration.ofSeconds( 30 )
    val s10 = zonedDateTime( LocalDateTime.now(), sampling2, ZoneId.of( "Europe/Berlin" ) )
    println( s10.take( 10 ).toList.mkString( "{", ",", "}" ) )

    val s11 = combine( s1, s3 )
    println( s11.take( 10 ).toList.map( a => a.mkString( "<", ",", ">" ) ).mkString( "{", ",", "}" ) )

    val s12 = combine( s5, s6 )
    println( s12.take( 10 ).toList.map( a => a.mkString( "<", ",", ">" ) ).mkString( "{", ",", "}" ) )

    // Safe, must be of the same type
    //val s13 = combine( s1, s6 )
    //println( s13.take( 10 ).toList.map( a => a.mkString( "<", ",", ">" ) ).mkString( "{", ",", "}" ) )

    // TODO: not safe, how do we avoid AnyVal?
    val s14 = combine( List( s4, s5, s6 ) )
    println( s14.take( 10 ).toList.map( a => a.mkString( "<", ",", ">" ) ).mkString( "{", ",", "}" ) )

    val s15 = Stream.from( 1 )
    val s16 = combine( List( s15, s1, s3 ) )
    println( s16.take( 10 ).toList.map( a => a.mkString( "<", ",", ">" ) ).mkString( "{", ",", "}" ) )

    val s17 = linspace(0.0, 1.0, 10)
    println( s17.take( 12 ).toList.mkString( "{", ",", "}" ) )

    val s18 = linspace(0.0, 1/9.0)
    println( s18.take( 12 ).toList.mkString( "{", ",", "}" ) )
   
    // append streams (note the take)
    val s19  = s15.take(5) #::: Stream.from(2, 2)
    println( s19.take( 10 ).toList.mkString( "{", ",", "}" ) )
    
    // append streams (note the take)
    val s20 = s19.take(15) append Stream.from(1, -1)
    println( s20.take( 20 ).toList.mkString( "{", ",", "}" ) )

    // append combined streams (note the take)
    val s21 =  combine( List( s4, s5 ) ).take(5) #:::  combine( List( s5, s6 ) )
    println( s21.take( 10 ).toList.map( a => a.mkString( "<", ",", ">" ) ).mkString( "{", ",", "}" ) )
    
  }

}
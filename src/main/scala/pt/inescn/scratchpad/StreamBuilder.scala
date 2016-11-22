package pt.inescn.scratchpad

import scala.util.Random
import org.apache.commons.math3.distribution.UniformRealDistribution
import org.apache.commons.math3.distribution.NormalDistribution 
import org.apache.commons.math3.distribution.ParetoDistribution 
import org.apache.commons.math3.distribution.UniformIntegerDistribution 
import org.apache.commons.math3.distribution.WeibullDistribution 
import org.apache.commons.math3.random.JDKRandomGenerator 
import org.apache.commons.math3.random.RandomGenerator 

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
  val defj = new scala.util.Random();

  // Define function with the same input but different return types. 
  // http://stackoverflow.com/questions/17888978/scala-trait-same-method-and-argument-with-different-return-types
  //Not used  http://stackoverflow.com/questions/3307427/scala-double-definition-2-methods-have-the-same-type-erasure
  //def uniform( r: Random, max: Int ): Stream[ Int ] = Stream.continually( r.nextInt( max ) )
  trait DiscreteImpl[ T ] { def apply( r: scala.util.Random ): T }
  implicit object DiscreteInt extends DiscreteImpl[ Int ] { def apply( r: scala.util.Random ) = r.nextInt }
  implicit object DiscreteLong extends DiscreteImpl[ Long ] { def apply( r: scala.util.Random ) = r.nextLong }
  implicit object DiscreteFloat extends DiscreteImpl[ Float ] { def apply( r: scala.util.Random ) = r.nextFloat }
  implicit object DiscreteDouble extends DiscreteImpl[ Double ] { def apply( r: scala.util.Random ) = r.nextDouble }
  implicit object DiscreteBoolean extends DiscreteImpl[ Boolean ] { def apply( r: scala.util.Random ) = r.nextBoolean }
  implicit object DiscreteChar extends DiscreteImpl[ Char ] { def apply( r: scala.util.Random ) = r.nextPrintableChar }
  def uniform[ T ]( r: scala.util.Random = defj )( implicit impl: DiscreteImpl[ T ] ): Stream[ T ] = Stream.continually( impl( r ) )

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

  import java.util.NavigableMap;
  import java.util.Random;
  import java.util.TreeMap;

  /**
   * Rejection Sampling 
   * http://stackoverflow.com/questions/30203362/how-to-generate-a-random-weighted-distribution-of-elements
   * http://en.wikipedia.org/wiki/Rejection_sampling
   */
  class RandomCollection[ E ](random: Random = new Random(), classes : Map[Double, E]) {
    
    val tmap = scala.collection.immutable.TreeMap[ Double, E ]()
    // calculate cumulative proportions
    val (total, nmap) = classes.foldLeft((0.0, tmap)) {
      case ( (acc, m), (k,v)) =>  
       val nacc = acc + k
       (nacc, m + (nacc -> v))
    }
    val map = if (total != 1.0) tmap else nmap

    def ceiling( weight: Double): Option[E] = {
      val from = map.from( weight )
      if ( from.isEmpty ) None
      else Some( map( from.firstKey ) )
    }

    def floor(weight: Double): Option[E] = {
      val to = map.to(weight)
      if (to.isEmpty) None
      else Some(map(to.lastKey))
    }
  }
  
  def discreteMultiModal[E]( r: RandomGenerator = defr, classes : Map[Double, E]): Stream[ E ] = {
    val d = new UniformRealDistribution( r, 0.0, 1.0 )
    val w = new RandomCollection[E](defr, classes)
    return Stream.continually{ 
      val value = d.sample * w.total 
      w.ceiling(value).get
     }
  }

  // TODO: need standard Double output bimodal - must be based on basic multi-model continuous
  
  // TODO: Multi-modal for a mix of distributions (mixture model)
  //  Both double and discrete
  // TODO: general discrete distributions, see current biModal
  
  /**
   * Generates a stream of linearly spaced values between a and b (inclusive).
   * The returned vector will have length elements, defaulting to 100.
   */
  def linspace( a: Double, b: Double, length: Int = 100 ): Stream[ Double ] = {
    val increment = ( b - a ) / ( length - 1 )
    Stream.iterate( a ) { acc => acc + increment }.take( length )
  }

  /**
   * Generates a stream of linearly spaced by `increment` values starting from a.
   */
  def linspace( a: Double, increment: Double ): Stream[ Double ] = Stream.iterate( a ) { acc => acc + increment }

  // https://groups.google.com/forum/#!topic/scala-user/QDMBgh9Z_vY
  // https://github.com/Jasper-M/implicitlogic/tree/master/src/main/scala/implicitlogic
  
  // TODO: return indexed sequence
  // https://twitter.github.io/scala_school/advanced-types.html
  /**
   * Returns stream of Seq containing each element in `s`
   */
  //def combine[ T ]( s: Seq[ Stream[ T ] ] ): Stream[ Seq[ T ] ] = s.map( _.head ) #:: combine( s.map( _.tail ) )
  //def combine[T](s:Seq[Stream[T]]):Stream[Seq[T]] = s.flatMap(_.headOption) #:: combine( s.map(_.tail) )
  //def combine[  T, T1 <: T, T2 <: T  ]( s: Seq[ Stream[ T1 ] ] )( implicit ev: T1 =:= T2 ): Stream[ Seq[ T ] ] = s.map( _.head ) #:: combine( s.map( _.tail ) )
 // def combine[  T, T1 <: T]( s: Seq[ Stream[ T1 ] ] )( implicit ev: T1 =:= T ): Stream[ Seq[ T ] ] = s.map( _.head ) #:: combine( s.map( _.tail ) )
  
  import implicitlogic._
  //def combine[T](s: Seq[Stream[T]])(implicit ev: Not[(AnyVal <:< T) Or (AnyRef <:< T)]): Stream[Seq[T]] = s.map( _.head ) #:: combine( s.map( _.tail ) )
  def combine[T](s: Seq[Stream[T]])(implicit ev: Not[(AnyVal <:< T) Or (AnyRef <:< T)]): Stream[Seq[T]] = s.map( _.head ) #:: combine( s.map( _.tail ) )
  
  
  // http://stackoverflow.com/questions/34118720/how-should-an-invariant-list-be-implemented-in-scala
  // http://alvinalexander.com/scala/how-why-make-mutable-collections-invariant-in-scala
  // http://like-a-boss.net/2012/09/17/variance-in-scala.html
  // http://eed3si9n.com/stricter-scala-with-ynolub
  // http://www.lihaoyi.com/post/StrategicScalaStylePracticalTypeSafety.html
  
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
  def combine[ T, T1 <: T, T2 <: T ]( s1: Stream[ T1 ], s2: Stream[ T2 ] )( implicit ev: T1 =:= T2 ): Stream[ Seq[ T ] ] = combine( Array( s1, s2 ) )  
  
  import scala.reflect.ClassTag
  
  def ensureSingleType[T : ClassTag](c:Seq[T]) = {
    val tmp : T = c(0)
    tmp
  }
  
  // TODO: 
  def separate[T](s: Stream[ Seq[T] ], select: Seq[Int]) : Tuple2[ Option[Stream[Seq[T]]], Option[Stream[Seq[T]]] ] = ???
  
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

    val l14 =  List( s4, s5, s6 )
    val s14 = combine( l14 )
    println( s14.take( 10 ).toList.map( a => a.mkString( "<", ",", ">" ) ).mkString( "{", ",", "}" ) )

    val s15 = Stream.from( 1 )
    val s16 = combine( List( s15, s1, s3 ) )
    println( s16.take( 10 ).toList.map( a => a.mkString( "<", ",", ">" ) ).mkString( "{", ",", "}" ) )
    
    // TODO: not safe, how do we avoid AnyVal?
    // http://stackoverflow.com/questions/40677849/enforce-scala-seq-to-use-a-single-tpe-only-no-lub
    //val l17 = List( s4, s5)  // Ok
    //val l17 = List( s4, s1)  // failed as required
    //val l17 = List( s4, s1, s3 ) // failed as required
    //val l17 = List( s4, s1, s2 ) // failed as required
    //val l17 = List( s4, s1, s7 ) // failed as required
    //val l17 = List[Double]( s4, s1, s8 ) // equivalent
    /*val l17 = List( s4, s1, s8 )
    val s17 = combine( l17 )
    println( s17.take( 10 ).toList.map( a => a.mkString( "<", ",", ">" ) ).mkString( "{", ",", "}" ) )*/

    val s18 = linspace( 0.0, 1.0, 10 )
    println( s18.take( 12 ).toList.mkString( "{", ",", "}" ) )

    val s19 = linspace( 0.0, 1 / 9.0 )
    println( s19.take( 12 ).toList.mkString( "{", ",", "}" ) )

    val w0 =  Map(0.1 -> "A", (0.3 ->  "B"), (0.6 ->  "C"))
    val w1 = new RandomCollection[String]( r, w0 )
    println("----------------------")
    println(w1.ceiling(0.0))
    println(w1.ceiling(0.01))
    println(w1.ceiling(0.09999999))
    println(w1.ceiling(0.1))
    
    println(w1.ceiling(0.1000001))
    println(w1.ceiling(0.2))
    println(w1.ceiling(0.4))
    
    println(w1.ceiling(0.400000001))
    println(w1.ceiling(0.5))
    println(w1.ceiling(1.0))

    val s200 = discreteMultiModal(r, w0)
    val as = s200.take(100).count { x => x == "A" }
    println(s"#A's = ${as}")
    val bs = s200.take(100).count { x => x == "B" }
    println(s"#B's = ${bs}")
    val cs = s200.take(100).count { x => x == "C" }
    println(s"#C's = ${cs}")
    
    // append streams (note the take)
    val s20 = s15.take( 5 ) #::: Stream.from( 2, 2 )
    println( s20.take( 10 ).toList.mkString( "{", ",", "}" ) )

    // append streams (note the take)
    val s21 = s19.take( 15 ) append Stream.from( 1, -1 )
    println( s21.take( 20 ).toList.mkString( "{", ",", "}" ) )

    // append combined streams (note the take)
    val s22 = combine( List( s4, s5 ) ).take( 5 ) #::: combine( List( s5, s6 ) )
    println( s22.take( 10 ).toList.map( a => a.mkString( "<", ",", ">" ) ).mkString( "{", ",", "}" ) )

    // Issues with invariance
    /*
    val t = Set[Double](1,2, 3.0, "Hello")
    val t0 : List[Any] = List(1,2, 3.0, "Hello")
    val t1 : List[Any] = List[Int](1,2, 3.0, "Hello")
    val t2 : Array[Any] = Array(1,2, 3.0, "Hello")
    val t4 : Array[Any] = Array[Int](1,2, 3.0, "Hello")
    val t5 : Seq[Double] = List(1,2, 3.0, "Hello")
    val t6 = scala.collection.mutable.ArrayBuffer(1,2, 3.0, "Hello")
    val t7 = ensureSingleType(List(1,2,""))
    println( t.toStream.take( 12 ).toList.mkString( "{", ",", "}" ) )
    */
  }

}
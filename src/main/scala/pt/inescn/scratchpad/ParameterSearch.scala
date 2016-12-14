package pt.inescn.scratchpad

trait Parameter {}

class learningRate(val value : Double) extends Parameter
class pValue(val value : Double) extends Parameter

import scala.language.higherKinds

//import pt.inescn.utils.HList._
  import pt.inescn.utils.HNil
  import pt.inescn.utils.HList.{:: => #:}  // rename the type for compatibility
  import pt.inescn.utils.HList

trait Model[T,U[T], P] {
  //type params = HList
  //val params : List[Parameter]
  val params : HList
  
  def fit(data : U[T]) : Unit
  def predict(datum : T) : P
}

object ModelAParams {
  type  w = learningRate #: pValue #: HNil
}

class ModelA[T,U[T]](val params: ModelAParams.w) extends Model[T,U,Double] {
  import pt.inescn.utils.HList._
  import pt.inescn.utils.HNil
  
  def x = HList.HNil
  def y = HNil
  def z = pt.inescn.utils.HList.HNil
  
  def fit(data : U[T]) : Unit = {}
  def predict(datum : T) : Double = {0.0}
}



//import scala.collection.parallel.ParIterableLike

trait ParameterSearch {}

/**
 * 
 * https://amplab.cs.berkeley.edu/wp-content/uploads/2015/07/163-sparks.pdf
 * 
 * https://softwaremill.com/comparing-akka-stream-scalaz-stream/
 * https://github.com/scala-blitz/scala-blitz/issues/47
 * http://stackoverflow.com/questions/17178201/parallel-iterator-in-scala
 * https://github.com/fsist/future-streams
 * https://github.com/gyeh/lazy-parallel-streams/blob/master/src/main/scala/LazyStreams.scala
 * http://grokbase.com/t/gg/scala-user/12bx1gp61a/traversing-iterator-elements-in-parallel
 *
 * http://stackoverflow.com/questions/18043749/mapping-a-stream-with-a-function-returning-a-future
 *    val result = Future.Traverse(x, toFutureString)
 */

/*
 * http://stackoverflow.com/questions/13483931/functional-style-early-exit-from-depth-first-recursion
 */

/**
 * Generate cross product.
 * Cartesian product of arbitrary lists.
 *
 * http://stackoverflow.com/questions/8321906/lazy-cartesian-product-of-several-seqs-in-scala
 * http://stackoverflow.com/questions/19382978/cartesian-product-stream-scala
 */
object GridPramaterSearch extends ParameterSearch {

  def cartesian[ A ]( list: List[ List[ A ] ] ): List[ List[ A ] ] = {
    list match {
      case Nil    => List( List() )
      case h :: t => h.flatMap( i => cartesian( t ).map( i :: _ ) )
    }
  }

  def cartesian0[ A ]( list: List[ List[ A ] ] ): List[ List[ A ] ] = {
    list match {
      case Nil    => List( List() )
      case h :: t => h.flatMap( i => cartesian( t ).map( i :: _ ) )
    }
  }

  def cartesian2[ A, B ]( list: Seq[ Seq[ A ] ], acc: Seq[ A ], f: Seq[ A ] => B ): Unit = {
    list match {
      case Nil    => f( acc )
      case h :: t => h.map { x => cartesian2( t, x +: acc, f ) }
    }
  }

  def cartesian3[ A, B ]( list: Seq[ Seq[ A ] ], acc: Seq[ A ], f: Seq[ A ] => B ): Seq[ B ] = {
    list match {
      case Nil    => List( f( acc ) )
      case h :: t => h.flatMap { x => cartesian3( t, x +: acc, f ) }
    }
  }

  import annotation.tailrec

  /**
   * @see http://blog.dmitryleskov.com/programming/scala/stream-hygiene-i-avoiding-memory-leaks/
   * @see http://stackoverflow.com/questions/21141853/scala-streams-how-to-avoid-to-keeping-a-reference-to-the-head-and-other-elemen
   */
  //@tailrec 
  def cartesian4[ A, B ]( list: Stream[ Seq[ A ] ], acc: Seq[ A ], f: Seq[ A ] => B ): Stream[ B ] = {
    list match {
      case Stream.Empty => List( f( acc ) ).toStream
      case h #:: t      => h.toStream.flatMap { x => cartesian4( t, x +: acc, f ) }
    }
  }

  /**
   * Avoiding out of memory errors
   * 1.  Stream parameter is used by-name
   * 2. Do NOT hold the return as a val
   *
   * @see http://blog.dmitryleskov.com/programming/scala/stream-hygiene-i-avoiding-memory-leaks/
   * @see http://blog.dmitryleskov.com/programming/scala/stream-hygiene-ii-hotspot-kicks-in/
   * @see https://github.com/stew/dogs/blob/master/core/src/main/scala/dogs/Streaming.scala
   * @see http://stackoverflow.com/questions/21141853/scala-streams-how-to-avoid-to-keeping-a-reference-to-the-head-and-other-elemen
   */
  def cartesian5[ A, B ]( list: => Stream[ Seq[ A ] ], f: Seq[ A ] => B ): Stream[ B ] = {
    def loop( lst: => Stream[ Seq[ A ] ], acc: Seq[ A ] ): Stream[ B ] = {
      lst match {
        case Stream.Empty => List( f( acc ) ).toStream
        case h #:: t      => h.toStream.flatMap { x => loop( t, x +: acc ) }
      }
    }
    loop( list, List() )
  }

  /*@tailrec
  def cartesian6[ A, B ]( list: => Stream[ Seq[ A ] ], f: Seq[ A ] => B ): Stream[ B ] = {
    def loop( lst: => Stream[ Seq[ A ] ], acc: Seq[ A ] ): Stream[ B ]  = {
      lst match {
        case Stream.Empty => List( f( acc ) ).toStream
        case h #:: t      => h.toStream.flatMap { x => loop( t, x +: acc ) }
      }
    }
    loop( list, List() )
  }*/

  /**
   * Example of a tail recursive call
   * @see http://stackoverflow.com/questions/10290189/how-to-make-this-recursive-method-tail-recursive-in-scala
   */
  def product[ T ]( listOfLists: List[ List[ T ] ] ): List[ List[ T ] ] = {
    @tailrec def innerProduct( listOfLists: List[ List[ T ] ], accum: List[ List[ T ] ] ): List[ List[ T ] ] =
      listOfLists match {
        case Nil       => accum
        case xs :: xss => innerProduct( xss, for ( y <- xs; a <- accum ) yield y :: a )
      }

    innerProduct( listOfLists.reverse, List( Nil ) )
  }

}

/**
 *
 * sbt "run-main pt.inescn.scratchpad.SearchParameters"
 */
object SearchParameters {

  def main( args: Array[ String ] ) {

    val l = List( List( 1, 2, 3 ), List( 4, 5, 6 ) )
    val c1 = GridPramaterSearch.cartesian( l )
    c1.foreach { x => println( x.mkString( "<", ",", ">" ) ) }

    val c2 = GridPramaterSearch.cartesian0( l )
    //c2.foreach { x => println( x.mkString("<", ",", ">") ) }

    GridPramaterSearch.cartesian2( l, List(), { x: Seq[ Int ] => println( x.mkString( "<", ",", ">" ) ) } )

    val c3 = GridPramaterSearch.cartesian3( l, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c3.foreach { println }

    val c4 = GridPramaterSearch.cartesian4( l.toStream, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c4.foreach { println }

    // Cartesian 3 while constructing the list - java.lang.OutOfMemoryError: GC overhead limit exceeded (slow due to memoization)
    // Cartesian 3 printing the list is ok, but memoization causes problems - java.lang.OutOfMemoryError: GC overhead limit exceeded 
    // Depth of the search is the same as the the number of lists to process - so this is not an issue
    //val l1 = List( 1 to 250, 1 to 250, 1 to 250 )
    // -----> val l1 = List( 1 to 2500, 1 to 2500, 1 to 2500 )
    val l1 = List( 1 to 3, 1 to 3, 1 to 3 )
    //val c5 = GridPramaterSearch.cartesian3( l1, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    //val c5 = GridPramaterSearch.cartesian4(l1.toStream, List(), { x: Seq[Int] => x.mkString("<", ",", ">")  } )
    def c5 = GridPramaterSearch.cartesian4( l1.toStream, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c5.foreach { println }

    // http://docs.scala-lang.org/overviews/core/futures.html
    // http://blog.jessitron.com/2014/02/scala-global-executioncontext-makes.html
    // http://blog.jessitron.com/2014/01/choosing-executorservice.html

    // http://alvinalexander.com/scala/concurrency-with-scala-futures-tutorials-examples
    import scala.concurrent.duration._
    import scala.concurrent.{ Await, Future, future }
    import scala.concurrent.forkjoin._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.language.postfixOps
    import scala.util.{ Try, Success, Failure }
    import scala.util.Random
    /*
    // used by 'time' method
    //implicit val baseTime = System.currentTimeMillis

    val f = Future {
      Thread.sleep( 500 )
      1 + 1
    }

    println( "Example 1: start" )
    val result = Await.result( f, 1 second )
    println( result )
    Thread.sleep( 1000 )
    println( "Example 1: end" )

    println( "Example 2: start" )
    val f1 = Future {
      Thread.sleep( Random.nextInt( 500 ) )
      42
    }
    f1.onComplete {
      case Success( value ) => println( s"Got the callback, meaning = $value" )
      case Failure( e )     => e.printStackTrace
    }
    // do the rest of your work
    println( "A ..." ); Thread.sleep( 100 )
    println( "B ..." ); Thread.sleep( 100 )
    println( "C ..." ); Thread.sleep( 100 )
    println( "D ..." ); Thread.sleep( 100 )
    println( "E ..." ); Thread.sleep( 100 )
    println( "F ..." ); Thread.sleep( 100 )
    println( "G ..." ); Thread.sleep( 100 )
    println( "H ..." ); Thread.sleep( 100 )
    Thread.sleep( 2000 )
    println( "Example 2: end" )

    println( "Example 3: start" )
    val f2 = Future {
      Thread.sleep( Random.nextInt( 500 ) )
      if ( Random.nextInt( 500 ) > 250 ) throw new Exception( "Yikes!" ) else 42
    }
    //   @deprecated("use `foreach` or `onComplete` instead (keep in mind that they take total rather than partial functions)", "2.12.0")
    f2 onSuccess {
      case result => println( s"Success: $result" )
    }
    // @deprecated("use `foreach` or `onComplete` instead (keep in mind that they take total rather than partial functions)", "2.12.0")
    f2 onFailure {
      case t => println( s"Exception: ${t.getMessage}" )
    }
    // do the rest of your work
    println( "A ..." ); Thread.sleep( 100 )
    println( "B ..." ); Thread.sleep( 100 )
    println( "C ..." ); Thread.sleep( 100 )
    println( "D ..." ); Thread.sleep( 100 )
    println( "E ..." ); Thread.sleep( 100 )
    println( "F ..." ); Thread.sleep( 100 )
    println( "G ..." ); Thread.sleep( 100 )
    println( "H ..." ); Thread.sleep( 100 )
    Thread.sleep( 2000 )
    println( "Example 3: end" )

    // @deprecated("use `Future { ... }` instead", "2.11.0")
    def longRunningComputation( i: Int ): Future[ Int ] = future {
      Thread.sleep( 100 )
      i + 1
    }

    // see also recover, recoverWith, fallbackTo and andThen
    
    // http://danielwestheide.com/blog/2013/01/09/the-neophytes-guide-to-scala-part-8-welcome-to-the-future.html
*/
    // http://stackoverflow.com/questions/18043749/mapping-a-stream-with-a-function-returning-a-future
    // val x = (1 until 10).toStream
    // def toFutureString(value : Integer) = Future(value toString)

    //val x = ( 1 to 10 ).toList
    val x = ( 1 to 100 ).toStream
    def toFutureString( value: Int ) = Future {
      println( "starting " + value )
      //Thread.sleep( 100 )
      ( 1 to 1500000 ).foreach( x => math.log(x * 2 / 3) )
      println( "completed " + value )
      value.toString
    }

    // If its a def and never use, nothing runs
    // If it a val, then it is used. Note that val stream means memoization is used
    def r = Future.traverse( x )( toFutureString )
    println( "Returned from traverse" )

    // Irrespective of val or def, we get the result 
    // But we only get it when all futures have been processed
    Await.ready( r, 1000 second )
    println( "Returned from Await.ready" )

    // http://stackoverflow.com/questions/18163656/traversing-lists-and-streams-with-a-function-returning-a-future
    // https://github.com/dnvriend/akka-concurrency-test
    // http://blog.quantifind.com/instantiations-of-scala-futures
    /* We want to automatically introduce back-pressure and block the caller from creating and enqueueing more futures until we 
       * have enough capacity to do so.
       * see http://grokbase.com/t/gg/scala-user/13ahck3d28/easy-parallel-mapping-an-iterator
       *      1.  `iterator.grouped(batchSize).par`.
       */

    // Very long list. All CPUs seem to be occupied but we end with a java.lang.OutOfMemoryError 
    // The OOM error occurs due to the Future.traverse that holds a value to the full stream
    // When this occurs, it also seems like the java threads become zombies 
    // Use top -M to see this in action
    /*
    def  y = ( 1 to 1000000000 ).toStream.grouped( 10 ).toStream.flatMap { x => x }
    def s = Future.traverse( y )( toFutureString )
    println( "Returned from traverse" )
    Await.ready( s, 1000 days ) // Duration.Inf    
    println( "Returned from Await.ready" )*/

    // Could only see about 60% CPU usage (grouped by 10 for 8 threads). To get to this point I had to remove the sleep and use tight loops
    // It may be that the grouping is delaying the push of the Futures onto the thread pool and hence easing off on the CPUs
    // By increasing the group size we could apply more back pressure. However, with 128 requests we maxed at 85%. 
    // This means that the Await.ready may be waiting on threads that have yet to finish in a given group. 
    // TODO: consider other Future or  Await calls
    def  z = ( 1 to 1000000000 ).grouped( 128 )
    z.foreach { x => 
      println( "Before group traverse" )
      def s = Future.traverse( x )( toFutureString )
      Await.ready( s, Duration.Inf  )
      println( "After group traverse" )
    }
        
  }
}
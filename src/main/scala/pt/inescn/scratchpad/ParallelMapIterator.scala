package pt.inescn.scratchpad

import java.util.concurrent.ThreadPoolExecutor

// Computation
// Computation: start additional tasks recursively
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.Executors

import java.util.concurrent.TimeUnit
import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent._
import scala.concurrent.duration.Duration

/**
 * Ryan LeCompte
 * https://github.com/ryanlecompte
 * lecompte@gmail.com
 * http://twitter.com/ryanlecompte
 *
 * http://blog.jessitron.com/2014/01/choosing-executorservice.html
 *  . https://gist.github.com/jessitron/60d0c1792e0c42b12533
 * http://blog.jessitron.com/2014/02/scala-global-executioncontext-makes.html
 * http://blog.jessitron.com/2014/01/fun-with-pool-induced-deadlock.html
 * https://gist.github.com/jessitron/8777503
 *  . illustrates using blocking { ... } to get the global ExecutionContext to start extra threads
 * https://examples.javacodegeeks.com/core-java/util/concurrent/rejectedexecutionexception/java-util-concurrent-rejectedexecutionexception-how-to-solve-rejectedexecutionexception/
 *
 * http://stackoverflow.com/questions/26079987/limiting-the-q-size-on-javas-1-7-forkjoinpool
 * http://stackoverflow.com/questions/29962437/how-to-block-a-queue-in-forkjoinpool
 *
 * sbt "run-main pt.inescn.scratchpad.ParallelMapIterator"
 *  top -H
 */
object ParallelMapIterator {
  val numThread = sys.runtime.availableProcessors

  // https://examples.javacodegeeks.com/core-java/util/concurrent/rejectedexecutionexception/java-util-concurrent-rejectedexecutionexception-how-to-solve-rejectedexecutionexception/
  import java.util.concurrent.ExecutorService
  import java.util.concurrent.ArrayBlockingQueue

  /*
  val executor1: ExecutorService = new ThreadPoolExecutor( 3, 3, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue( numThread ) )
  // Computation when starting computations recursively. Must use blocking when doing I/O
  val executor2 = ForkJoinPool.commonPool()
  // Computation without starting computations recursively
  // Block tasks when no more threads available but queue keeps growing
  val executer3 = java.util.concurrent.Executors.newFixedThreadPool( numThread )
  // I/O without throttling but won't fail. Grow and shrinks pool 
  val executer4 = java.util.concurrent.Executors.newCachedThreadPool()
  // I/O without throttling and will fail if not enough threads available (for IO use limited number of threads)
  val executer5 = java.util.concurrent.Executors.newFixedThreadPool( numThread )
  // Execute after a delay or periodically
  val executer6 = java.util.concurrent.Executors.newScheduledThreadPool( numThread )
  val executer7 = java.util.concurrent.Executors.newSingleThreadExecutor()
  val executer8 = java.util.concurrent.Executors.newSingleThreadScheduledExecutor()
  // Number of threads may grow or shrink
  val executer9 = java.util.concurrent.Executors.newWorkStealingPool( numThread )
 */
  implicit val context = ExecutionContext.fromExecutorService(
    new ThreadPoolExecutor(
      numThread, numThread,
      0L, TimeUnit.SECONDS,
      new ArrayBlockingQueue[ Runnable ]( numThread ) {
        override def offer( e: Runnable ) = {
          put( e ); // Waiting for empty room
          true
        }
      })
     )

  def timeKiller( i: Int ) = {
    //( 1 to 10000 ).foreach( _ * 2 )
    ( 1 to 1500000 ).foreach( x => math.log( x * 2 / 3 ) )
    i.toLong
  }

  def reduce( a: Future[ Long ], b: Future[ Long ] ) = {
    a.flatMap( v => b.map( v + _ ) )
  }

  import scala.util.{ Try, Success, Failure }
  import scala.util.Random

  def longComputation() = {
    val id = Thread.currentThread().getId
    //blocking {
      println( s"Started thread: $id" )
      Thread.sleep( 500 )
      println( s"Finished thread: $id" )
    //}
    id
  }

  /**
   * In the event that some of the callbacks never complete (e.g. the callback contains an infinite loop), the other callbacks may 
   * not be executed at all. In these cases, a potentially blocking callback must use the blocking construct.
   */
  def processResult[T](r : Try[T]) = {
    blocking {
       println("???????????")
        r match {
          case Success( id ) => println( s"Thread result: $id" )
          case Failure( t )  => println( "An error has occured: " + t.getMessage )
         }
    }
  }

  val singleThreadContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

  def main( args: Array[ String ] ) {

    /*
    // Will start threads and wait on all of them in the same order
    val sum = ( 1 to 100000 ).toIterator.map( i => Future( timeKiller( i ) ) ).reduce( reduce )

    println( Await.result( sum, Duration.Inf ) )
    */

    val s = Stream.from( 0 )
    //s.foreach { x => println(x) ;  val f = Future( longComputation ) ; f.onComplete{ processResult } }
    /*s.foreach { x => 
      println(x) 
      val f = Future( longComputation )  
      val p = Promise[Long]()
      p completeWith f
      p.future.onComplete{ processResult } 
    }*/
    
    s.foreach { x => 
      println(x) 
      //val f = Future( longComputation )  
      val f = Future( timeKiller(x) )  
      f.onComplete { processResult } (singleThreadContext) 
    }
    
    println("Finished")
    //context.shutdown
  }

}
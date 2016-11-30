package pt.inescn.scratchpad

import java.util.concurrent.ThreadPoolExecutor
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
 * sbt "run-main pt.inescn.scratchpad.ParallelMapIterator"
 */
object ParallelMapIterator {
  val numThread = 5

  // https://examples.javacodegeeks.com/core-java/util/concurrent/rejectedexecutionexception/java-util-concurrent-rejectedexecutionexception-how-to-solve-rejectedexecutionexception/
  import java.util.concurrent.ExecutorService 
 import java.util.concurrent.ArrayBlockingQueue
  	
  val executor : ExecutorService = new ThreadPoolExecutor(3, 3, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue(15));
  
  implicit val context = ExecutionContext.fromExecutorService(
      //executor
      
    new ThreadPoolExecutor(
      numThread, numThread,
      0L, TimeUnit.SECONDS,
      new ArrayBlockingQueue[ Runnable ]( 5 ) {
        override def offer( e: Runnable ) = {
          put( e ); // Waiting for empty room
          true
        }
      } )
      )

  def timeKiller( i: Int ) = {
    //( 1 to 10000 ).foreach( _ * 2 )
    ( 1 to 1500000 ).foreach( x => math.log(x * 2 / 3) )
    i.toLong
  }

  def reduce( a: Future[ Long ], b: Future[ Long ] ) = {
    a.flatMap( v => b.map( v + _ ) )
  }

  def main( args: Array[ String ] ) {
    
    val sum = ( 1 to 100000 ).toIterator.map( i =>
      Future( timeKiller( i ) ) ).reduce( reduce )

    println( Await.result( sum, Duration.Inf ) )
    context.shutdown
  }


}
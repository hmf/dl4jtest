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
 * sbt "run-main pt.inescn.scratchpad.ThrottledParallelTasks"
 *  top -H
 */
object ThrottledParallelTasks {
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
      } ) )

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

  /**
   * The println does not show in the console.
   * However, the callback prints work correctly.
   */
  def longComputation() = {
    val id = Thread.currentThread().getId
    //blocking {
    println( s"Started thread: $id ***************" )
    Thread.sleep( 500 )
    println( s"Finished thread: $id ***************" )
    //}
    id
  }

  /**
   * In the event that some of the callbacks never complete (e.g. the callback contains an infinite loop), the other callbacks may
   * not be executed at all. In these cases, a potentially blocking callback must use the blocking construct.
   */
  def processResult[ T ]( r: Try[ T ] ) = {
    //blocking {
    r match {
      case Success( id ) => println( s"Thread result: $id" )
      case Failure( t )  => println( "An error has occured: " + t.getMessage )
    }
    //}
  }

  //val singleThreadContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
  val singleThreadContext = ExecutionContext.fromExecutorService( java.util.concurrent.Executors.newSingleThreadExecutor() )

  // http://rapture.io/
  // https://github.com/scala-incubator/scala-io
  // https://github.com/jesseeichar/scala-io
  // https://github.com/pathikrit/better-files
  // https://github.com/jsuereth/scala-arm
  // http://langref.org/scala/parallel/threads/create-readwrite-lock-on-a-shared-resource
  // http://stackoverflow.com/questions/2885173/how-to-create-a-file-and-write-to-a-file-in-java
  // http://docs.oracle.com/javase/tutorial/essential/io/file.html
  // "./output/bimiodal_1"
  //import scala.io.Source
  import java.io._
  import scala.util.{ Try, Success, Failure }

  val filename = " ./output/results.txt"
  //val o = Source.fromFile( filename )  // reading only
  //val writer = new PrintWriter(new File(filename)) // must exist
  try {
    val writer = new PrintWriter( "./output/test.txt", "UTF-8" ) // creates
    writer.write( "Hello" )
    writer.close
  } catch {
    case ioe: IOException => ()
    //case e: Exception => less specific after
  }

  //val writer : Writer = new BufferedWriter(new OutputStreamWriter( new FileOutputStream("filename.txt"), "utf-8"))
  // writer.write("something");

  import java.nio.charset.StandardCharsets

  val f = Try {
    val writer = new PrintWriter( "./output/test2.txt", StandardCharsets.UTF_8.name() ) // creates
    writer.write( "Hello\n" )
    writer.write( "1\n" )
    writer.append( "Two\n" )
    writer.flush()
    writer.close
  }
  println( f )

  val f1 = Try {
    //val writer = new PrintWriter( "./output/test2.txt", StandardCharsets.UTF_8.name()) // creates
    //val writer : PrintWriter = new PrintWriter(new FileOutputStream( new File("./output/test2.txt"), true /* append = true */));     
    val writer: PrintWriter = new PrintWriter( new BufferedWriter( new FileWriter( "./output/test2.txt", true ) ) )
    writer.println( "4" )
    writer.flush()
    writer.close
  }
  println( f1 )

  // http://stackoverflow.com/questions/3893274/how-to-mix-in-a-trait-to-instance
  // http://stackoverflow.com/questions/10373318/mixing-in-a-trait-dynamically
  // http://alvinalexander.com/scala/how-to-dynamically-add-scala-trait-to-object-instance
  // http://www.cakesolutions.net/teamblogs/ways-to-pattern-match-generic-types-in-scala
  trait Managed {
    def close() : Unit
    def flush() : Unit
    def checkError(): Boolean
  }

  /**
   * Use the loan pattern to ensure that we write stuff and then flush the file
   * The PrintWriter does throw exceptions and fails silently. Check for errors manually. 
   *  
   * @see   http://stackoverflow.com/questions/297303/printwriter-and-printstream-never-throw-ioexceptions
   */
  def usingThenFlush[ T <: Managed, U  ]( resource: T )( block: T => U) : Try[U] = {
      Try{
          val r = block( resource)
          resource.flush
          if (resource.checkError())  throw new IOException("Flush failed") else r
      }
  }

  /**
   * Use the loan pattern to ensure that we write stuff and then close the file
   * The PrintWriter does throw exceptions and fails silently. Check for errors manually. 
   *  
   * @see   http://stackoverflow.com/questions/297303/printwriter-and-printstream-never-throw-ioexceptions
   */
  def usingThenClose[ T <: Managed, U ]( resource: T )( block: T => U) : Try[U] = {
    Try {
      val r = block(resource)
      resource.close
      if (resource.checkError())  throw new IOException("Flush failed") else r
    }
  }

  import java.time.LocalDateTime;
  import java.time.Period;
  import java.time.ZoneId;
  import java.time.ZonedDateTime;

  // http://stackoverflow.com/questions/2207425/what-automatic-resource-management-alternatives-exist-for-scala
  def resultLogger(data : String)(writer: PrintWriter) = {
    val d = LocalDateTime.now
    val z = ZoneId.systemDefault() // ZoneId.of( "Europe/Lisbon")
    writer.println( s"$data: $d $z" )
  }

  def getFlushingLogger = {
    
  }
  
  def main( args: Array[ String ] ) {

    val fileWithPath = "./output/test2.txt"
    val append = true
    //val writer = new PrintWriter( new BufferedWriter( new FileWriter( fileWithPath, append ) ) ) with Managed
    val writer = new PrintWriter( new BufferedWriter( new FileWriter( fileWithPath, append ) ), true ) with Managed
   
    //using(writer) (resultLogger _) 
    val r1 = usingThenClose(writer) (resultLogger("C") _) 
    println(s"Loan Close : $r1")
    val r2 = usingThenFlush(writer) (resultLogger("F") _) 
    println(s"Loan Flush : $r2")
    
    /*
    // Will start threads and wait on all of them in the same order
    val sum = ( 1 to 100000 ).toIterator.map( i => Future( timeKiller( i ) ) ).reduce( reduce )

    println( Await.result( sum, Duration.Inf ) )
    */

    // Here we use throttling to avoid OOM errros. However the use of the callbacks to 
    // process the results causes a deadlock. This happens because all Futures are started
    // filling up the queue annd hence blocking n a new Future creation. Each of the queue
    // Futures executes the main task to the end. However the thread is not released until 
    // the future adds its own task that executes the callback. Because the queue is already full
    // the request for a callback will also block. So now none of the main tasks Futures can complete
    // and therefore release a queue entry
    // The use of blocking or Promises will not solve the issue
    // Solution:  
    // @see http://stackoverflow.com/questions/40981240/throttling-scala-future-blocks-when-oncomplete-is-used/40982776
    //val s = Stream.from( 0 )
    val s = ( 0 to 25 ).toStream
    //val s = ( 0 to 1000000 ).toStream
    // Deadlock
    //s.foreach { x => println(x) ;  val f = Future( longComputation ) ; f.onComplete{ processResult } }
    // Deadlock
    /*s.foreach { x => 
      println(x) 
      val f = Future( longComputation )  
      val p = Promise[Long]()
      p completeWith f
      p.future.onComplete{ processResult } 
    }*/

    // Works
    s.foreach { x =>
      println( s"Launhed $x" )
      //val f = Future( longComputation )  
      val f = Future( timeKiller( x ) )
      f.onComplete { processResult } ( singleThreadContext )
    }

    // see http://stackoverflow.com/questions/18425026/shutdown-and-awaittermination-which-first-call-have-any-difference
    println( "Finished" )
    // Does an orderly "shutdown". It will wait for all tasks on the queue to complete
    // If new tasks are placed on the queue, these will be rejected causing exceptions
    // Use a timed-out termination to avoid task execution loss (pending call-backs)
    // Don't accept more mains tasks
    context.shutdown
    // Wait for pending main tasks
    context.awaitTermination( 100, TimeUnit.DAYS )
    // If all the contexts are not shutdown then the man thread will remain active
    // All main tasks executed, so all callbacks already requested
    // No more callbacks
    singleThreadContext.shutdown
    // Wait until they finish
    singleThreadContext.awaitTermination( 100, TimeUnit.DAYS )
  }

}
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

import java.util.concurrent.RejectedExecutionHandler

/**
 * When a thread pool bounded queue is full, we simply terminate the application.
 */
object ExitAppRejectedExecutionHandler extends RejectedExecutionHandler {
  @Override
  def rejectedExecution( worker: Runnable, executor: ThreadPoolExecutor ) = {
    System.out.println( worker.toString() + " is Rejected" );
    System.exit( 1 )
  }
}

/**
 * Ryan LeCompte
 * https://github.com/ryanlecompte
 * lecompte@gmail.com
 * http://twitter.com/ryanlecompte
 *
 * http://www.cakesolutions.net/teamblogs/demystifying-the-blocking-construct-in-scala-futures
 * "This means that out of the box the blocking {} constructs only has an effect if you use the global execution context, nothing more"
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

  val numIOThreads = 2
  val numThread = sys.runtime.availableProcessors - numIOThreads

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
   * We have to flush in order to see them.
   * Don't block so that OOM can be easily shown.
   * This blocking reduced the number of parallel tasks
   * created by the global (ForkJoin) pool.
   *
   * @see http://www.cakesolutions.net/teamblogs/demystifying-the-blocking-construct-in-scala-futures
   */
  def longComputation( timeMS: Long = 500 )() = {
    val id = Thread.currentThread().getId
    //blocking {
    //println( s"Started thread: $id ***************" )
    Thread.sleep( timeMS )
    //println( s"Finished thread: $id ***************" )
    System.out.flush()
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

  ////////////////////////////////////////////////////
  import java.time.LocalDateTime;
  import java.time.ZoneId;
  import java.time.ZonedDateTime;

  val zone = ZoneId.systemDefault() // ZoneId.of( "Europe/Lisbon")

  /**
   * Example of formating the results to the logger. The input
   * should have the same type the tasks will produce. Make sure
   * it is only one parameter.
   */
  def formatCSV( id: Long ) = {
    val d = LocalDateTime.now
    s"$id; $d; $zone"
  }

  /**
   * A task may succeed or fail. If it succeeds, then write to the
   */
  def logResult[ T ]( format: T => String )( r: Try[ T ] ) = {
    r match {
      case Success( id ) => format( id )
      case Failure( t )  => "ERROR; " + t.getMessage
    }
  }

  def resultLogger[ T ]( logResult: T => String )( writer: PrintWriter )( data: T ) = {
    blocking {
      println(s"Finished: $data")
      writer.println( logResult( data ) )
    }
  }

  /*
   * Basic Future experiments
   */

  def time[ R ]( block: => R ): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    val diff = ( t1 - t0 ) / 10.0e6
    println( "Elapsed time: " + diff + "ms" )
    result
  }

  /**
   * Will start threads and wait on all of them in the same order.
   * This mans that a task that takes very long will not allow us
   * to get and process results of tasks that have already executed
   * asynchronously.
   *
   * NOTE: If we create  a very large amount of tasks we may either have
   * an Out-of-memory error (OOM) because the task results are stored. Or
   * we may have exceptions due to unavailable threads or OOM because
   * the execution context service either respectively grow the queue
   * or fail for a fixed size queue. Usually the execution context is the problem.
   *
   * We need to throttle the launching of new tasks.
   *
   * IMPORTANT: this example will NOT fail because we are performing lazy
   * consumption of the `Future`s. It will reduce the range by pairs with success.
   * We get about 90% CPU usage.
   */
  def experiment_0( numTasks: Int ) = {
    val id = 0
    println( s"Started experiment $id" )
    val sum = ( 1 to numTasks ).toIterator.map( i => Future( timeKiller( i ) ) ).reduce( reduce )
    println( Await.result( sum, Duration.Inf ) )
    println( s"Finished experiment $id" )
  }

  /**
   * Here we need to use a CPU intensive task consume more CPU.  The use of `longComputation` that uses `sleep`
   * will produce about 4% CPU usage. With the `timeKiller` we get about 85% CPU usage.
   *
   * Note: ForkJoin grows its queue indefinitely. If we keep on adding tasks we end up with an out-of-memory error.
   * In order to "force" this we must nake sure tha the processes are long-running. This is why we set the wait for
   * 1500ms. Norte that if we used the `block` construct, the resuklt is different. In our tests we get :
   *
   * `java.lang.OutOfMemoryError: Java heap space
   * at java.util.concurrent.ForkJoinPool$WorkQueue.growArray(ForkJoinPool.java:886)`
   *
   * For a queue with 4194394 elements.
   *
   * @see http://www.cakesolutions.net/teamblogs/demystifying-the-blocking-construct-in-scala-futures
   */
  def experiment_1( numTasks: Int ) = {
    val id = 1
    println( s"Started experiment $id" )
    val s = ( 0 to numTasks ).toStream
    val executerl = ForkJoinPool.commonPool() // no limits on the queue size
    val contextl = ExecutionContext.fromExecutorService( executerl )
    s.foreach { x => println( x ); val f = Future( longComputation( 1500 ) )( contextl ) }
    //s.foreach { x => println( s"Created task: $x" ); val f = Future( timeKiller(x) )(contextl)  }
    println( s"Finished experiment $id" )
  }

  /**
   * Same as above only now we use a fixed thread pool. In this case we can exhaust
   * the available resources and task execution will fail (be rejected). We install
   * a custom `RejectedExecutionHandler` that simply exits the application.  Note
   * that Java already has several RejhectExecution handlers (see javadocs for more
   * information)
   *
   * @see https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/ThreadPoolExecutor.html
   * @see http://stackoverflow.com/questions/6306132/java-thread-pool-with-a-bounded-queue
   * @see http://stackoverflow.com/questions/27692420/use-custom-rejectedexecutionhandler
   * @see https://www.javacodegeeks.com/2013/01/java-thread-pool-example-using-executors-and-threadpoolexecutor.html
   * @see https://examples.javacodegeeks.com/core-java/util/concurrent/rejectedexecutionhandler/java-util-concurrent-rejectedexecutionhandler-example/
   */
  def experiment_2( numTasks: Int ) = {
    val id = 2
    println( s"Started experiment $id" )
    val s = ( 0 to numTasks ).toStream
    // Not Ok
    //val executorl = java.util.concurrent.Executors.newFixedThreadPool( 1 ) // unbound queue, keeps growing until we get OOM error
    // Ok
    //val executorl = new ThreadPoolExecutor( 3, 3, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue[Runnable]( 1 ), ExitAppRejectedExecutionHandler )
    // Ok
    val executorl = new ThreadPoolExecutor( 3, 3, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue( 1 ) )
    executorl.setRejectedExecutionHandler( ExitAppRejectedExecutionHandler );
    val contextl = ExecutionContext.fromExecutorService( executorl )
    s.foreach { x => println( x ); val f = Future( longComputation( 100 ) )( contextl ) }
    //s.foreach { x => println( s"Created task: $x" ); val f = Future( timeKiller(x) )(contextl)  }
    println( s"Finished experiment $id" )
  }

  /**
   * Here we use throttling to avoid OOM errors. However the use of the callbacks to
   * process the results causes a deadlock. This happens because all Futures are started
   * filling up the queue and hence blocking n a new Future creation. Each of the queue
   * Futures executes the main task to the end. However the thread is not released until
   * the future adds its own task that executes the callback. Because the queue is already full
   * the request for a callback will also block. So now none of the main tasks Futures can complete
   * and therefore release a queue entry.
   * The use of blocking or Promises will not solve the issue
   *
   * NOTE : do not execute otherwise the application will lock.
   * Solution:
   * @see http://stackoverflow.com/questions/40981240/throttling-scala-future-blocks-when-oncomplete-is-used/40982776
   */
  def experiment_3( numTasks: Int ) = {
    val id = 3
    println( s"Started experiment $id" )
    val s = ( 0 to numTasks ).toStream
    // Deadlock
    s.foreach { x => println( s"Created task: $x" ); val f = Future( longComputation() ); f.onComplete{ processResult } }
    println( s"Finished experiment $id" )
  }

  /**
   * Same as experiment 2. Deadlock will occur.
   */
  def experiment_4( numTasks: Int ) = {
    val id = 4
    println( s"Started experiment $id" )
    val s = ( 0 to numTasks ).toStream
    // Deadlock
    s.foreach { x =>
      println( s"Created task: $x" )
      val f = Future( longComputation() )
      val p = Promise[ Long ]()
      p completeWith f
      p.future.onComplete{ processResult }
    }
    println( s"Finished experiment $id" )
  }

  //val fixedThreadContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
  //val fixedThreadContext = ExecutionContext.fromExecutorService( java.util.concurrent.Executors.newSingleThreadExecutor() )
  val fixedThreadContext = ExecutionContext.fromExecutorService( java.util.concurrent.Executors.newFixedThreadPool( numIOThreads ) )
  // cachedthreadpool ?

  def experiment_5( numTasks: Int ) = {
    val id = 5
    println( s"Started experiment $id" )
    val s = ( 0 to numTasks ).toStream
    s.foreach { x =>
      println( s"Launhed $x" )
      //val f = Future( longComputation )  
      val f = Future( timeKiller( x ) )
      f.onComplete { processResult } ( fixedThreadContext )
    }
    println( s"Finished experiment $id" )
  }
  // CheckManaged

  def experiment_6( numTasks: Int ) = {
    val id = 6
    println( s"Started experiment $id" )

    import java.nio.charset.StandardCharsets
    val fixedThreadContextX = ExecutionContext.fromExecutorService( java.util.concurrent.Executors.newFixedThreadPool( numIOThreads ) )
    
    // Open and write a "header"
    val filename = " ./output/results.txt"
    //val o = Source.fromFile( filename )  // reading only
    //val writer = new PrintWriter(new File(filename)) // must exist
    val writerT = new PrintWriter( "./output/test1.txt", "UTF-8" ) // creates
    writerT.close
    val tmpWriter = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( "./output/test1.txt", true), StandardCharsets.UTF_8.name() ) )
    val writer = new PrintWriter( tmpWriter, true )
    
    try {
      writer.write( s"Started experiment $id\n" )
      def logger = resultLogger( logResult( formatCSV ) )( writer ) _
      val s = ( 0 to numTasks ).toStream
      s.foreach { x =>
        println( s"Launhed $x" )
        //val f = Future( longComputation )  
        val f = Future( timeKiller( x ) )
        f.onComplete { logger } ( fixedThreadContextX )
      }
    } catch {
      case ioe: IOException => println(ioe)
      //case e: Exception => less specific after
    } 
    finally {
    fixedThreadContextX.shutdown
    // Wait until they finish
    fixedThreadContextX.awaitTermination( 100, TimeUnit.DAYS )
      writer.close
    }

    println( s"Finished experiment $id" )
  }
  // CheckManaged

  def main( args: Array[ String ] ) {

    // Ok, synchronized
    time( experiment_0( 100 ) )
    // OOM, growing pool size - no onCompete
    // >= 4194394 with a wait of 1500ms
    //time( experiment_1( 100000000 ) )
    // Tasks rejected, fixed pool size- no onCompete
    //time( experiment_2( 100000000 ) )

    // Deadlock
    //time( experiment_3( 1000 ) )
    // Deadlock
    //time( experiment_4( 1000 ) )

    // Ok
    //time( experiment_5( 25 ) )

    // Ok
    time( experiment_6( 25 ) )
/*
    val bufferedSource = io.Source.fromFile( "./output/test1.txt" )
    /*val r0 = bufferedSource.getLines().toStream.map { line =>  line.split(";").map(_.trim) }
  println("A")
  r0.foreach{ x => println( x.mkString("<", "~", ">") ) }*/
    val r1 = bufferedSource.getLines().toStream.drop( 1 ).map { line => val r = line.split( ";" ).map( _.trim ); if ( r.size > 0 ) Some( r( 0 ) ) else None }
    /*
  val r0 = r1.sortWith{ case (Some(i), Some(j)) => i.toLong < j.toLong }
  println("A")
  r0.foreach{ x => println(x) }
    */
    val r2 = r1.sortWith{ case ( Some( i ), Some( j ) ) => i.toLong < j.toLong }.zipWithIndex
    println( "B" )
    r2.foreach{ x => println( x ) }
    val r3 = r2.forall{
      case ( Some( i ), j ) => i == j
      case ( None, _ )      => false
    }
    println( s"Experiment 6 test : $r3" )
    bufferedSource.close
*/
    Thread.sleep(10000)
    
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
    fixedThreadContext.shutdown
    // Wait until they finish
    fixedThreadContext.awaitTermination( 100, TimeUnit.DAYS )
  }

}
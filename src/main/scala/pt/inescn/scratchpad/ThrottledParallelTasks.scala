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
 * Note that if we don't shutdown the thread pool, the application will not terminate. 
 */
object ExitAppRejectedExecutionHandler extends RejectedExecutionHandler {
  @Override
  def rejectedExecution( worker: Runnable, executor: ThreadPoolExecutor ) = {
    System.out.println( worker.toString() + " is Rejected" );
    executor.shutdownNow()
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
  /*
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
  */
  def timeKiller( i: Int ) = {
    //( 1 to 10000 ).foreach( _ * 2 )
    ( 1 to 1500000 ).foreach( x => math.log( x * 2 / 3 ) )
    i.toLong
  }

  def reduce( context: ExecutionContext )( a: Future[ Long ], b: Future[ Long ] ) = {
    a.flatMap( v => b.map( v + _ )( context ) )( context )
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
      case Failure( t )  => "ERROR: " + t.getMessage
    }
  }

  def resultLogger[ T ]( logResult: T => String )( writer: PrintWriter )( data: T ) = {
    blocking {
      println( s"Finished: $data" )
      writer.println( logResult( data ) )
    }
  }

  /*
   * Basic Future tests
   */

  def time[ R ]( block: => R ): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    val diff = ( t1 - t0 ) / 10.0e6
    println( "Elapsed time: " + diff + "ms" )
    result
  }

  def throttlingPool(numThread: Int) = {
    val contextl = ExecutionContext.fromExecutorService(
      new ThreadPoolExecutor(
        numThread, numThread,
        0L, TimeUnit.SECONDS,
        new ArrayBlockingQueue[ Runnable ]( numThread ) {
          override def offer( e: Runnable ) = {
            put( e ); // Waiting for empty room
            true
          }
        } ) )

    contextl
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
   * NOTE 2: We need to explicitly shutdown the thread pool. If not, the process will not
   * terminate. In order to make the tests independent  we will pass the thread
   * pools. explicitly and not use it as an implicit.
   *
   * IMPORTANT: this example will NOT fail because we are performing lazy
   * consumption of the `Future`s. It will reduce the range by pairs with success.
   * We get about 90% CPU usage.
   */
  def test_0( numTasks: Int, numThreads: Int ) = {
    val id = 0
    val contextl = throttlingPool(numThreads)
    println( s"Started test $id" )
    val sum = ( 1 to numTasks ).toIterator.map( i => Future( timeKiller( i ) )( contextl ) ).reduce( reduce( contextl ) )
    println( Await.result( sum, Duration.Inf ) )
    // don't accept any more requests
    contextl.shutdown
    // Wait for pending main tasks
    contextl.awaitTermination( 100, TimeUnit.DAYS )
    println( s"Finished test $id" )
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
  def test_1( numTasks: Int ) = {
    val id = 1
    println( s"Started test $id" )
    val s = ( 0 to numTasks ).toStream
    val executerl = ForkJoinPool.commonPool() // no limits on the queue size
    val contextl = ExecutionContext.fromExecutorService( executerl )
    s.foreach { x => println( x ); val f = Future( longComputation( 1500 ) )( contextl ) }
    //s.foreach { x => println( s"Created task: $x" ); val f = Future( timeKiller(x) )(contextl)  }
    // don't accept any more requests
    contextl.shutdown
    // Wait for pending main tasks
    contextl.awaitTermination( 100, TimeUnit.DAYS )
    println( s"Finished test $id" )
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
  def test_2( numTasks: Int ) = {
    val id = 2
    println( s"Started test $id" )
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
    // don't accept any more requests
    contextl.shutdown
    // Wait for pending main tasks
    contextl.awaitTermination( 100, TimeUnit.DAYS )
    println( s"Finished test $id" )
  }

  def safelyClosePools( contextLocal: ExecutionContextExecutorService, fixedThreadContextLocal: ExecutionContextExecutorService ) = {
    // don't accept any more requests
    contextLocal.shutdown
    // Wait for pending main tasks
    contextLocal.awaitTermination( 100, TimeUnit.DAYS )
    // The main tasks only terminate when the callbacks are on queue, 
    // so we can now ask for a shutdown because no more threads will be added 
    fixedThreadContextLocal.shutdown
    // Wait until the callbacks finish
    fixedThreadContextLocal.awaitTermination( 100, TimeUnit.DAYS )
    // Now we can safely close the file
  }

  /**
   * THIS LOCKS!
   *
   * Here we use throttling to avoid OOM errors. However the use of the callbacks to
   * process the results causes a deadlock. This happens because all Futures are started
   * filling up the queue and hence blocking n a new Future creation. Each of the queue
   * Futures executes the main task to the end. However the thread is not released until
   * the future adds its own task that executes the callback. Because the queue is already full
   * the request for a callback will also block. So now none of the main tasks Futures can complete
   * and therefore release a queue entry.
   * The use of blocking or Promises will not solve the issue
   *
   * We use a local thread pool to keep pool management local to the test
   *
   * NOTE : do not execute otherwise the application will lock.
   *
   * Solution:
   * @see http://stackoverflow.com/questions/40981240/throttling-scala-future-blocks-when-oncomplete-is-used/40982776
   */
  def test_3( numTasks: Int, numThreads: Int ) = {
    val id = 3
    val contextl = throttlingPool(numThreads)

    println( s"Started test $id" )
    val s = ( 0 to numTasks ).toStream
    // Deadlock
    s.foreach { x => println( s"Created task: $x" ); val f = Future( longComputation() )( contextl ); f.onComplete{ processResult }( contextl ) }

    // don't accept any more requests
    contextl.shutdown
    // Wait for pending main tasks
    contextl.awaitTermination( 100, TimeUnit.DAYS )
    println( s"Finished test $id" )
  }

  /**
   * Same as test 3. Deadlock will occur.
   */
  def test_4( numTasks: Int, numThreads: Int ) = {
    val id = 4
    val contextl = throttlingPool(numThreads)

    println( s"Started test $id" )
    val s = ( 0 to numTasks ).toStream
    // Deadlock
    s.foreach { x =>
      println( s"Created task: $x" )
      val f = Future( longComputation() )( contextl )
      val p = Promise[ Long ]()
      p completeWith f
      p.future.onComplete{ processResult } ( contextl )
    }

    // don't accept any more requests
    contextl.shutdown
    // Wait for pending main tasks
    contextl.awaitTermination( 100, TimeUnit.DAYS )
    println( s"Finished test $id" )
  }

  //val fixedThreadContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
  //val fixedThreadContext = ExecutionContext.fromExecutorService( java.util.concurrent.Executors.newSingleThreadExecutor() )
  //val fixedThreadContext = ExecutionContext.fromExecutorService( java.util.concurrent.Executors.newFixedThreadPool( numIOThreads ) )
  // cachedthreadpool ?

  /**
   * To solve the deadlock issue we use two thread pools. The man pool locks when full thereby
   * throttling the tasks. The second thread pool i unlimited and keeps accepting the IO tasks
   * without blocking (it may generate an OOM though). This means the main thread pool may lock
   * but the IO tasks will not block indefinitely because the IO queue never blocks.
   *
   */
  def test_5( numTasks: Int, numThreads: Int, numIOThreads: Int ) = {
    val id = 5
    val contextl = throttlingPool(numThreads)
    val fixedThreadContext = ExecutionContext.fromExecutorService( java.util.concurrent.Executors.newFixedThreadPool( numIOThreads ) )
    println( s"Started test $id" )
    val s = ( 0 to numTasks ).toStream
    s.foreach { x =>
      println( s"Launhed $x" )
      //val f = Future( longComputation )  
      val f = Future( timeKiller( x ) )( contextl )
      f.onComplete { processResult } ( fixedThreadContext )
    }
    safelyClosePools( contextl, fixedThreadContext )
    println( s"Finished test $id" )
  }
  // CheckManaged

  /**
   * This is an example of how to log the results of the tasks via callbacks. We launch a
   * number of tasks. Each tume the callback is executed it writes the thread's number to
   * disk.
   *
   * IMPORTANT NOTE: the PrintWrite fails silently. More specifically, if we close the file
   * handle the write and reads will silently consume the exceptions and those operations will
   * not work correctly. One should use the `FileLoans` `check` operation.
   */
  def test_6( numTasks: Int, numThreads: Int, numIOThreads: Int, filename: String = "./output/results.txt" ) = {
    val id = 6
    println( s"Started test $id" )

    import java.nio.charset.StandardCharsets

    // Main CPU intensive tasks
    val contextLocal = throttlingPool(numThreads)
    // IO tasks for logging only (grows indefinitely)
    val fixedThreadContextLocal = ExecutionContext.fromExecutorService( java.util.concurrent.Executors.newFixedThreadPool( numIOThreads ) )

    // Open and write a "header"
    val autoFlush = true
    val append = true
    //val o = Source.fromFile( filename )  // reading only
    //val writer = new PrintWriter(new File(filename)) // must exist
    val writerT = new PrintWriter( filename, "UTF-8" ) // creates
    writerT.close // clean
    val tmpWriter = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( filename, append ), StandardCharsets.UTF_8.name() ) )
    val writer = new PrintWriter( tmpWriter, autoFlush )

    println( s"Started test $id\n" )
    try {
      def logger = resultLogger( logResult( formatCSV ) )( writer ) _
      val s = ( 0 to numTasks ).toStream
      s.foreach { x =>
        println( s"Launhed $x" )
        //val f = Future( longComputation )  
        val f = Future( timeKiller( x ) )( contextLocal )
        f.onComplete { logger } ( fixedThreadContextLocal )
      }
    } catch {
      case ioe: IOException => println( ioe )
      //case e: Exception => less specific after
    } finally {
      // don't accept any more requests and close pools
      safelyClosePools( contextLocal, fixedThreadContextLocal )
      // Now we can safely close the file
      writer.close
    }
    println( s"Finished test $id" )
  }

  /**
   * This method is used to check if all the results were produced. The test generates a CSV file were the first field is an
   * ID that is a sequence of integers from `0` to `m`. We get this list of integers and check that  this sequence is in fact
   * complete.
   */
  def checkLogResults( filename: String, maxv: Int ) = {

    // Open the file with the logs
    val bufferedSource = io.Source.fromFile( filename )
    // assume it is a CSV file with a separator, split each line into items
    val r1 = bufferedSource.getLines().toStream.map { line => line.split( ";" ).map( _.trim ) }
    // Get the first item (empty lines are ignored)
    val r2 = r1.map { line => if ( line.size > 0 ) Some( line( 0 ) ) else None }
    // Convert the first item into an integer (error messages will fail here and generate None, ignore those)
    val r3 = r2.flatMap { case ( Some( i ) ) => Some( i.toInt ); case ( None ) => None }
    // Sort the ids we have and pair them with a sequence starting from 0
    val r4 = r3.sortWith{ ( i, j ) => i < j }.zipWithIndex
    //r4.foreach{ x => println( x ) }
    // If the pairs don't match, then some error occurred
    val r5 = r4.forall{ case ( i, j ) => /*println(s"Ok ($i,$j) : ${i == j}") ;*/ i == j }
    // Make sure we have all the tasks done
    val m = r4.max
    bufferedSource.close
    r5 && ( m._1 == maxv )
  }

  
  def main( args: Array[ String ] ) {
    val numIOThreads = 1
    val numThreads = sys.runtime.availableProcessors - numIOThreads

    // Ok, synchronized
    time( test_0( 100, numThreads ) )
    // OOM, growing pool size - no onCompete
    // >= 4194394 to 4398750 with a wait of 1500ms
    //time( test_1( 100000000 ) )
    // Tasks rejected, fixed pool size- no onCompete
    //time( test_2( 100000000 ) )

    // Deadlock
    //time( test_3( 1000, numThreads ) )
    // Deadlock
    //time( test_4( 1000, numThreads ) )

    // Ok
    //time( test_5( 25, numThreads, numIOThreads ) )

    // Ok
    val filename = "./output/results.txt"
    //val maxv = 1000000 // 45MB file
    val maxv = 25
    time( test_6( maxv, numThreads, numIOThreads, filename ) )
    val r6 = checkLogResults( filename, maxv )
    println( s"Test 6 test : $r6" )
    
    import pt.inescn.utils.FileLoans._
    import java.nio.charset.StandardCharsets

    val fileWithPath = "./output/test2.txt"
    // Make sure we append the results we collect
    val append = true
    // We use a PrintWriter that flushes on newlines automatically. No need to do it explicitly.
    val autoFlush = true
    
    val tmpWriter = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( fileWithPath, append ), StandardCharsets.UTF_8.name() ) )
    val writer = new PrintWriter( tmpWriter, autoFlush ) with FlushManaged with CheckManaged
    
    
    // see http://stackoverflow.com/questions/18425026/shutdown-and-awaittermination-which-first-call-have-any-difference
    println( "Finished" )
    // Does an orderly "shutdown". It will wait for all tasks on the queue to complete
    // If new tasks are placed on the queue, these will be rejected causing exceptions
    // Use a timed-out termination to avoid task execution loss (pending call-backs)
    // Don't accept more mains tasks
    //context.shutdown
    // Wait for pending main tasks
    //context.awaitTermination( 100, TimeUnit.DAYS )
    // If all the contexts are not shutdown then the man thread will remain active
    // All main tasks executed, so all callbacks already requested
    // No more callbacks
    //fixedThreadContext.shutdown
    // Wait until they finish
    //fixedThreadContext.awaitTermination( 100, TimeUnit.DAYS )
  }

}
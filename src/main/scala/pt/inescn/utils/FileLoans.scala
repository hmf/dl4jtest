package pt.inescn.utils

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.channels.FileChannel
import java.nio.channels.FileLock

import scala.concurrent._
import scala.util.{ Try, Success, Failure }

/**
 * ARM - Automatic Resource Management
 * Used to manage file for result recoding.
 *
 * sbt "run-main pt.inescn.utils.FileLoans"
 */
object FileLoans {
  /*  
  //val writer : Writer = new BufferedWriter(new OutputStreamWriter( new FileOutputStream("filename.txt"), "utf-8"))
  // writer.write("something");

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
*/
  // http://stackoverflow.com/questions/3893274/how-to-mix-in-a-trait-to-instance
  // http://stackoverflow.com/questions/10373318/mixing-in-a-trait-dynamically
  // http://alvinalexander.com/scala/how-to-dynamically-add-scala-trait-to-object-instance
  // http://www.cakesolutions.net/teamblogs/ways-to-pattern-match-generic-types-in-scala
  /**
   * An resource must be closed when not needed.
   */
  trait Managed {
    def close(): Unit
  }

  /**
   * Some resource smay be flushed to save on resources.
   */
  trait FlushManaged extends Managed {
    def flush(): Unit
  }

  trait CheckManaged extends Managed {
    def checkError(): Boolean
  }

  trait ChannelManaged extends Managed {
    def getChannel(): FileChannel
  }

  /**
   * Loan pattern ensure we catch all exceptions and return a Try.
   * Execute `exec` on the `resource` and return the result as a `Success` or
   * alternately return `Failure`
   */
  def using[ T <: Managed, U ]( resource: T )( exec: T => U ): Try[ U ] = { Try{ exec( resource ) } }

  /**
   * The same as using by lock on a process basis. Slow.
   * FileLock is only for interprocess locking, javadoc reads:
   * File locks are held on behalf of the entire Java virtual machine.
   * They are not suitable for controlling access to a file by multiple threads
   * within the same virtual machine."
   *
   * Note that this only works if the resource has a <code>getChannel()</code> method.
   * For example, the PrintWrite does not have. Note also that PrintWriter is thread
   * safe so the locking loans are not required.
   *
   * TODO:  This is an unsafe version. We should type the locked resources so that
   * we ensure that we only check after the lock has been released. We also have an issue
   * with composition - we can only flush or check when locked.
   *
   * PrintWriter (and Writer) are synchronized. So this is not required.
   * @see http://stackoverflow.com/questions/128038/how-can-i-lock-a-file-using-java-if-possible
   * @see http://stackoverflow.com/questions/11543967/how-to-lock-a-file
   */
  def usingProcessLock[ T <: ChannelManaged, U ]( resource: T )( exec: T => U ): Try[ U ] = {
    Try {
      val channel: FileChannel = resource.getChannel();
      // This method blocks until it can retrieve the lock.
      val lock: FileLock = channel.lock();
      val r = exec( resource )
      // Release the lock - if it is not null!
      if ( lock != null ) {
        lock.release();
      }
      // Close the channel also closes the file
      //channel.close();
      r
    }
  }

  //import java.util.concurrent.locks.ReentrantLock
  //val glock : ReentrantLock  = new ReentrantLock();

  val glock: Object = new Object();

  /**
   * We can opt for the simple Java synchronized or the ReentrantLock.  It seems that
   * for the simple case of thread concurrency synchronized my be more efficient.
   *
   * TODO:  This is an unsafe version. We should type the locked resources so that
   * we ensure that we only check after the lock has been released. We also have an issue
   * with composition - we can only flush or check when locked.
   *
   * @see http://stackoverflow.com/questions/33682553/finally-equivalent-in-scala-try
   * @see https://blogs.oracle.com/dave/entry/java_util_concurrent_reentrantlock_vs
   */
  def lockByThread[ T <: Managed, U ]( resource: T )( exec: T => U ): Try[ U ] = {
    glock.synchronized {
      Try {
        exec( resource )
      }
    }
  }

  /**
   * Loan pattern ensure we catch all exceptions and return a Try.
   * Close the `resource` and return the `prevResult` is all is ok.
   */
  def close[ T <: Managed, U ]( resource: T )( prevResult: U ): Try[ U ] = { Try { resource.close; prevResult } }

  /**
   * Loan pattern ensure we catch all exceptions and return a Try.
   * Flush the `resource` and return the `prevResult` is all is ok.
   */
  def flush[ T <: FlushManaged, U ]( resource: T )( prevResult: U ): Try[ U ] = { Try { resource.flush; prevResult } }

  /**
   * Some resources do not throw exceptions nor return `Try`.
   * For example Java's PrinWriter catches all exceptions and
   * does not return any failure code. Here we explicitly check for
   * an error and report that.
   */
  def check[ T <: CheckManaged, U ]( resource: T )( prevResult: U ): Try[ U ] =
    { Try { if ( resource.checkError() ) throw new IOException( "Error detected" ) else prevResult } }

  /*
   * Combinations of flushing, closing and checking
   */

  /**
   * Use the loan pattern to ensure that we write stuff and then flush the file
   *
   * @see   http://stackoverflow.com/questions/297303/printwriter-and-printstream-never-throw-ioexceptions
   * @notes https://mauricio.github.io/2014/02/17/scala-either-try-and-the-m-word.html
   */
  def usingThenFlush[ T <: FlushManaged, U ]( resource: T )( exec: T => U ): Try[ U ] = {
    using( resource )( exec ) flatMap { x => flush( resource )( x ) }
  }

  /**
   * Use the loan pattern to ensure that we write stuff and then flush the file
   * The PrintWriter does not throw exceptions and fails silently. Check for errors manually.
   *
   * @see   http://stackoverflow.com/questions/297303/printwriter-and-printstream-never-throw-ioexceptions
   */
  def usingThenFlushAndCheck[ T <: FlushManaged with CheckManaged, U ]( resource: T )( exec: T => U ): Try[ U ] = {
    usingThenFlush( resource )( exec ) flatMap { x => check( resource )( x ) }
  }

  /**
   * Use the loan pattern to ensure that we write stuff and then close the file
   * The PrintWriter does throw exceptions and fails silently. Check for errors manually.
   *
   * @see   http://stackoverflow.com/questions/297303/printwriter-and-printstream-never-throw-ioexceptions
   */
  def usingThenClose[ T <: Managed, U ]( resource: T )( exec: T => U ): Try[ U ] = {
    using( resource )( exec ) flatMap { x => close( resource )( x ) }
  }

  /**
   * PrinWriter for example fails silently. Explicitly check for the issues.
   *
   * TODO: How do we compose such a thing?
   */
  def lockByThreadThenUseAndCheck[ T <: CheckManaged, U ]( resource: T )( exec: T => U ): Try[ U ] = {
    glock.synchronized {
      Try {
        val r = exec( resource )
        if ( resource.checkError() ) throw new IOException( "Exec failed" ) else r
      }
    }
  }

  import java.time.LocalDateTime;
  import java.time.Period;
  import java.time.ZoneId;
  import java.time.ZonedDateTime;

  // http://stackoverflow.com/questions/2207425/what-automatic-resource-management-alternatives-exist-for-scala
  def resultLogger( data: String )( writer: PrintWriter ) = {
    val d = LocalDateTime.now
    val z = ZoneId.systemDefault() // ZoneId.of( "Europe/Lisbon")
    println( s"$data: $d $z" )
    writer.println( s"$data: $d $z" )
  }

  // http://stackoverflow.com/questions/128038/how-can-i-lock-a-file-using-java-if-possible
  def logResult[ T ]( r: Try[ T ] ) = {
    blocking {
      r match {
        case Success( id ) => println( s"Thread result: $id" )
        case Failure( t )  => println( "An error has occured: " + t.getMessage )
      }
    }
  }

  /**
   * Test code and examples.
   */
  def main( args: Array[ String ] ) {

    val fileWithPath = "./output/test2.txt"
    // Make sure we append the results we collect
    val append = true
    // We use a PrintWriter that flushes on newlines automatically. No need to do it explicitly.
    val autoFlush = true

    // Create or clean the file if it exists
    val f = Try {
      val writer = new PrintWriter( fileWithPath, StandardCharsets.UTF_8.name() ) // creates
      writer.flush()
      writer.close
    }
    println( f )

    // Open the file indicating its capabilities via the appropriate traits
    //val writer = new PrintWriter( new BufferedWriter( new FileWriter( fileWithPath, append ) ) ) with Managed with CheckManaged
    //val writer = new PrintWriter( new BufferedWriter( new FileWriter( fileWithPath, append ) ), autoFlush  ) with FlushManaged with CheckManaged
    // A way to add append, auto-flush and the character set
    val tmpWriter = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( fileWithPath, append ), StandardCharsets.UTF_8.name() ) )
    val writer = new PrintWriter( tmpWriter ) with FlushManaged with CheckManaged

    // Write 
    val r2 = usingThenFlush( writer ) ( resultLogger( "F" ) _ )
    println( s"Loan Flush : $r2" )
    val r1 = usingThenClose( writer ) ( resultLogger( "C" ) _ )
    println( s"Loan Close : $r1" )
    // This will no be written - file was closed, so error
    val r3 = lockByThreadThenUseAndCheck( writer ) ( resultLogger( "D" ) _ )
    println( s"Loan Lock : $r3" )
  }

}
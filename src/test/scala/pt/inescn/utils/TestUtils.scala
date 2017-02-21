package pt.inescn.utils

object TestUtils {
  
  import org.scalatest._
  import org.scalatest.Matchers._

  val precision = 1e-5

  /**
   * Compare two real values with a given precision. The `eps` parameter
   * determines the precision with which the comparison is done.
   */
  def chk( a: Double, b: Double, eps: Double = precision ) = {
    if ( a.isNaN ) b.isNaN should be (true)
    else if ( a.isInfinity ) b.isInfinity should be (true)
    else if ( b.isNaN ) a.isNaN should be (true)
    else if ( b.isInfinity ) a.isInfinity should be (true)
    else a should be ( ( b ) +- eps )
  }
  
}
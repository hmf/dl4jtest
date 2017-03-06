package pt.inescn.utils

import org.scalatest._

import collection.mutable.Stack
import org.scalatest._

//import pt.inescn.utils.TestUtils._
// import scala.collection.JavaConversions._

import no.uib.cipr.matrix.DenseMatrix
import no.uib.cipr.matrix.DenseVector
import no.uib.cipr.matrix.Matrices
import no.uib.cipr.matrix.Matrix
import no.uib.cipr.matrix.QRP
import no.uib.cipr.matrix.Vector

/**
 *
 *
 *  test:compile
 *  test:console
 *  test:consoleQuick
 *  test:run
 *  test:runMain
 *
 * sbt test
 * sbt "testOnly pt.inescn.utils.QRMatrixToolkitSpec"
 *
 *
 */
class QRMatrixToolkitSpec extends WordSpec with Matchers {

  "Matrices" when {
    "genrated with random elements " should {
      "being thin have only linear independent columns" in {
        val rand = Matrices.random( 6, 4 )
        val A = new DenseMatrix( rand.numRows(), rand.numRows() )
        rand.transBmult( rand, A )
        val qrp = QRP.factorize( A )

        Math.min( rand.numRows(), rand.numColumns() ) shouldBe qrp.getRank()
      }
      "being fat have only linear independent columns" in {
        val rand = Matrices.random( 4, 6 )
        val A = new DenseMatrix( rand.numRows(), rand.numRows() )
        rand.transBmult( rand, A )
        val qrp = QRP.factorize( A )

        Math.min( rand.numRows(), rand.numColumns() ) shouldBe qrp.getRank()
      }
    }
  }

  "The sub-matrices of the simple 1/0 matrix" when {
    
    val c1 = Array( 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 )
    val c2 = Array( 1.0, 1.0, 1.0, 0.0, 0.0, 0.0 )
    val c3 = Array(  0.0, 0.0, 0.0, 1.0, 1.0, 1.0 )
    val c4 = Array(  1.0, 0.0, 0.0, 1.0, 0.0, 0.0 )
    val c5 = Array(  0.0, 1.0, 0.0, 0.0, 1.0, 0.0 )
    val c6 = Array(  0.0, 0.0, 1.0, 0.0, 0.0, 1.0 )

    val threshold = 1e-7
    
    "checked for rank" should {
        "give the same result as SVD 1" in {
          
        }
        
      }
    
  }

}
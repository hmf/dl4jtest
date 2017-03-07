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

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RRQRDecomposition;
import org.apache.commons.math3.linear.RealMatrix;
//import org.apache.commons.math3.linear.QRDecomposition;
//import org.apache.commons.math3.linear.RealMatrix;
//import org.apache.commons.math3.linear.DefaultRealMatrixChangingVisitor;
import org.apache.commons.math3.linear.SingularValueDecomposition;
import org.apache.commons.math3.util.FastMath;

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
    val c3 = Array( 0.0, 0.0, 0.0, 1.0, 1.0, 1.0 )
    val c4 = Array( 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 )
    val c5 = Array( 0.0, 1.0, 0.0, 0.0, 1.0, 0.0 )
    val c6 = Array( 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 )

    val threshold = 1e-7

    def checkRank( threshold: Double, am: Array[ Array[ Double ] ] ) = {
      
      import pt.inescn.scratchpad.QRMatrixToolkit.getRankN
      
      val m = new Array2DRowRealMatrix(am, false) // use array, don't copy
      val qr = new RRQRDecomposition(m, threshold)
      val rank = qr.getRank(threshold)
      println("0 QR rank: " + rank)
      println("1 QR rank: " + getRankN(qr))
      
      val A = new DenseMatrix(am) 
      val qrp = QRP.factorize(A) 
      val nrank = qrp.getRank()
      println("2 QR rank: " + nrank);
      // println("R: \n" + r.toString());
      
      val sv2 = new org.apache.commons.math3.linear.SingularValueDecomposition(m)
      val svdRank = sv2.getRank()
      println("SVD rank: " + svdRank)

      (nrank, svdRank)
    }

    "checked for rank" should {
      "give the same result as SVD 1" in {
        //double[][] am = new double[5][];
        //var am = Array.ofDim[Double](5, 6)
        // Array.fill[ABCD](100,6) { new ABCD }
        // Array.tabulate[ABCD](100,6) { (i,j) => new ABCD(i,j) }
        var am = Array.ofDim[ Array[ Double ] ]( 5 )
        am( 0 ) = c1
        am( 1 ) = c2
        am( 2 ) = c3
        am( 3 ) = c4
        am( 4 ) = c6
        val (nrank, svdRank) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 2" in {
        var am = Array.ofDim[ Array[ Double ] ]( 3 )
        am(0) = c1
        am(1) = c2
        am(2) = c3
        val (nrank, svdRank) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 3" in {
        var am = Array.ofDim[ Array[ Double ] ]( 4 )
        am(0) = c1
        am(1) = c4
        am(2) = c5
        am(3) = c6
        val (nrank, svdRank) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 4" in {
        var am = Array.ofDim[ Array[ Double ] ]( 6 )
        am(0) = c1
        am(1) = c2
        am(2) = c3
        am(3) = c4
        am(4) = c5
        am(5) = c6
        val (nrank, svdRank) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 5" in {
        var am = Array.ofDim[ Array[ Double ] ]( 6 )
        am(0) = c1
        am(1) = c2
        am(2) = c3
        am(3) = c4
        am(4) = c5
        am(5) = c6
        val (nrank, svdRank) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }

    }

  }

}
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

  /**
   * Due to a possibly a numeric stability issue (see link below),  we found that the rank
   * was not correctly determined by the rank revealing QR decomposition function of the 
   * Apache Commons Math3. This function calculates the rank via SVD and checks that 
   * it is equal to the rank calculated by the Matrix Toolkit Java code. 
   * 
   * @param threshold - any matrix value below the threshold is considered  0. 
   * @param am - a column wise matrix (each first index points to a column)
   * 
   * @see  https://issues.apache.org/jira/browse/MATH-1403
   * @see no.uib.cipr.matrix.QRP.factor(Matrix A)
   * @see https://github.com/fommil/matrix-toolkits-java/blob/6157618bc86bcda3749af2a60bf869d8f3292960/src/main/java/no/uib/cipr/matrix/QRP.java
   */
  def checkRank( threshold: Double, am: Array[ Array[ Double ] ] ) = {
    import pt.inescn.scratchpad.QRMatrixToolkit.getRankN

    val m = new Array2DRowRealMatrix( am, false ) // use array, don't copy
    val qr = new RRQRDecomposition( m, threshold )
    val rank = qr.getRank( threshold )
    println( "0 QR rank: " + rank )
    println( "1 QR rank: " + getRankN( qr ) )

    val A = new DenseMatrix( am )
    val qrp = QRP.factorize( A )
    val nrank = qrp.getRank()
    println( "2 QR rank: " + nrank );
    // println("R: \n" + r.toString());

    val sv2 = new org.apache.commons.math3.linear.SingularValueDecomposition( m )
    val svdRank = sv2.getRank()
    println( "SVD rank: " + svdRank )

    ( nrank, svdRank )
  }

  /**
   * Takes an existing matrix `am` and generates a single column that is 
   * linearly dependent on another 3 columns. Both the coefficients and 
   * the columns are randomly selected. 
   * 
   * Note that a column major matrix is returned. 
   * 
   * @see https://en.wikipedia.org/wiki/Rank_%28linear_algebra%29
   * @see https://en.wikipedia.org/wiki/Condition_number
   * @see https://github.com/topepo/caret/issues/607
   * @see https://en.wikipedia.org/wiki/Row-_and_column-major_order
   * 
   * @return  a column major matrix with one column linearly dependent on another 3. 
   */
  def combineLinear1(am: Array[ Array[ Double ] ]) = {
    // Coefficients
    val d1 = new NormalDistribution(1.0, 2.0)
    val d2 = new NormalDistribution(.05, 0.05)
    val d3 = new NormalDistribution(10.0, 0.1)
    val a = d1.sample()
    val b = d2.sample()
    val c = d3.sample()
  
    // Select columns
    val r = new java.util.Random()
    val i1 = r.nextInt(am.length)
    val i2 = r.nextInt(am.length)
    val i3 = r.nextInt(am.length)
    val i4 = r.nextInt(am.length)

    // Generate linear combination
    // am[i1] = (am[i2] * a) + (am[i3] * b) - (am[i4]*c)
    val v1 = new DenseVector(am(i2)).scale(a);
    val v2 = new DenseVector(am(i3)).scale(b);
    val v3 = new DenseVector(am(i4)).scale(-c);
    val v4 = v1.add(v2).add(v3);
    for (i <- 0 until am(i1).length) {
      am(i1)(i) = v4.get(i);
    }
}
  
    /**
   * Generates a 10 x 6 matrix were all the columns are randomly generated
   * using Gaussian (normal) distributions. This is used to check the rank 
   * of a singular matrix (more columns than rows) where all columns are 
   * linearly independent.
   * 
   * Note that a column major matrix is returned. 
   * 
   * @see https://en.wikipedia.org/wiki/Rank_%28linear_algebra%29
   * @see https://en.wikipedia.org/wiki/Condition_number
   * @see https://github.com/topepo/caret/issues/607
   * @see https://en.wikipedia.org/wiki/Row-_and_column-major_order
   * 
   * @return  a column major matrix wit all columns linearly independent of each other. 
   */
  def  genColumns :  Array[ Array[ Double ] ] = {

    val d1 = new NormalDistribution(5.0, 2.0)
    val d2 = new NormalDistribution(15.0, 5.0)
    val d3 = new NormalDistribution(25.0, 6.0)
    val d4 = new NormalDistribution(35.0, 7.0)
    val d5 = new NormalDistribution(45.0, 8.0)
    val d6 = new NormalDistribution(55.0, 9.0)

    val n = 10

    val am = Array.ofDim[ Array[ Double ] ]( 6 )
    
    am(0) = d1.sample(n)
    am(1) = d2.sample(n)
    am(2) = d3.sample(n)
    am(3) = d4.sample(n)
    am(4) = d5.sample(n)
    am(5) = d6.sample(n)

    am
  }
  
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
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 2" in {
        var am = Array.ofDim[ Array[ Double ] ]( 3 )
        am( 0 ) = c1
        am( 1 ) = c2
        am( 2 ) = c3
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 3" in {
        var am = Array.ofDim[ Array[ Double ] ]( 4 )
        am( 0 ) = c1
        am( 1 ) = c4
        am( 2 ) = c5
        am( 3 ) = c6
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 4" in {
        var am = Array.ofDim[ Array[ Double ] ]( 6 )
        am( 0 ) = c1
        am( 1 ) = c2
        am( 2 ) = c3
        am( 3 ) = c4
        am( 4 ) = c5
        am( 5 ) = c6
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 5 with 1 linear combination" in {
        var am = Array.ofDim[ Array[ Double ] ]( 6 )
        am( 0 ) = c1
        am( 1 ) = c2
        am( 2 ) = c3
        am( 3 ) = c4
        am( 4 ) = c5
        am( 5 ) = c6
        combineLinear1(am);
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 6 with 2 linear combinations" in {
        var am = Array.ofDim[ Array[ Double ] ]( 6 )
        am( 0 ) = c1
        am( 1 ) = c2
        am( 2 ) = c3
        am( 3 ) = c4
        am( 4 ) = c5
        am( 5 ) = c6
        combineLinear1( am );
        combineLinear1( am );
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 7 with repeated 0 linear combinations" in {
        val r = (0 to 10)
        r.foreach { i =>  
            val am = genColumns
            val ( nrank, svdRank ) = checkRank( threshold, am )
            nrank shouldBe svdRank
            List(i, nrank)  should contain theSameElementsInOrderAs List( i, svdRank)
          }
      }
      "give the same result as SVD 8 with repeated 1 linear combination" in {
        val r = (0 to 10)
        r.foreach { i =>  
            val am = genColumns
            combineLinear1(am)
            val ( nrank, svdRank ) = checkRank( threshold, am )
            nrank shouldBe svdRank
            List(i, nrank)  should contain theSameElementsInOrderAs List( i, svdRank)
          }
      }
      "give the same result as SVD 9 with repeated 2 linear combinations" in {
        val r = (0 to 10)
        r.foreach { i =>  
            val am = genColumns
            combineLinear1(am)
            combineLinear1(am)
            val ( nrank, svdRank ) = checkRank( threshold, am )
            nrank shouldBe svdRank
            List(i, nrank)  should contain theSameElementsInOrderAs List( i, svdRank)
          }
      }

/*
    for (int i = 0; i < 10; i++) {
      double[][] am6 = genColumns();
      combineLinear1(am6);
      combineLinear1(am6);
      checkRank(threshold, am6);
    }
    for (int i = 0; i < 10; i++) {
      double[][] am6 = genColumns();
      combineLinear1(am6);
      combineLinear1(am6);
      checkRank(threshold, am6);
    }
 */

    }

  }

}
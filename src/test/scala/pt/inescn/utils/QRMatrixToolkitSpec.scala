package pt.inescn.utils

import org.scalatest._

import collection.mutable.Stack
import org.scalatest._

//import pt.inescn.utils.TestUtils._
// import scala.collection.JavaConverters._

import no.uib.cipr.matrix.DenseMatrix
import no.uib.cipr.matrix.DenseVector
import no.uib.cipr.matrix.Matrices
import no.uib.cipr.matrix.Matrix
import no.uib.cipr.matrix.QRP
import no.uib.cipr.matrix.Vector

import org.apache.commons.lang3.tuple.Pair
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.RRQRDecomposition
import org.apache.commons.math3.linear.RealMatrix
//import org.apache.commons.math3.linear.QRDecomposition
//import org.apache.commons.math3.linear.RealMatrix
//import org.apache.commons.math3.linear.DefaultRealMatrixChangingVisitor
import org.apache.commons.math3.linear.SingularValueDecomposition
import org.apache.commons.math3.util.FastMath

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
    //println( "0 QR rank: " + rank )
    //println( "1 QR rank: " + getRankN( qr ) )

    val A = new DenseMatrix( am )
    val qrp = QRP.factorize( A )
    val nrank = qrp.getRank()
    //println( "2 QR rank: " + nrank )
    // println("R: \n" + r.toString())

    val sv2 = new org.apache.commons.math3.linear.SingularValueDecomposition( m )
    val svdRank = sv2.getRank()
    //println( "SVD rank: " + svdRank )

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
  def combineLinear1( am: Array[ Array[ Double ] ] ) = {
    // Coefficients
    val d1 = new NormalDistribution( 1.0, 2.0 )
    val d2 = new NormalDistribution( .05, 0.05 )
    val d3 = new NormalDistribution( 10.0, 0.1 )
    val a = d1.sample()
    val b = d2.sample()
    val c = d3.sample()

    // Select columns
    val r = new java.util.Random()
    val i1 = r.nextInt( am.length )
    val i2 = r.nextInt( am.length )
    val i3 = r.nextInt( am.length )
    val i4 = r.nextInt( am.length )

    // Generate linear combination
    // am[i1] = (am[i2] * a) + (am[i3] * b) - (am[i4]*c)
    val v1 = new DenseVector( am( i2 ) ).scale( a )
    val v2 = new DenseVector( am( i3 ) ).scale( b )
    val v3 = new DenseVector( am( i4 ) ).scale( -c )
    val v4 = v1.add( v2 ).add( v3 )
    for ( i <- 0 until am( i1 ).length ) {
      am( i1 )( i ) = v4.get( i )
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
  def genColumns: Array[ Array[ Double ] ] = {

    val d1 = new NormalDistribution( 5.0, 2.0 )
    val d2 = new NormalDistribution( 15.0, 5.0 )
    val d3 = new NormalDistribution( 25.0, 6.0 )
    val d4 = new NormalDistribution( 35.0, 7.0 )
    val d5 = new NormalDistribution( 45.0, 8.0 )
    val d6 = new NormalDistribution( 55.0, 9.0 )

    val n = 10
    val am = Array.ofDim[ Array[ Double ] ]( 6 )
    am( 0 ) = d1.sample( n )
    am( 1 ) = d2.sample( n )
    am( 2 ) = d3.sample( n )
    am( 3 ) = d4.sample( n )
    am( 4 ) = d5.sample( n )
    am( 5 ) = d6.sample( n )

    am
  }

  import pt.inescn.scratchpad.QRMatrixToolkit.collinear

  /**
   * This function take sin  a set of vectors that represent a matrice's columns.
   * It converts these into a dense matrix and checks do a single set of multicollinearity.
   * It invokes the `collinear` function with the `checkResults` set to true. When this
   * is done, each set of coefficients are multiplied with the columns that are identified as
   * dependent.  The results are checked to confirm that the independent column is obtained.
   * It then returns the results of `collinear` - a list of lists, were each inner list has as the
   * first element the index of the dependent column and the rest of the elements the indexes
   * of the independent columns.
   *
   * @see collinear
   *
   * @param cols
   * @param threshold
   * @return
   */
  def test3( cols: Array[ Vector ], threshold: Double ): java.util.List[ java.util.List[ Integer ] ] = {
    val M = new DenseMatrix( cols )
    //println("Testing M = \n" + M.toString)
    //printMat(M)
    collinear( M, threshold, true )
  }

  /**
   * This function invokes and returns the results of  `test3`. However it also
   * takes in a string `expectedResult` that is compared to the conversion of the
   * `test3`results. In this way we can test if we have the expected results.
   *
   * @see test3
   *
   * @param cols
   * @param threshold
   * @param expectedResult
   */
  def test4( cols: Array[ Vector ], threshold: Double, expectedResult: String ) = {
    val l = test3( cols, threshold )
    //println(l.toString)
    import org.scalactic.StringNormalizations._
    //assert (l.toString().equalsIgnoreCase(expectedResult))
    l.toString() should equal ( expectedResult ) ( after being lowerCased )
  }

  /**
   * Generates a set of columns with one column that is a linear combination of another
   * set of columns. This columns are created with `numrows`, wherein the `insert` row
   * is the linear dependent column. The number of columns is determined by the
   * size of `dists` (distributions) and `coeff`. Each column's data is generated using
   * on distribution from `dists`. Each independent column is scaled by the value of
   * `coeff`. Note that the size of `dists` and `coeff` <bold>must</bold> be the same.
   *
   * Note that the number of columns is the size of `dists` + 1.
   *
   * @see combineLinear1
   *
   * @param insert
   * @param numrows
   * @param dists
   * @param coeff
   * @return the set of columns generated and the index of the linear dependent column
   */
  def combineLinear2(
    insert: Int, numrows: Int,
    dists: Array[ NormalDistribution ],
    coeff: Array[ Double ] ): ( Array[ Vector ], Integer ) = {

    val len = dists.length
    val cols = Array.ofDim[ Vector ]( len )
    val colst = cols.zipWithIndex
    // Create all the independent columns
    colst.filter( _._2 != insert )
      .map {
        case ( c, i ) =>
          cols( i ) = new DenseVector( dists( i ).sample( numrows ) ).scale( coeff( i ) )
          ()
      }

    // Create the single independent column
    cols( insert ) = new DenseVector( numrows )
    // Make it a lnear combination of all the independent columns
    colst.filter( _._2 != insert )
      .map {
        case ( c, i ) =>
          cols( insert ).add( cols( i ) )
      }
    ( cols, insert )
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
        //double[][] am = new double[5][]
        //var am = Array.ofDim[Double](5, 6)
        // Array.fill[ABCD](100,6) { new ABCD }
        // Array.tabulate[ABCD](100,6) { (i,j) => new ABCD(i,j) }
        var am = Array.ofDim[ Array[ Double ] ]( 5 )
        am( 0 ) = c1.clone
        am( 1 ) = c2.clone
        am( 2 ) = c3.clone
        am( 3 ) = c4.clone
        am( 4 ) = c6.clone
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 2" in {
        var am = Array.ofDim[ Array[ Double ] ]( 3 )
        am( 0 ) = c1.clone
        am( 1 ) = c2.clone
        am( 2 ) = c3.clone
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 3" in {
        var am = Array.ofDim[ Array[ Double ] ]( 4 )
        am( 0 ) = c1.clone
        am( 1 ) = c4.clone
        am( 2 ) = c5.clone
        am( 3 ) = c6.clone
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 4" in {
        var am = Array.ofDim[ Array[ Double ] ]( 6 )
        am( 0 ) = c1.clone
        am( 1 ) = c2.clone
        am( 2 ) = c3.clone
        am( 3 ) = c4.clone
        am( 4 ) = c5.clone
        am( 5 ) = c6.clone
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 5 with 1 linear combination" in {
        var am = Array.ofDim[ Array[ Double ] ]( 6 )
        am( 0 ) = c1.clone
        am( 1 ) = c2.clone
        am( 2 ) = c3.clone
        am( 3 ) = c4.clone
        am( 4 ) = c5.clone
        am( 5 ) = c6.clone
        combineLinear1( am )
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 6 with 2 linear combinations" in {
        var am = Array.ofDim[ Array[ Double ] ]( 6 )
        am( 0 ) = c1.clone
        am( 1 ) = c2.clone
        am( 2 ) = c3.clone
        am( 3 ) = c4.clone
        am( 4 ) = c5.clone
        am( 5 ) = c6.clone
        combineLinear1( am )
        combineLinear1( am )
        val ( nrank, svdRank ) = checkRank( threshold, am )
        nrank shouldBe svdRank
      }
      "give the same result as SVD 7 with repeated 0 linear combinations" in {
        val r = ( 0 to 10 )
        r.foreach { i =>
          val am = genColumns
          val ( nrank, svdRank ) = checkRank( threshold, am )
          nrank shouldBe svdRank
          List( i, nrank ) should contain theSameElementsInOrderAs List( i, svdRank )
        }
      }
      "give the same result as SVD 8 with repeated 1 linear combination" in {
        val r = ( 0 to 10 )
        r.foreach { i =>
          val am = genColumns
          combineLinear1( am )
          val ( nrank, svdRank ) = checkRank( threshold, am )
          nrank shouldBe svdRank
          List( i, nrank ) should contain theSameElementsInOrderAs List( i, svdRank )
        }
      }
      "give the same result as SVD 9 with repeated 2 linear combinations" in {
        val r = ( 0 to 10 )
        r.foreach { i =>
          val am = genColumns
          combineLinear1( am )
          combineLinear1( am )
          val ( nrank, svdRank ) = checkRank( threshold, am )
          nrank shouldBe svdRank
          List( i, nrank ) should contain theSameElementsInOrderAs List( i, svdRank )
        }
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

    val v1 = new DenseVector( c1 )
    val v2 = new DenseVector( c2 )
    val v3 = new DenseVector( c3 )
    val v4 = new DenseVector( c4 )
    val v5 = new DenseVector( c5 )
    val v6 = new DenseVector( c6 )

    val threshold = 1e-7

    "checked for collinearity" should {
      "find solution 1" in {
        val cols1 = Array[ Vector ]( v1, v2, v3 )
        test4( cols1, threshold, "[[1, 0, 2]]" )
      }
      "find solution 2" in {
        val cols2 = Array[ Vector ]( v1, v4, v5, v6 )
        test4( cols2, threshold, "[[3, 0, 1, 2]]" )
      }
      "find solution 3" in {
        val cols3 = Array[ Vector ]( v1, v2, v3, v4, v6 )
        test4( cols3, threshold, "[[1, 0, 2]]" )
      }
      "find multiple solutions 1" in {
        val cols4 = Array[ Vector ]( v1, v2, v3, v4, v5, v6 )
        test4( cols4, threshold, "[[1, 0, 2], [5, 0, 3, 4]]" )
      }
    }
  }

  "The sub-matrices of the random matrix 1 " when {

    val d1 = new NormalDistribution( 1.0, 2.0 )
    val d2 = new NormalDistribution( .05, 0.05 )
    val d3 = new NormalDistribution( 10.0, 0.1 )
    val d4 = new NormalDistribution( 100.0, 0.1 )
    val d5 = new NormalDistribution( 0.6, 0.3 )

    val threshold = 1e-7

    "checked for collinearity" should {
      "find solution with correct coefficents 1" in {
        val dists = Array[ NormalDistribution ]( d1, d2, d3, d4, d5 )
        val coeff = Array[ Double ]( 1.0, 2.0, 3.0, 4.0, 5.0 )
        val p = combineLinear2( 0, 5, dists, coeff )
        //println( p._1.mkString("<",",", ">") )
        //println("Dependent idx = " + p._2)
        test3( p._1, threshold );
      }
      "find solution with correct coefficents 2" in {
        val dists = Array[ NormalDistribution ]( d5, d3, d4, d2, d1 )
        val coeff = Array[ Double ]( -10.0, 20.0, 3.0, 4.0, -500.0 )
        val p = combineLinear2( 3, 5, dists, coeff )
        //println( p._1.mkString("<",",", ">") )
        //println("Dependent idx = " + p._2)
        test3( p._1, threshold );
      }
    }
  }

  "The sub-matrices of the random matrix 2 " when {

    val c11 = Array[ Double ]( 1.012, 10.01, 20.0211, 300.0303, 5.06, 6.06 )
    val c12 = Array[ Double ]( 0.033, 0.045, 0.022, 0.033, 0.045, 0.06 )
    val c13 = Array[ Double ]( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 )
    val c14 = Array[ Double ]( 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 )
    val c15 = Array[ Double ]( 66.06, 98.9, 107.707, 110.0119, 67.08, 88.87 )
    val c16 = Array[ Double ]( 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 )

    val v11 = new DenseVector( c11 )
    val v12 = new DenseVector( c12 )
    val v13 = new DenseVector( c13 )
    val v14 = new DenseVector( c14 )
    val v15 = new DenseVector( c15 )
    val v16 = new DenseVector( c16 )

    val threshold = 1e-7

    "checked for collinearity" should {
      "find solution with correct coefficents 1" in {
        val v11t = v11.copy().scale( 56.03 )
        val v12t = v12.copy().scale( 89.309 )
        val v15t = v15.copy().scale( 0.9077 )
        v13.add( v11t ).add( v12t ).add( v15t );

        val cols7 = Array[ Vector ]( v11, v12, v13, v14, v15 )
        test4( cols7, threshold, "[[1, 2, 4, 0]]" )
      }
      "find solution with correct coefficents 2" in {
        val v11t = v11.copy().scale( 6.03 )
        val v12t = v12.copy().scale( 899.309 )
        val v15t = v15.copy().scale( 0.177 )
        v13.add( v11t ).add( v12t ).add( v15t );

        val cols7 = Array[ Vector ]( v11, v12, v13, v14, v15 )
        test4( cols7, threshold, "[[1, 2, 4, 0]]" )
      }
    }
  }

  "The sub-matrices of the simple 1/0 matrix" when {

    val c1 = Array( 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0 )
    val c2 = Array( 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0 )
    val c3 = Array( 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0 )
    val c4 = Array( 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0 )
    val c5 = Array( 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0 )
    val c6 = Array( 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 )
    val c7 = Array( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0 )
    val c8 = Array( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 2.0 )
    val c9 = Array( 0.0, 0.0, 2.0, 0.0, 0.0, 2.0, 0.0, 0.0 )
    val c10 = Array( 0.0, 0.0, -1.0, 0.0, 0.0, -1.0, 0.0, 0.0 )
    //val c7 = Array( 0.0, 2.0, 2.0, 0.0, 2.0, 2.0 )
    //val c8 = Array( 0.0, -2.0, -1.0, 0.0, -2.0, -1.0 )
    
    val v1 = new DenseVector( c1 )
    val v2 = new DenseVector( c2 )
    val v3 = new DenseVector( c3 )
    val v4 = new DenseVector( c4 )
    val v5 = new DenseVector( c5 )
    val v6 = new DenseVector( c6 )
    val v7 = new DenseVector( c7 )
    val v8 = new DenseVector( c8 )
    val v9 = new DenseVector( c9 )
    val v10 = new DenseVector( c10 )

    val threshold = 1e-7

    "checked for badlists" should {
      "give Caret's results" in {
        import pt.inescn.scratchpad.QRMatrixToolkit.findLinearCombos

        val cols = Array[ Vector ]( v1, v2, v3, v4, v5, v6 )
        val M = new DenseMatrix( cols )
        val p = findLinearCombos( M, threshold, true )
        
        import scala.collection.JavaConverters._

        val l : java.util.List[java.util.List[Integer]] = p.getFirst()
        val r : java.util.List[Integer] = p.getSecond()
        println("ls = " + l.asScala.mkString(","))
        println("rs = " + r.asScala.mkString(","))
        val ll = l.asScala.toList.map { x => x.asScala }
        //ll should contain theSameElementsAs List( List(1,0,2), List(5,0,3,4) )
        ll should contain theSameElementsInOrderAs List( List(1,0,2), List(5,0,3,4) )
        r.asScala should contain theSameElementsInOrderAs List(1,5)
     }
     "find multiple groups" in {
        import pt.inescn.scratchpad.QRMatrixToolkit.findLinearCombos

        val cols = Array[ Vector ]( v1, v2, v3, v4, v5, v6, v7, v8 )
        val M = new DenseMatrix( cols )
        val p = findLinearCombos( M, threshold, true )
        
        import scala.collection.JavaConverters._

        val l : java.util.List[java.util.List[Integer]] = p.getFirst()
        val r : java.util.List[Integer] = p.getSecond()
        println("ls = " + l.asScala.mkString(","))
        println("rs = " + r.asScala.mkString(","))
        val ll = l.asScala.toList.map { x => x.asScala }
        ll should contain theSameElementsInOrderAs List( List(1,0,2), List(4,0,3,5), List(6,7) )
        r.asScala should contain theSameElementsInOrderAs List(1,4,6)
     }
     /*
      * Finds correct slution, but not the same as caret's
      * $linearCombos
      * $linearCombos[[1]]
      * [1] 3 1 2
      * 
      * $linearCombos[[2]]
      * [1] 6 1 4 5
      * 
      * $linearCombos[[3]]
      * [1] 7 1 4 5
      * 
      * $linearCombos[[4]]
      * [1] 8 1 4 5
      * 
      * $remove
      * [1] 3 6 7 8
      */
      "give Caret's results 2" in {
        import pt.inescn.scratchpad.QRMatrixToolkit.findLinearCombos

        val cols = Array[ Vector ]( v1, v2, v3, v4, v5, v6, v9, v10 )
        val M = new DenseMatrix( cols )
        val p = findLinearCombos( M, threshold, true )
        
        import scala.collection.JavaConverters._

        val l : java.util.List[java.util.List[Integer]] = p.getFirst()
        val r : java.util.List[Integer] = p.getSecond()
        println("ls = " + l.asScala.mkString(","))
        println("rs = " + r.asScala.mkString(","))
        val ll = l.asScala.toList.map { x => x.asScala }
        ll should contain theSameElementsInOrderAs List( List(1,0,2), List(4,6,0,3), List(5,6), List(7,6) )
        r.asScala should contain theSameElementsInOrderAs List(1,4,5,7)
     }
    }
  }
}
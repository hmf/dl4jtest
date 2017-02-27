package pt.inescn.scratchpad;

import no.uib.cipr.matrix.DenseMatrix;
import no.uib.cipr.matrix.Matrices;
import no.uib.cipr.matrix.Matrix;
import no.uib.cipr.matrix.QRP;
import static org.junit.Assert.assertEquals;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.QRDecomposition;
import org.apache.commons.math3.linear.RRQRDecomposition;
import org.apache.commons.math3.linear.RealMatrix;
//import org.apache.commons.math3.linear.DefaultRealMatrixChangingVisitor;
import org.apache.commons.math3.linear.SingularValueDecomposition;
import org.apache.commons.math3.util.FastMath;

/**
 * 
 * Note: The threshold used to identify non-negligible terms is max(m,n) ×
 * ulp(s1) where ulp(s1) is the least significant bit of the largest singular
 * value.
 * 
 * sbt "run-main pt.inescn.scratchpad.QRMatrixToolkit"
 */
public class QRMatrixToolkit {

  /**
   * https://issues.apache.org/jira/browse/MATH-1403
   * 
   * https://amath.colorado.edu/faculty/martinss/Teaching/APPM5720_2016s/notes05
   * .pdf Literature survey on low rank approximation of matrices N. Kishore
   * Kumar and J. Schneider https://arxiv.org/pdf/1606.06511.pdf
   * 
   * On Rank Revealing QR Factorizations Shivkumar Chandrasekaran, Ilse Ipsen
   * Research Report YaleU/DCS/RR-880 December 1991
   * http://cpsc.yale.edu/sites/default/files/files/tr880.pdf
   * 
   * http://www.math.sjsu.edu/~foster/rankrevealingcode.html
   * 
   * https://gitlab.msu.edu/orobworld/QRTest/blob/master/src/qrtest/QRTest.java
   * 
   * A randomized blocked algorithm for efficiently computing rank-revealing
   * factorizations of matrices Per-Gunnar Martinsson and Sergey Voronin
   * https://arxiv.org/pdf/1503.07157.pdf
   * 
   * https://github.com/fommil/netlib-java Breeze:
   * https://github.com/scalanlp/breeze
   * /blob/5fd292608f3152bb0126694f7454890fa47c3e78
   * /math/src/test/scala/breeze/linalg/LinearAlgebraTest.scala
   * https://launchpad.net/ubuntu/+source/lapack
   * https://launchpad.net/ubuntu/+source/openblas
   * 
   * https://github.com/fommil/matrix-toolkits-java
   * https://github.com/fommil/matrix
   * -toolkits-java/blob/6157618bc86bcda3749af2a60bf869d8f3292960
   * /src/test/java/no/uib/cipr/matrix/QRPTest.java
   * 
   * http://ejml.org/wiki/index.php?title=Main_Page
   * 
   * https://github.com/scalanlp/breeze/blob/5f
   * d292608f3152bb0126694f7454890fa47c3e78
   * /math/src/test/scala/breeze/linalg/LinearAlgebraTest.scala import
   * breeze.linalg.qr.QR import breeze.linalg.qrp.QRP import breeze.{math =>
   * bmath}
   * 
   * https://eigen.tuxfamily.org/dox/group__QR__Module.html scipy.linalg.qr
   * https://help.scilab.org/docs/5.5.1/ru_RU/rankqr.html (Slicot library
   * routines MB03OD, ZB03OD.) https://searchcode.com/codesearch/view/25389128/
   * https://sourcecodebrowser.com/scilab/5.1.1/sci__f__rankqr_8f_source.html
   * 
   * @param qr
   * @param dropThreshold
   * @return
   */

  public static void testRank1() {
    Matrix rand = Matrices.random(6, 4);

    Matrix A = new DenseMatrix(rand.numRows(), rand.numRows());
    rand.transBmult(rand, A);

    QRP qrp = QRP.factorize(A);

    assertEquals(Math.min(rand.numRows(), rand.numColumns()), qrp.getRank());
  }

  public static void testRank2() {
    Matrix rand = Matrices.random(4, 6);

    Matrix A = new DenseMatrix(rand.numRows(), rand.numRows());
    rand.transBmult(rand, A);

    QRP qrp = QRP.factorize(A);

    assertEquals(Math.min(rand.numRows(), rand.numColumns()), qrp.getRank());
  }

  static public int getRank(double[][] am) {
    Matrix A = new DenseMatrix(am);
    QRP qrp = QRP.factorize(A);
    return qrp.getRank();
  }

  static public void test3(double threshold, double[][] am) {
    Array2DRowRealMatrix m = new Array2DRowRealMatrix(am, false); // use array,
    // don't copy
    RRQRDecomposition qr = new RRQRDecomposition(m, threshold);
    RealMatrix r = qr.getR();
    int numColumns = r.getColumnDimension();
    int rank = qr.getRank(threshold);
    System.out.println("QR rank: " + rank);
    System.out.println("QR is singular: " + !qr.getSolver().isNonSingular());
    System.out.println("QR is singular: " + (numColumns == rank));
    System.out.println("R: \n" + r.toString());
    int nrank = getRank(am);
    System.out.println("QR rank 1: " + nrank);
    // System.out.println("R: \n" + r.toString());

    SingularValueDecomposition sv2 = new org.apache.commons.math3.linear.SingularValueDecomposition(
        m);
    int svdRank = sv2.getRank();
    System.out.println("SVD rank: " + svdRank);

    // System.out.println( "" + rank + " == " + svdRank);
    // assert(rank == svdRank);

    // System.out.println( "" + nrank + " == " + svdRank);
    assert (nrank == svdRank);
  }

  static public double[][] genColumns() {
    NormalDistribution d1 = new NormalDistribution(5.0, 2.0);
    NormalDistribution d2 = new NormalDistribution(15.0, 5.0);
    NormalDistribution d3 = new NormalDistribution(25.0, 6.0);
    NormalDistribution d4 = new NormalDistribution(35.0, 7.0);
    NormalDistribution d5 = new NormalDistribution(45.0, 8.0);
    NormalDistribution d6 = new NormalDistribution(55.0, 9.0);

    int n = 10;

    double[][] am = new double[6][];
    double[] c1 = d1.sample(n);
    double[] c2 = d2.sample(n);
    double[] c3 = d3.sample(n);
    double[] c4 = d4.sample(n);
    double[] c5 = d5.sample(n);
    double[] c6 = d6.sample(n);

    am[0] = c1;
    am[1] = c2;
    am[2] = c3;
    am[3] = c4;
    am[4] = c5;
    am[5] = c6;

    return am;
  }

  /*
   * static public void test1(double threshold, double[][] am) {
   * Array2DRowRealMatrix m = new Array2DRowRealMatrix(am, false); // use array,
   * // don't copy RRQRDecomposition qr = new RRQRDecomposition(m, threshold);
   * RealMatrix r = qr.getR(); int numColumns = r.getColumnDimension(); int rank
   * = qr.getRank(threshold); System.out.println("QR rank: " + rank);
   * System.out.println("QR is singular: " + !qr.getSolver().isNonSingular());
   * System.out.println("QR is singular: " + (numColumns == rank)); TODO * int
   * nrank = getRankN(qr, threshold); //int nrank = getRank(qr, 0.1);
   * System.out.println("QR rank 1: " + nrank);
   * 
   * System.out.println("R: \n" + r.toString());
   * 
   * SingularValueDecomposition sv2 = new
   * org.apache.commons.math3.linear.SingularValueDecomposition(m); int svdRank
   * = sv2.getRank(); System.out.println("SVD rank: " + svdRank);
   * 
   * //System.out.println( "" + rank + " == " + svdRank); //assert(rank ==
   * svdRank);
   * 
   * //System.out.println( "" + nrank + " == " + svdRank); assert(nrank ==
   * svdRank); }
   */
  public static void main(String[] args) {

    double[] c1 = new double[] { 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 };
    double[] c2 = new double[] { 1.0, 1.0, 1.0, 0.0, 0.0, 0.0 };
    double[] c3 = new double[] { 0.0, 0.0, 0.0, 1.0, 1.0, 1.0 };
    double[] c4 = new double[] { 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 };
    double[] c5 = new double[] { 0.0, 1.0, 0.0, 0.0, 1.0, 0.0 };
    double[] c6 = new double[] { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 };

    Double threshold = 1e-7;

    testRank1();
    testRank2();

    double[][] am = new double[5][];
    am[0] = c1;
    am[1] = c2;
    am[2] = c3;
    am[3] = c4;
    am[4] = c6;
    test3(threshold, am);

    double[][] am1 = new double[3][];
    am1[0] = c1;
    am1[1] = c2;
    am1[2] = c3;
    test3(threshold, am1);

    double[][] am2 = new double[4][];
    am2[0] = c1;
    am2[1] = c4;
    am2[2] = c5;
    am2[3] = c6;
    test3(threshold, am2);

    double[][] am3 = new double[6][];
    am3[0] = c1;
    am3[1] = c2;
    am3[2] = c3;
    am3[3] = c4;
    am3[4] = c5;
    am3[5] = c6;
    test3(threshold, am3);

    double[][] am4 = genColumns();
    for (int i = 0; i < 100; i++) {
      test3(threshold, am4);
    }

  }
}

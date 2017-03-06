package pt.inescn.scratchpad;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.stream.IntStream;

import no.uib.cipr.matrix.DenseMatrix;
import no.uib.cipr.matrix.Matrices;
import no.uib.cipr.matrix.Matrix;
import no.uib.cipr.matrix.MatrixEntry;
import no.uib.cipr.matrix.QRP;
import static org.junit.Assert.assertEquals;
import no.uib.cipr.matrix.Vector;
import no.uib.cipr.matrix.DenseVector;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;
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
 * Note: The threshold used to identify non-negligible terms is max(m,n) ×
 * ulp(s1) where ulp(s1) is the least significant bit of the largest singular
 * value.
 * 
 * @see https://github.com/fommil/matrix-toolkits-java
 * @see https://github.com/fommil/netlib-java
 * @see http://icl.cs.utk.edu/f2j/
 * @see http://netlib.org/
 * 
 *  sbt "run-main pt.inescn.scratchpad.QRMatrixToolkit"
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



  /**
   * Creates a copy of the top left matrix of `am`.  It copies all rows
   * until `row`. It copies all columns until `col`. 
   * 
   * @param am
   * @param row - copy all rows until this row
   * @param col - copy all columns until this column
   * @return  a copy of the top left matrix `am`
   */
  public static double[][] subTopLeftMatrix(DenseMatrix am, int row, int col) {
    double[][] nam = new double[row][col];
    for (int i = 0; i < row; i++) {
      for (int j = 0; j < col; j++) {
        nam[i][j] = am.get(i, j);
      }
    }
    return nam;
  }

  /**
   * Creates a copy of the top right matrix of `am`.  It copies all rows
   * until `row`. It copies all columns from `col` until the last column of
   * `am`
   * 
   * @param am
   * @param row
   * @param col
   * @return a copy of the top right matrix `am`
   */
  public static double[][] subTopRightMatrix(DenseMatrix am, int row, int col) {
    double[][] nam = new double[row][am.numColumns() - col];
    for (int i = 0; i < row; i++) {
      for (int j = col; j < am.numColumns(); j++) {
        nam[i][j - col] = am.get(i, j);
      }
    }
    return nam;
  }

  /**
   * It scans all values of matrix `m` and if any of these values are
   * below the `threshold` then it assigns a value 0.0. 
   * 
   * @param m
   * @param threshold
   */
  public static void zap(DenseMatrix m, double threshold) {
    for (int i = 0; i < m.numRows(); i++)
      for (int j = 0; j < m.numColumns(); j++)
        if (Math.abs(m.get(i, j)) < threshold)
          m.set(i, j, 0.0);
  }

  /**
   * This function scans all the rows of the `column` in the matrix `m`.
   * It returns a list of the rows that are not 0.0. Note that we only use this 
   * on values that have been "zapped". 
   * 
   * @param m
   * @param column
   * @return indexes of rows that are not 0.0 
   * @see zap
   */
  public static List<Integer> which(DenseMatrix m, int column) {
    List<Integer> idxs = new ArrayList<Integer>();
    for (int i = 0; i < m.numRows(); i++)
      if (m.get(i, column) != 0.0)
        idxs.add(i);
    return idxs;
  }

  /**
   * Returns sub-matrix from the matrix `am`. It selects rows from `row1` to `row2` inclusive.  
   * It only selects and returns the columns listed in `cols`. Note that this function performs a 
   * deep copy twice. 
   * 
   * TODO: add to Matrix class
   * 
   * @param am
   * @param cols
   * @param row1
   * @param row2
   * @return DenseMatrix
   */
  public static DenseMatrix getSubMarix(Matrix am, List<Integer> cols, int row1, int row2) {
    int num_rows = row2 - row1 + 1;
    // System.out.println("num_rows : "+ num_rows);
    // System.out.println("num_cols : "+ cols.size());
    double[][] nam = new double[num_rows][cols.size()];
    for (int i = row1; i <= row2; i++) {
      for (int j = 0; j < cols.size(); j++) {
        // System.out.println("(" + (i-row1) + "," + cols.get(j) + ") -> (" + i
        // + ", " + j + ")");
        nam[i - row1][j] = am.get(i, cols.get(j));
      }
    }
    return new DenseMatrix(nam);
  }

  /**
   * Returns sub-matrix from the matrix `am`. It selects columns from `col1` to `col2` inclusive.  
   * It only selects and returns the rows listed in `rows`. Note that this function performs a 
   * deep copy twice. 
   * 
   * TODO: add to Matrix class
   * 
   * @param am
   * @param col1
   * @param col2
   * @param rows
   * @return DenseMatrix
   */
  public static DenseMatrix getSubMarix(Matrix am, int col1, int col2, List<Integer> rows) {
    int num_cols = col2 - col1 + 1;
    // System.out.println("num_rows : "+ rows.size());
    // System.out.println("num_cols : "+ num_cols);
    double[][] nam = new double[rows.size()][num_cols];
    for (int i = 0; i < rows.size(); i++) {
      for (int j = col1; j <= col2; j++) {
        // System.out.println("(" + rows.get(i) + "," + j + ") -> (" + i + ", "
        // + (j-col1) + ")");
        nam[i][j - col1] = am.get(rows.get(i), j);
      }
    }
    return new DenseMatrix(nam);
  }

  /**
   * Returns a single column from the matrix `am`. It selects column `col1`
   * and all rows from `row1` to `row2` inclusive.  
   * 
   * @param am
   * @param col1
   * @param row1
   * @param row2
   * @return returns a DenseMatrix that represents the column matrix. 
   */
  public static DenseMatrix getColumn(Matrix am, int col1, int row1, int row2) {
    List<Integer> cols = new ArrayList<Integer>();
    cols.add(col1);
    return getSubMarix(am, cols, row1, row2);
  }

  /**
   * Checks if two matrixes `a` and `b` are equal. These two matrices are not equal 
   * if at least one elemnt has an absolute difference equal to or greater than `eps`. 
   * 
   * @param a
   * @param b
   * @param eps
   * @return true if equal otherwise false
   */
  public static boolean isEqual(DenseMatrix a, DenseMatrix b, double eps) {
    if (a.numColumns() != b.numColumns())
      return false;
    if (a.numRows() != b.numRows())
      return false;
    for (int i = 0; i < a.numRows(); i++) {
      for (int j = 0; j < a.numColumns(); j++) {
        // System.out.println(a.get(i, j) + " - " + b.get(i, j));
        if (FastMath.abs(a.get(i, j) - b.get(i, j)) >= eps)
          return false;
      }
    }
    return true;
  }

  /**
   * Convert a double `a` to its string representation. Use a maximum and 
   * minimum of  `num_digits` decimal digits to represent the number. 
   * 
   * @param a
   * @param num_digits
   * @return string representation of the double
   */
  public static String floatToString(double a, int num_digits) {
    //System.out.print(a.get(i, j) + ", ");
    //System.out.printf("%f, ", a.get(i, j));
    //System.out.println(new BigDecimal(a.get(i, j)).toPlainString());
    //System.out.printf("%.80f, ", a.get(i, j));
    DecimalFormat df = new DecimalFormat("#");
    df.setMinimumFractionDigits(num_digits);
    df.setMaximumFractionDigits(num_digits);
    return  df.format( a );
  }
  
  /**
   * This function prints out the matrix `a`.  Use this function to output the values
   * to an arbitrary number of decimal places. It allows us to output values so that 
   * they can be used elsewhere for checking.  This allows us to avoid truncaton errors
   * when we print out the values. 
   * 
   * @param a
   * @see floatToString
   */
  public static void printMat(Matrix a, int num_digits) {
    for (int i = 0; i < a.numRows(); i++) {
      System.out.println();
      for (int j = 0; j < a.numColumns(); j++) {
        String s = floatToString( a.get(i, j), num_digits );
        System.out.print(  s + ", " );
      }
    }
    System.out.println();
  }

  /**
   * 
   * This is an re-implementation of Caret's `internalEnumLC`. It checks if matrix `B` has
   * any colinear columns. It returns a list of lists. Each inner list consists of a set of column 
   * indexes. The first index represents the dependent column. The next indexes represent the 
   * columns that can be linearly combined to produce the dependent column. 
   * This method produces the regression coefficients eficiently but does not return them. 
   *
   * If the boolean `checkResults` is set to `true`, then each set of regression coefficients that 
   * are found are multiplied with the orginal Matrix's independent columns and cheked if it equals 
   * the original independent column. 
   * 
   * We identify and select the collinear coefficients if they are not zero. We assume zero if an
   * element has an absolute value less than `dropThreshold` (see zap). 
   * 
   * @see isEqual
   * @see zap
   * @see https://github.com/topepo/caret
   * @see http://topepo.github.io/caret/index.html
   * @see https://github.com/topepo/caret/blob/master/pkg/caret/R/findLinearCombos.R
   * @see https://en.wikipedia.org/wiki/Multicollinearity
   * @see https://en.wikipedia.org/wiki/Condition_number
   * @See https://github.com/topepo/caret/issues/607
   * 
   * @param B
   * @param dropThreshold
   * @param checkResults
   * @return sets of collinear columns of the matrix. The first element of each sublist is the dependent 
   * column. The rest are the indepedent ones.  
   */
  public static List<List<Integer>> collinear(DenseMatrix B, double dropThreshold, boolean checkResults) {
    // List<Integer> l = new ArrayList<Integer>();
    List<List<Integer>> l = new ArrayList<List<Integer>>();

    // R <- qr.R(qrObj) # extract R matrix
    QRP qr = QRP.factorize(B);
    DenseMatrix r = qr.getR();
    // numColumns <- dim(R)[2] # number of columns in R
    int numColumns = r.numColumns();
    // rank <- qrObj$rank # number of independent columns
    int rank = qr.getRank();
    System.out.println("Rank R = " + rank);
    // pivot <- qrObj$pivot # get the pivot vector
    // Matrix pivotm = qr.getP();
    int[] pivotm = qr.getPVector();

    // if (is.null(numColumns) || rank == numColumns)
    if ((numColumns == 0) || (rank == numColumns)) {
      // list() # there are no linear combinations
      // there are no linear combinations
      System.out.println("Empty: rank = " + rank);
      return l;
    } else {
      // p1 <- 1:rank
      // X <- R[p1, p1] # extract the independent columns
      System.out.println("R :\n" + r.toString());
      double[][] x = subTopLeftMatrix(r, rank, rank);
      DenseMatrix X = new DenseMatrix(x);
      System.out.println("X :\n" + X.toString());
      // Y <- R[p1, -p1, drop = FALSE] # extract the dependent columns
      double[][] y = subTopRightMatrix(r, rank, rank);
      DenseMatrix Y = new DenseMatrix(y);
      System.out.println("Y :\n" + Y.toString());
      // b <- qr(X) # factor the independent columns
      QRP bqr = QRP.factorize(X);
      DenseMatrix br = bqr.getR();
      DenseMatrix b = new DenseMatrix(y.length, y[0].length);
      br.solve(new DenseMatrix(y), b);
      System.out.println("b :\n" + b.toString());
      // b <- qr.coef(b, Y) # get regression coefficients of the dependent
      // columns
      // b[abs(b) < 1e-6] <- 0 # zap small values
      zap(b, 1e-6);
      System.out.println("zapped(b) :\n" + b.toString());

      // Testing y = br*s
      DenseMatrix yt = new DenseMatrix(y.length, y[0].length);
      br.mult(b, yt);
      System.out.println("yt :\n" + yt.toString());

      System.out.println("pivot :\n" + Arrays.toString(pivotm));
      System.out.println("br :\n" + br.toString());
      // # generate a list with one element for each dependent column
      // lapply(1:dim(Y)[2], function(i) c(pivot[rank + i], pivot[which(b[,i] !=
      // 0)]))

      int nColsy = Y.numColumns();
      for (int k = 0; k < nColsy; k++) {
        System.out.println("\n-------------------------------------------\n");
        List<Integer> depst = which(b, k);
        System.out.println("depst(" + k + ") = " + depst.toString());
        List<Integer> deps = new ArrayList<Integer>();
        for (int e : depst) {
          deps.add(pivotm[e]);
        }
        int indep = pivotm[k + rank];
        deps.add(0, indep);
        System.out.println("deps(" + k + ") = " + deps.toString());

        // Checking
        if (checkResults) {
          DenseMatrix idep = getColumn(B, indep, 0, B.numRows() - 1);
          System.out.println("check independent = \n" + idep.toString());
          System.out.println("(" + B.numRows() + " , " + B.numColumns() + ")");
          DenseMatrix ndep = getSubMarix(B, deps.subList(1, deps.size()), 0,
              B.numRows() - 1);
          System.out.println("check dependent = \n" + ndep.toString());
          System.out.println("coeffs = \n" + b.toString());
          //printMat(b);
          System.out.println("coeffs(" + b.numRows() + "," + b.numColumns()+ ")");
          DenseMatrix nb = getSubMarix(b, k, k, depst);
          System.out.println("ncoeffs = \n" + nb.toString());
          DenseMatrix C = new DenseMatrix(idep.numRows(), idep.numColumns());
          ndep.mult(nb, C);
          System.out.println("calculated dep = \n" + C.toString());
          assert (isEqual(idep, C, 1e-12));
          // No subtract available
          // ndep.multAdd(b, idep);
          // System.out.println("idep = \n" + idep.toString() );
        }
        l.add(deps);
      }
      return l;
    }
  }
  
  ////////////////////////////////////////////
  /////////// Test Code
  ///////////////////////////////////////////
  
  /**
   * Original test from Matrix Toolkit Java
   */
  public static void testRank1() {
    Matrix rand = Matrices.random(6, 4);

    Matrix A = new DenseMatrix(rand.numRows(), rand.numRows());
    rand.transBmult(rand, A);

    QRP qrp = QRP.factorize(A);

    assertEquals(Math.min(rand.numRows(), rand.numColumns()), qrp.getRank());
  }

  /**
   * Original test from Matrix Toolkit Java
   */
  public static void testRank2() {
    Matrix rand = Matrices.random(4, 6);

    Matrix A = new DenseMatrix(rand.numRows(), rand.numRows());
    rand.transBmult(rand, A);

    QRP qrp = QRP.factorize(A);

    assertEquals(Math.min(rand.numRows(), rand.numColumns()), qrp.getRank());
  }

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
  static public void checkRank(double threshold, double[][] am) {
    Array2DRowRealMatrix m = new Array2DRowRealMatrix(am, false); // use array, don't copy
    RRQRDecomposition qr = new RRQRDecomposition(m, threshold);
    int rank = qr.getRank(threshold);
    System.out.println("0 QR rank: " + rank);
    System.out.println("1 QR rank: " + getRankN(qr));

    Matrix A = new DenseMatrix(am);
    QRP qrp = QRP.factorize(A);
    int nrank = qrp.getRank();
    System.out.println("2 QR rank: " + nrank);
    // System.out.println("R: \n" + r.toString());

    SingularValueDecomposition sv2 = new org.apache.commons.math3.linear.SingularValueDecomposition(m);
    int svdRank = sv2.getRank();
    System.out.println("SVD rank: " + svdRank);

    assert (nrank == svdRank);
  }

  /**
   * Calculate the rank based on a precision EPS. 
   * Function found in Matrix Toolkit Java which uses 
   * netlib-java. 
   * 
   * @see https://github.com/fommil/netlib-java. 
   * @see no.uib.cipr.matrix.QRP.factor(Matrix A)
   * @see https://github.com/fommil/matrix-toolkits-java/blob/6157618bc86bcda3749af2a60bf869d8f3292960/src/main/java/no/uib/cipr/matrix/QRP.java
   */
  public static int getRankN(RRQRDecomposition qrp) {
    RealMatrix R = qrp.getR();
    int k = FastMath.min(R.getRowDimension(), R.getColumnDimension());
    final double EPS = 1e-12;
    int rank;
    for (rank = 0; rank < k; rank++) {
      if (Math.abs(R.getEntry(rank, rank)) < EPS)
        break;
    }
    return rank;
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
  static void combineLinear1(double[][] am) {
    // Coefficients
    NormalDistribution d1 = new NormalDistribution(1.0, 2.0);
    NormalDistribution d2 = new NormalDistribution(.05, 0.05);
    NormalDistribution d3 = new NormalDistribution(10.0, 0.1);
    double a = d1.sample();
    double b = d2.sample();
    double c = d3.sample();

    // Select columns
    java.util.Random r = new java.util.Random();
    int i1 = r.nextInt(am.length);
    int i2 = r.nextInt(am.length);
    int i3 = r.nextInt(am.length);
    int i4 = r.nextInt(am.length);

    // Generate linear combination
    // am[i1] = (am[i2] * a) + (am[i3] * b) - (am[i4]*c)
    DenseVector v1 = new DenseVector(am[i2]).scale(a);
    DenseVector v2 = new DenseVector(am[i3]).scale(b);
    DenseVector v3 = new DenseVector(am[i4]).scale(-c);
    Vector v4 = v1.add(v2).add(v3);
    for (int i = 0; i < am[i1].length; i++) {
      am[i1][i] = v4.get(i);
    }
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
  static Pair<DenseVector[], Integer> combineLinear2(
      int insert, int numrows, NormalDistribution[] dists, double coeff[]) {

    int len = dists.length;
    DenseVector[] cols = new DenseVector[len];
    for (int i = 0; i < len; i++) {
      if (i != insert)
        cols[i] = new DenseVector(dists[i].sample(numrows)).scale(coeff[i]);
    }

    cols[insert] = new DenseVector(numrows);
    for (int i = 0; i < len; i++) {
      if (i != insert)
        cols[insert].add(cols[i]);
    }
    
    Pair<DenseVector[], Integer> p = Pair.of(cols, insert);
    return p;
  }


  public static void basicTests() {

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
    checkRank(threshold, am);

    double[][] am1 = new double[3][];
    am1[0] = c1;
    am1[1] = c2;
    am1[2] = c3;
    checkRank(threshold, am1);

    double[][] am2 = new double[4][];
    am2[0] = c1;
    am2[1] = c4;
    am2[2] = c5;
    am2[3] = c6;
    checkRank(threshold, am2);

    double[][] am3 = new double[6][];
    am3[0] = c1;
    am3[1] = c2;
    am3[2] = c3;
    am3[3] = c4;
    am3[4] = c5;
    am3[5] = c6;
    checkRank(threshold, am3);

    double[][] am4 = new double[6][];
    am4[0] = c1;
    am4[1] = c2;
    am4[2] = c3;
    am4[3] = c4;
    am4[4] = c5;
    am4[5] = c6;
    combineLinear1(am4);
    checkRank(threshold, am4);

    double[][] am5 = new double[6][];
    am5[0] = c1;
    am5[1] = c2;
    am5[2] = c3;
    am5[3] = c4;
    am5[4] = c5;
    am5[5] = c6;
    combineLinear1(am5);
    combineLinear1(am5);
    checkRank(threshold, am5);

    for (int i = 0; i < 10; i++) {
      double[][] am6 = genColumns();
      checkRank(threshold, am6);
    }

    for (int i = 0; i < 10; i++) {
      double[][] am6 = genColumns();
      combineLinear1(am6);
      checkRank(threshold, am6);
    }

    for (int i = 0; i < 10; i++) {
      double[][] am6 = genColumns();
      combineLinear1(am6);
      combineLinear1(am6);
      checkRank(threshold, am6);
    }
  }

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
  public static List<List<Integer>> test3(Vector[] cols, double threshold) {
    DenseMatrix M = new DenseMatrix(cols);
    System.out.println("Testing M = \n" + M.toString());
    //printMat(M);
    return collinear(M, threshold, true);
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
  public static void test4(Vector[] cols, double threshold, String expectedResult) {
    List<List<Integer>> l = test3(cols, threshold);
    System.out.println(l.toString());
    assert (l.toString().equalsIgnoreCase(expectedResult));
  }

  public static void main(String[] args) {
    // basicTests();

    double[] c1 = new double[] { 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 };
    double[] c2 = new double[] { 1.0, 1.0, 1.0, 0.0, 0.0, 0.0 };
    double[] c3 = new double[] { 0.0, 0.0, 0.0, 1.0, 1.0, 1.0 };
    double[] c4 = new double[] { 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 };
    double[] c5 = new double[] { 0.0, 1.0, 0.0, 0.0, 1.0, 0.0 };
    double[] c6 = new double[] { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 };

    Vector v1 = new DenseVector(c1);
    Vector v2 = new DenseVector(c2);
    Vector v3 = new DenseVector(c3);
    Vector v4 = new DenseVector(c4);
    Vector v5 = new DenseVector(c5);
    Vector v6 = new DenseVector(c6);

    Double threshold = 1e-7;
    
    // Test 1 
    Vector[] cols1 = new Vector[]{ v1, v2, v3}; 
    test4(cols1, threshold, "[[1, 0, 2]]");
    
    // Test 2 
    Vector[] cols2 = new Vector[]{ v1, v4, v5, v6}; 
    test4(cols2, threshold, "[[3, 0, 1, 2]]");
    
    // Test 3 
    Vector[] cols3 = new Vector[]{ v1, v2, v3, v4, v6};
    test4(cols3, threshold, "[[1, 0, 2]]");
     
     // Test 4 
    Vector[] cols4 = new Vector[]{ v1, v2, v3, v4, v5, v6};
     test4(cols4, threshold, "[[1, 0, 2], [5, 0, 3, 4]]");
     
     NormalDistribution d1 = new NormalDistribution(1.0, 2.0);
     NormalDistribution d2 = new NormalDistribution(.05, 0.05);
     NormalDistribution d3 = new NormalDistribution(10.0, 0.1);
     NormalDistribution d4 = new NormalDistribution(100.0, 0.1);
     NormalDistribution d5 = new NormalDistribution(0.6, 0.3);
     
     // Test 5 
     NormalDistribution dists1[] = new NormalDistribution[]{d1, d2, d3, d4, d5}; 
     double coeff1[] = new double[]{ 1.0 , 2.0, 3.0, 4.0, 5.0 };
     Pair<DenseVector[], Integer> p = combineLinear2(0, 5, dists1, coeff1) ;
     //System.out.println(p.getLeft().toString());
     //System.out.println("Dependent idx = " + p.getRight());
     test3(p.getLeft(), threshold);
     
     // Test 6 
     NormalDistribution dists2[] = new NormalDistribution[]{d5, d3,d4, d2, d1}; 
     double coeff2[] = new double[]{ -10.0 , 20.0, 3.0, 4.0,-500.0 }; 
     Pair<DenseVector[], Integer> p2 = combineLinear2(3, 5, dists2,coeff2) ; 
     //System.out.println(p.getLeft().toString());
     //System.out.println("Dependent idx = " + p.getRight());
     test3(p2.getLeft(), threshold);
     
    double[] c11 = new double[] { 1.012, 10.01, 20.0211, 300.0303, 5.06, 6.06 };
    double[] c12 = new double[] { 0.033, 0.045, 0.022, 0.033, 0.045, 0.06 };
    double[] c13 = new double[] { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
    double[] c14 = new double[] { 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 };
    double[] c15 = new double[] { 66.06, 98.9, 107.707, 110.0119, 67.08, 88.87 };
    double[] c16 = new double[] { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 };

    Vector v11 = new DenseVector(c11);
    Vector v12 = new DenseVector(c12);
    Vector v13 = new DenseVector(c13);
    Vector v14 = new DenseVector(c14);
    Vector v15 = new DenseVector(c15);
    Vector v16 = new DenseVector(c16);

    Vector v11t = v11.copy().scale(56.03);
    Vector v12t = v12.copy().scale(89.309);
    Vector v15t = v15.copy().scale(0.9077);
    v13.add(v11t).add(v12t).add(v15t);

    // Test 7
    Vector[] cols7 = new Vector[] { v11, v12, v13, v14, v15 };
    test4(cols7, threshold, "[[1, 2, 4, 0]]");
  }

}

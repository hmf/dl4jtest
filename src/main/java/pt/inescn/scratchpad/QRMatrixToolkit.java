package pt.inescn.scratchpad;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import no.uib.cipr.matrix.DenseMatrix;
import no.uib.cipr.matrix.Matrix;
import no.uib.cipr.matrix.QRP;

import org.apache.commons.math3.linear.RRQRDecomposition;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.util.FastMath;
//import org.apache.commons.math3.linear.QRDecomposition;
//import org.apache.commons.math3.linear.RealMatrix;
//import org.apache.commons.math3.linear.DefaultRealMatrixChangingVisitor;

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
  
  static final boolean debug = false;
  
  public static void printd(String s) {
    if (debug)
      System.out.print(s);
  }
  
  public static void printlnd(String s) {
    if (debug)
      System.out.println(s);
  }
  


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
    // printlnd("num_rows : "+ num_rows);
    // printlnd("num_cols : "+ cols.size());
    double[][] nam = new double[num_rows][cols.size()];
    for (int i = row1; i <= row2; i++) {
      for (int j = 0; j < cols.size(); j++) {
        // printlnd("(" + (i-row1) + "," + cols.get(j) + ") -> (" + i + ", " + j + ")");
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
    // printlnd("num_rows : "+ rows.size());
    // printlnd("num_cols : "+ num_cols);
    double[][] nam = new double[rows.size()][num_cols];
    for (int i = 0; i < rows.size(); i++) {
      for (int j = col1; j <= col2; j++) {
        // printlnd("(" + rows.get(i) + "," + j + ") -> (" + i + ", "
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
   * if at least one element has an absolute difference equal to or greater than `eps`. 
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
        // printlnd(a.get(i, j) + " - " + b.get(i, j));
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
    //printd(a.get(i, j) + ", ");
    //System.out.printf("%f, ", a.get(i, j));
    //printlnd(new BigDecimal(a.get(i, j)).toPlainString());
    //System.out.printf("%.80f, ", a.get(i, j));
    DecimalFormat df = new DecimalFormat("#");
    df.setMinimumFractionDigits(num_digits);
    df.setMaximumFractionDigits(num_digits);
    return  df.format( a );
  }
  
  /**
   * This function prints out the matrix `a`.  Use this function to output the values
   * to an arbitrary number of decimal places. It allows us to output values so that 
   * they can be used elsewhere for checking.  This allows us to avoid truncation errors
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
   * This method produces the regression coefficients efficiently but does not return them. 
   *
   * If the boolean `checkResults` is set to `true`, then each set of regression coefficients that 
   * are found are multiplied with the original Matrix's independent columns and checked if it equals 
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
   * column. The rest are the independent ones.  
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
    printlnd("Rank R = " + rank);
    // pivot <- qrObj$pivot # get the pivot vector
    // Matrix pivotm = qr.getP();
    int[] pivotm = qr.getPVector();

    // if (is.null(numColumns) || rank == numColumns)
    if ((numColumns == 0) || (rank == numColumns)) {
      // list() # there are no linear combinations
      // there are no linear combinations
      printlnd("Empty: rank = " + rank);
      return l;
    } else {
      // p1 <- 1:rank
      // X <- R[p1, p1] # extract the independent columns
      printlnd("R :\n" + r.toString());
      double[][] x = subTopLeftMatrix(r, rank, rank);
      DenseMatrix X = new DenseMatrix(x);
      printlnd("X :\n" + X.toString());
      // Y <- R[p1, -p1, drop = FALSE] # extract the dependent columns
      double[][] y = subTopRightMatrix(r, rank, rank);
      DenseMatrix Y = new DenseMatrix(y);
      printlnd("Y :\n" + Y.toString());
      // b <- qr(X) # factor the independent columns
      QRP bqr = QRP.factorize(X);
      DenseMatrix br = bqr.getR();
      DenseMatrix b = new DenseMatrix(y.length, y[0].length);
      br.solve(new DenseMatrix(y), b);
      printlnd("b :\n" + b.toString());
      // b <- qr.coef(b, Y) # get regression coefficients of the dependent
      // columns
      // b[abs(b) < 1e-6] <- 0 # zap small values
      zap(b, 1e-6);
      printlnd("zapped(b) :\n" + b.toString());

      // Testing y = br*s
      DenseMatrix yt = new DenseMatrix(y.length, y[0].length);
      br.mult(b, yt);
      printlnd("yt :\n" + yt.toString());

      printlnd("pivot :\n" + Arrays.toString(pivotm));
      printlnd("br :\n" + br.toString());
      // # generate a list with one element for each dependent column
      // lapply(1:dim(Y)[2], function(i) c(pivot[rank + i], pivot[which(b[,i] != 0)]))

      int nColsy = Y.numColumns();
      for (int k = 0; k < nColsy; k++) {
        printlnd("\n-------------------------------------------\n");
        List<Integer> depst = which(b, k);
        printlnd("depst(" + k + ") = " + depst.toString());
        List<Integer> deps = new ArrayList<Integer>();
        for (int e : depst) {
          deps.add(pivotm[e]);
        }
        int indep = pivotm[k + rank];
        deps.add(0, indep);
        printlnd("deps(" + k + ") = " + deps.toString());

        // Checking
        if (checkResults) {
          DenseMatrix idep = getColumn(B, indep, 0, B.numRows() - 1);
          printlnd("check independent = \n" + idep.toString());
          printlnd("(" + B.numRows() + " , " + B.numColumns() + ")");
          DenseMatrix ndep = getSubMarix(B, deps.subList(1, deps.size()), 0,
              B.numRows() - 1);
          printlnd("check dependent = \n" + ndep.toString());
          printlnd("coeffs = \n" + b.toString());
          //printMat(b);
          printlnd("coeffs(" + b.numRows() + "," + b.numColumns()+ ")");
          DenseMatrix nb = getSubMarix(b, k, k, depst);
          printlnd("ncoeffs = \n" + nb.toString());
          DenseMatrix C = new DenseMatrix(idep.numRows(), idep.numColumns());
          ndep.mult(nb, C);
          printlnd("calculated dep = \n" + C.toString());
          assert (isEqual(idep, C, 1e-12));
          // No subtract available
          // ndep.multAdd(b, idep);
          // printlnd("idep = \n" + idep.toString() );
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

  
  public static void main(String[] args) {
  }

}

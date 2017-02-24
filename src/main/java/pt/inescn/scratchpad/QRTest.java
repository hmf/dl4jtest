package pt.inescn.scratchpad ;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.OpenMapRealMatrix;
import org.apache.commons.math3.linear.RRQRDecomposition;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.random.RandomDataGenerator;

/**
 * QRTest tests the use of QR decomposition in the Apache Commons Math
 * library to find a basis for the null space (kernel) of a sparse matrix.
 *
 * If the target matrix is A, the decomposition is done to its transpose A':
 *   A'P = QR
 * where P is a square pivot matrix, Q is a square orthogonal matrix (Q'Q = I)
 * and R is upper triangular.
 *
 * Note that it is imperative that the "rank revealing" version of QR
 * decomposition is used.
 * @author Paul A. Rubin (rubin@msu.edu)
 * 
 * @see https://gitlab.msu.edu/orobworld/QRTest/blob/master/src/qrtest/QRTest.java
 * sbt "run-main pt.inescn.scratchpad.QRTest" 
 */
public final class QRTest {

  /**
   * Constructor.
   * This doesn't do anything; it just makes the CheckStyle style checker
   * feel better about things.
   */
  private QRTest() { }

  /**
   * Generate a sparse matrix, decompose it, and identify a basis of the
   * kernel of the matrix.
   *
   * To experiment, modify the parameter settings at the top of the main
   * method.
   * @param args the command line arguments
   */
  @SuppressWarnings({ "checkstyle:magicnumber" })
  public static void main(final String[] args) {
    // set the parameters for the test
    int rows = 12;          // # of rows
    int cols = 20;          // # of columns
    long seed = 981L;       // random number seed
    double density = 0.2;   // target density of the matrix
    double absval = 3.0;    // maximum absolute value of any matrix entry
    double fuzz = 1e-7;     // rounding allowance
    boolean tweak = false;  // if true, replace one column of the test matrix
                            // with a linear combination of the other columns
                            // to ensure that the kernel is not just {0}
    // generate a test matrix (A)
    OpenMapRealMatrix aMatrix = generate(rows, cols, seed, density, absval, tweak);
    // sanity check: report the dimensions of A
    System.out.printf("The dimensions of A are %d x %d.\n",
                      aMatrix.getRowDimension(), aMatrix.getColumnDimension());
    // decompose the transpose A'
    RRQRDecomposition qr = new RRQRDecomposition(aMatrix.transpose());
    // get the factors
    RealMatrix qMatrix = qr.getQ();
    RealMatrix rMatrix = qr.getR();
    System.out.printf("The dimensions of Q are %d x %d.\n",
                      qMatrix.getRowDimension(), qMatrix.getColumnDimension());
    System.out.printf("The dimensions of R are %d x %d.\n",
                      rMatrix.getRowDimension(), rMatrix.getColumnDimension());
    System.out.println();
    // sanity check: confirm orthogonality
    RealMatrix eye = new Array2DRowRealMatrix(cols, cols);  // identity matrix
    for (int i = 0; i < cols; i++) {
      eye.setEntry(i, i, 1.0);
    }
    System.out.printf("The difference between Q'Q and I has sup norm %f.\n",
                      eye.subtract(qMatrix.transpose().multiply(qMatrix))
                         .getNorm());
    // get the rank of A
    int rank = qr.getRank(fuzz);
    System.out.printf("The reported rank of A = %d.\n", rank);
    // confirm that the last n - r columns of Q belong to the kernel of A,
    // where n is the column dimension of A and r is the rank of A
    System.out.println("\nChecking kernel columns ...");
    RealMatrix zMatrix = aMatrix.multiply(qMatrix);
    for (int c = rank; c < cols; c++) {
      System.out.printf("The product of A with column %d of Q has sup "
                        + "norm %f.\n",
                        c, zMatrix.getColumnMatrix(c).getNorm());
    }
  }

  /**
   * Generate a random matrix instance.
   * Note: There are no validity tests for the arguments; the onus for using
   * legitimate values (e.g., density between 0 and 1) is on the user.
   * @param rows the desired number of rows
   * @param cols the desired number of columns
   * @param seed a random number seed
   * @param density the target density for the matrix
   * @param absval the maximum absolute value of any matrix entry
   * @param tweak if true, replace one column with a linear combination
   * of the other columns
   * @return a sparse matrix containing random entries.
   */
  private static OpenMapRealMatrix generate(final int rows,
                                            final int cols,
                                            final long seed,
                                            final double density,
                                            final double absval,
                                            final boolean tweak) {
    // create an empty matrix
    OpenMapRealMatrix matrix = new OpenMapRealMatrix(rows, cols);
    // create a random number generator
    RandomDataGenerator rng = new RandomDataGenerator();
    rng.reSeed(seed);
    // compute the desired number of nonzero entries
    long nnz = Math.round(rows * cols * density);
    // fill in that many cells randomly
    while (nnz > 0) {
      // randomly choose a cell
      int r = rng.nextInt(0, rows - 1);
      int c = rng.nextInt(0, cols - 1);
      if (matrix.getEntry(r, c) == 0) {
        matrix.setEntry(r, c, rng.nextUniform(-absval, absval));
        nnz--;
      }
    }
    // if requested, randomly replace one column with a linear combination of
    // the others to ensure less than full rank
    if (tweak) {
      // pick a column to replace
      int c = rng.nextInt(0, cols - 1);
      System.out.printf("Making column %d of A a linear combination of "
                        + "other columns ...\n", c);
      // generate weights for a linear combination of the other columns
      double[] combo = new double[cols];
      for (int i = 0; i < cols; i++) {
        combo[i] = rng.nextUniform(0, 1);
      }
      combo[c] = 0;
      // compute that linear combination
      double[] delta =
        matrix.multiply(new Array2DRowRealMatrix(combo)).getColumn(0);
      // replace the target column
      for (int r = 0; r < rows; r++) {
        matrix.setEntry(r, c, delta[r]);
      }
    }
    return matrix;
  }

}

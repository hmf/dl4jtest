package pt.inescn.scratchpad;

import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.linear.RRQRDecomposition;
import org.apache.commons.math3.linear.Array2DRowRealMatrix;
//import org.apache.commons.math3.linear.DefaultRealMatrixChangingVisitor;

public class QRIssue {

  public static void main(String[] args) {
    
    double[][] am = new double[5][];
    double[] c1 = new double[] {1.0, 1.0, 1.0, 1.0, 1.0, 1.0} ; 
    double[] c2 = new double[] {1.0, 1.0, 1.0, 0.0, 0.0, 0.0} ;
    double[] c3 = new double[] {0.0, 0.0, 0.0, 1.0, 1.0, 1.0} ;
    double[] c4 = new double[] {1.0, 0.0, 0.0, 1.0, 0.0, 0.0 } ;
    double[] c6 = new double[] {0.0, 0.0, 1.0, 0.0, 0.0, 1.0 } ;      
    
    am[0] = c1 ;
    am[1] = c2 ;
    am[2] = c3 ;
    am[3] = c4 ;
    am[4] = c6 ;
    
    Array2DRowRealMatrix m = new Array2DRowRealMatrix( am, false )  ; // use array, don't copy 
    RRQRDecomposition qr = new RRQRDecomposition( m, 1e-1 ) ;
    RealMatrix r = qr.getR() ;
    int numColumns = r.getColumnDimension() ;
    int rank = qr.getRank( 1e-1 ) ;
    System.out.println("QR rank: " + rank) ;
    System.out.println("QR is singular: " + !qr.getSolver().isNonSingular()) ;
    
    /*
    import org.apache.commons.math3.linear.SingularValueDecomposition
    val sv2 = new org.apache.commons.math3.linear.SingularValueDecomposition(m);
    //assertEquals(379, sv2.getRank());//this is OK      
    println(s"SVD rank: ${sv2.getRank()}")*/
}
}

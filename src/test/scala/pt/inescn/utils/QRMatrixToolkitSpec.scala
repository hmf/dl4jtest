package pt.inescn.utils

import org.scalatest._

import collection.mutable.Stack
import org.scalatest._

//import pt.inescn.utils.TestUtils._

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
class QRMatrixToolkitSpec extends FlatSpec with Matchers {

  "check rank" should "get thin matrix rank number for linear independent columns" in {
    
    val rand = Matrices.random( 6, 4 )
    val A = new DenseMatrix( rand.numRows(), rand.numRows() )
    rand.transBmult( rand, A )
    val qrp = QRP.factorize( A )

    Math.min( rand.numRows(), rand.numColumns() ) shouldBe qrp.getRank()
  }

  it should "get fat matrix rank number for linear independent columns" in {
    val rand = Matrices.random(4, 6)
    val A = new DenseMatrix(rand.numRows(), rand.numRows())
    rand.transBmult(rand, A)
    val qrp = QRP.factorize(A)

    Math.min(rand.numRows(), rand.numColumns()) shouldBe qrp.getRank()
  }

}
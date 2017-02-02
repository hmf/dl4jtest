package pt.inescn.scratchpad.examples

/**
 *
 * @see http://www.scala-lang.org/old/node/1403
 * @see http://blog.fogus.me/2009/03/26/baysick-a-scala-dsl-implementing-basic/
 * @see https://github.com/fogus/baysick/tree/master
 * @see http://blog.siddhuw.info/writing-dsls-using-scala-part-1-underlying-concepts/
 * @see http://www.devx.com/enterprise/introduction-to-internal-dsls-in-scala.html
 *
 * Example in http://debasishg.blogspot.pt/2008/05/designing-internal-dsls-in-scala.html
 * TradeDSL
 * 
 * sbt "run-main pt.inescn.scratchpad.examples.DSL_2"
 */
object DSL_2 {

  abstract class Instrument( name: String ) { def stype: String }
  case class Stock( name: String ) extends Instrument( name ) {
    override val stype = "equity"
  }
  case class Bond( name: String ) extends Instrument( name ) {
    override val stype = "bond"
  }

  abstract class TransactionType { def value: String }
  case class buyT() extends TransactionType {
    override val value = "bought"
  }
  case class sellT() extends TransactionType {
    override val value = "sold"
  }

  class PimpedInt( qty: Int ) {
    def sharesOf( name: String ) = {
      ( qty, Stock( name ) )
    }

    def bondsOf( name: String ) = {
      ( qty, Bond( name ) )
    }
  }

  import scala.language.implicitConversions

  implicit def pimpInt( i: Int ) = new PimpedInt( i )

  class Order {
    var price = 0
    var ins: Instrument = null
    var qty = 0;
    var totalValue = 0
    var trn: TransactionType = null
    var account: String = null

    def to( i: Tuple3[ Instrument, Int, TransactionType ] ) = {
      ins = i._1
      qty = i._2
      trn = i._3
      this
    }
    def maxUnitPrice( p: Int ) = { price = p; this }

    def using( pricing: ( Int, Int ) => Int ) = {
      totalValue = pricing( qty, price )
      this
    }

    def forAccount( a: String )( implicit pricing: ( Int, Int ) => Int ) = {
      account = a
      totalValue = pricing( qty, price )
      this
    }
  }
  
  def buy( qi: Tuple2[ Int, Instrument ] ) = ( qi._2, qi._1, buyT() )
  def sell( qi: Tuple2[ Int, Instrument ] ) = ( qi._2, qi._1, sellT() )

  def main( args: Array[ String ] ) {
 def premiumPricing(qty: Int, price: Int) = qty match {
      case q if q > 100 => q * price - 100
      case _ => qty * price
    }

    def defaultPricing(qty: Int, price: Int): Int = qty * price

    val orders = List[Order](

      new Order to buy(100 sharesOf "IBM")
                maxUnitPrice 300
                using premiumPricing,

      new Order to buy(200 sharesOf "CISCO")
                maxUnitPrice 300
                using premiumPricing,

      new Order to buy(200 sharesOf "GOOGLE")
                maxUnitPrice 300
                using defaultPricing,

      new Order to sell(200 bondsOf "Sun")
                maxUnitPrice 300
                using {
                  (qty, unit) => qty * unit - 500
                }
    )
    println((0 /: orders)(_ + _.totalValue))
  }
}
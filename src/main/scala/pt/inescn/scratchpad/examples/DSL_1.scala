package pt.inescn.scratchpad.examples

/**
 *
 * @see http://www.scala-lang.org/old/node/1403
 * @see http://blog.fogus.me/2009/03/26/baysick-a-scala-dsl-implementing-basic/
 * @see https://github.com/fogus/baysick/tree/master
 * @see http://blog.siddhuw.info/writing-dsls-using-scala-part-1-underlying-concepts/
 * @see http://www.devx.com/enterprise/introduction-to-internal-dsls-in-scala.html
 *
 * Example in http://www.devx.com/enterprise/introduction-to-internal-dsls-in-scala.html
 *
 *
 */
object DSL_1 {

  trait Currency {
    def getCode: String
  }

  object Currency {
    object USD extends Currency {
      val getCode = "USD"
    }

    object EUR extends Currency {
      val getCode = "EUR"
    }

    object GBP extends Currency {
      val getCode = "GBP"
    }

    def apply( s: String ): Currency = s.toUpperCase match {
      case "USD" => USD
      case "EUR" => EUR
      case "GBP" => GBP
    }

    type Conversion = Map[ ( Currency, Currency ), BigDecimal ]

    case class Converter( conversion: Conversion ) extends {
      def convert( from: Currency, to: Currency ): BigDecimal = {
        if ( from == to ) 1
        else conversion.getOrElse( ( from, to ), 1 / conversion( ( to, from ) ) )
      }
    } // Converter

    case class OMoney( amount: BigDecimal, currency: Currency, converter: Converter ) {

      def +( thatOMoney: OMoney ): OMoney = performOperation( thatOMoney, _ + _ )

      def performOperation( thatOMoney: OMoney, operation: ( BigDecimal, BigDecimal ) => BigDecimal ): OMoney = {
        thatOMoney match {
          case OMoney( v, c, conv ) if c == currency => OMoney( operation( amount, v ), currency, conv )
          case OMoney( v, c, _ )                     => performOperation( thatOMoney.to( currency ), operation )
        }
      }

      def to( thatCurrency: Currency ): OMoney = {
        val rate = converter.convert( currency, thatCurrency )
        OMoney( amount * rate, thatCurrency, converter )
      }

    } // OMoney

    case class Money( amount: BigDecimal, currency: Currency )( implicit converter: Converter ) {

      def +( thatMoney: Money ): Money = performOperation( thatMoney, _ + _ )

      def performOperation( thatMoney: Money, operation: ( BigDecimal, BigDecimal ) => BigDecimal ): Money = {
        thatMoney match {
          case Money( v, c ) if c == currency => Money( operation( amount, v ), currency )
          case Money( v, c )                  => performOperation( thatMoney.to( currency ), operation )
        }
      }

      def to( thatCurrency: Currency ): Money = {
        val rate = converter.convert( currency, thatCurrency )
        Money( amount * rate, thatCurrency )
      }

    } // Money

    implicit class BigDecimalOps( value: BigDecimal ) {
      def apply( currency: Currency )( implicit converter: Converter ): Money = Money( value, currency )
    }

    implicit class IntOps( value: Int ) {
      def apply( currency: Currency )( implicit converter: Converter ): Money = ( value: BigDecimal ).apply( currency )
    }

    implicit class DoubleOps( value: Double ) {
      def apply( currency: Currency )( implicit converter: Converter ): Money = ( value: BigDecimal ).apply( currency )
    }
  }

  def main( args: Array[ String ] ) {

    import Currency._
    import DSL_1._

    val conversion: Conversion = Map(
      ( GBP, EUR ) -> 1.39,
      ( EUR, USD ) -> 1.08,
      ( GBP, USD ) -> 1.5 )

    //val converter = Converter( conversion )
    implicit val converter = Converter(conversion) 

    // result is 79.8 USD
    val result = OMoney( 42, USD, converter ) + OMoney( 35, EUR, converter )
    println( result.amount )

    // resultToPound is 53.2 GBP
    val resultToPound = result.to( GBP )
    println( resultToPound.amount )

    // DSL Land
    val dollars = 100(USD) 
    val euros = 42.24(EUR) 
    
    val result1 = 42( USD ) + 35( EUR )
    val resultToPound1 = result1 to GBP
    println(resultToPound.amount)
  }

}
package pt.inescn.scratchpad.debug

object FlatMapGenerics {
  import scala.language.implicitConversions

  object Test {
    /* Ok
    implicit def duplicate0[ X ]( hc: Stream[ X ] ): Stream[ _ ] = {
      val crss = hc.flatMap { x: X => List( x, x ) }
      crss
    }*/

    /* BUG - recursion
     * @see https://groups.google.com/forum/#!topic/scala-user/58p-VbFEcu8
    // Ok... but
    // Need to add X to Stream[_] output
    // If we add another function with the same signature, we get an error
    // The implicitis interact !! They can be chained
    implicit def duplicate1[ X, H[ _ ] ]( hc: H[ X ] ): Stream[ X ] = {
      val crss = hc.flatMap { x: X => List( x, x ) }
      crss
    }*/

    /*
    implicit def duplicate1[ X, H[ _ ] <: scala.collection.AbstractSeq[X] ]( hc: H[ X ] ): Stream[ X ] = {
      val crss = hc.toStream.flatMap { x: X => List( x, x ) }
      crss
    }*/

    import scala.language.higherKinds
    /*
    implicit def duplicate1[ X, H[ _ ] <: scala.collection.AbstractSeq[X] ]( hc: H[ X ] ): H[ X ] = {
      val crss = hc.flatMap { x: X => List( x, x ) }
      crss
    }*/
    /*
    implicit def duplicate1[ X, H[ _ ] <: List[X] ]( hc: H[ X ] ): H[ X ] = {
      val crss = hc.flatMap { x: X => List( x, x ) }
      crss
    }*/

    /*
    // If we ensure that H[X] = Stream[X] why do we get:
    // type mismatch in return:  crss.type (with underlying type scala.collection.immutable.Stream[X]) not H[X]
    import scala.language.higherKinds
    implicit def duplicate2[ X, H[ _ ] ]( hc: H[ X ] )(implicit ev: H[X] =:= Stream[X]): H[ X ] = {
      val crss = hc.flatMap { x: X => List( x, x ) }
      crss
    }*/

    /*
    import scala.language.higherKinds
    // Must use >:, if <: then error: overloaded method value flatMap with alternatives
    //implicit def duplicate3[ X, H[ _ ] >: Seq[ _ ] ]( hc: H[ X ] ) : Seq[X]= {
    implicit def duplicate3[ X, H[ _ ] >: Stream[ _ ] ]( hc: H[ X ] ) : Seq[X]= {
    //implicit def duplicate3[ X, H[ _ ] >: Stream[ _ ] ]( hc: H[ X ] ) : Stream[X]= {
    //implicit def duplicate3[ X, H[ _ ] <: Stream[ _ ] ]( hc: H[ X ] ) : Stream[X]= {
      val crss = hc.flatMap { x: X => List(x,x) }
      crss
    }*/

    /*
    import scala.language.higherKinds
    // Must use >:, if <: then error: overloaded method value flatMap with alternatives
    //implicit def duplicate3[ X, H[ _ ] >: Stream[ _ ] ]( hc: H[ X ] ) : Stream[X] = {
    implicit def duplicate3[ X, H[ _ ] >: Stream[ _ ] ]( hc: H[ X ] ) : Int = {
    //implicit def duplicate3[ X, H[ _ ] <: Stream[ _ ] ]( hc: H[ X ] ) : Int = {
      def doMore(h : H[X]) : Stream[X] = {
        val crss = h.flatMap { x: X => List(x,x) }
        crss
      }
      doMore(hc)
      1
    }*/

    /*
    //implicit def duplicate3[ X, H[ _ ] >: Stream[ _ ] ]( hc: H[ X ] ) : Int = {
    implicit def duplicate3[ X, H[ _ ] >: scala.collection.immutable.Stream[ _ ] ]( hc: H[ X ] ) : Int = {
      def doMore[Y, G[_] >: scala.collection.immutable.Stream[_] ](g : G[Y]) : scala.collection.immutable.Stream[Y] = {
        val crss = g.flatMap { x: X => List(x,x) }
        crss
      }
      doMore(hc)
      1
    }*/

    /*
    def duplicate4[ X ]( hc: scala.collection.immutable.Stream[ X ] ): Int = {
      def doMore[ Y ]( g: scala.collection.immutable.Stream[ Y ] ): scala.collection.immutable.Stream[ Y ] = {
        val crss = g.flatMap { y: Y => List( y, y ) }
        crss
      }
      doMore( hc )
      1
    }
*/
    //import scala.collection.immutable.Stream

    /*
    def duplicate5[ X ]( hc: Stream[ X ] ): Int = {
      def doMore[ Y ]( g: Stream[ Y ] ): Stream[ Y ] = {
        val crss = g.flatMap { y: Y => List( y, y ) }
        crss
      }
      doMore( hc )
      1
    }
*/

    //def f[ A: Ordering ]( a: A, b: A ) = if ( implicitly[ Ordering[ A ] ].lt( a, b ) ) a else b

    /*
    def duplicate6[ X ]( hc: X )(implicit ev : Stream[X]): Int = {
      def doMore[ Y]( g: Y )(implicit iev : Stream[Y]): Stream[ Y ] = {
        val crss =  iev(g).flatMap { y: Y => List( y, y ) }
        crss
      }
      doMore( hc )
      1
    }*/

    implicit def listToStream[ X ]( l: List[ X ] ) = l.toStream

    def duplicate8[ X, CC ]( hc: CC )( implicit conv: CC => Stream[ X ] ): Stream[ X ] = {
      val crss = hc.flatMap { x: X => List( x, x ) }
      crss
    }

    val v1 = duplicate8( List( 1, 2, 3 ) )
    val v2 = duplicate8( Stream( 1, 2, 3 ) )

    /*
    def duplicate7[ X: Stream ]( hc: X ): Int = {
      def doMore[ Y: Stream ]( g: Y ): Stream[ Y ] = {
        val yy = implicitly[ Stream[ Y ] ]
        val crss =  yy.flatMap { y: Y => List( y, y ) }
        //val crss =  g.flatMap { y: Y => List( y, y ) }
        crss
      }
      doMore( hc )
      1
    }
*/
    /*
    // overloaded method value flatMap with alternatives
    import scala.language.higherKinds
    implicit def duplicate[ X, H[ _ ] <: Stream[ _ ] ]( hc: H[ X ] ) : Stream[X]= {
      val crss = hc.flatMap { x: X => List(x,x) }
      crss
    }*/

    /*
     * @see https://github.com/azavea/numeric/blob/master/plugin/src/test/scala/Example.scala 
     * @seehttp://rudairandamacha.blogspot.pt/2012/11/saddling-horse-with-type-classes-in.html
     * @see https://github.com/afwlehmann/typeclasses-intro
     */
    trait TA {
      type T
      def get: T
    }

    import pt.inescn.scratchpad.examples.HList._
    import pt.inescn.scratchpad.examples.HList
    import pt.inescn.scratchpad.examples.HCons
    import pt.inescn.scratchpad.examples.HNil

    class TB extends TA {
      type T = Int :: String :: HNil.type
      def get = 1 :: "2" :: HNil
    }

    def dynamic = {
      new TB
    }

    def dynamic0 = {
      new TA {
        type T = Int :: String :: HNil.type
        def get = 1 :: "2" :: HNil
      }
    }

    def dynamic1 = {
      // If we use only new TB we are creating a new "dynamic" class
      // Cannot extend, only with
      new TB with TA {
        override type T = Int :: String :: HNil.type
        override def get = 1 :: "2" :: HNil
      }
    }

    trait Model {
      trait ParamsRange {
        type P
        def get: P
      }

      def getParamRanges: ParamsRange
    }


    // Not the solution
    class M extends Model {
      def getParamRanges : ParamsRange = {
        new ParamsRange {
          type P = String :: Int :: HNil.type
          def get = "1" :: 2 :: HNil
        }
      }
    }
    
    // Second best solution
    class MA extends Model {
      // If you explicilty type the retun as ParamsRange
      // then the Model trait's version of  ParamsRange is used 
      def getParamRanges = {
        new ParamsRange {
          type P = String :: Int :: HNil.type
          def get : P = "1" :: 2 :: HNil
        }
      }
    }

    // Best solution
    class MB extends Model {
      class PR extends ParamsRange {
        type P = String :: Int :: HNil.type
        def get = "1" :: 2 :: HNil
      }

      val prs = new PR
      def getParamRanges : PR = prs // no need to explicitly type the return
    }

  }

  def main( args: Array[ String ] ) {
    import Test._

    val v1 = new TB
    val v1g = v1.get
    val v1gp1 = v1g.head
    val v1gp2 = v1g.tail.head

    val v2 = dynamic
    val v2g = v2.get
    val v2gp1 = v2g.head
    val v2gp2 = v2g.tail.head

    val v3 = dynamic0
    val v3g = v3.get
    val v3gp1 = v3g.head
    val v3gp2 = v3g.tail.head

    val v4 = dynamic1
    val v4g = v4.get
    val v4gp1 = v4g.head
    val v4gp2 = v4g.tail.head

    val m = new M
    val mr = m.getParamRanges
    val mrg = mr.get
    // val mrg_v1 = mrg.head // does not compile

    val ma = new MA
    val mar = ma.getParamRanges
    val marg = mar.get  // Type not explcitily visible
    val marg_v1 = marg.head // Type not explcitily visible

    val mb = new MB
    val mbr = mb.getParamRanges
    val mbrg = mbr.get // Type not explcitily visible
    val mbrg_v1 = mbrg.head // Type not explcitily visible
  }

}
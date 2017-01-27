package pt.inescn.scratchpad.examples

/**
 *
 *  sbt "run-main pt.inescn.scratchpad.examples.CartesianProductV3"
 *
 * @see http://stackoverflow.com/questions/16219545/scala-cross-cartesian-product-with-multiple-sources-and-heterogeneous-types
 * @see https://github.com/msavva/transphoner/blob/master/libtransphone/src/main/scala/org/babysherlock/util/EnrichedCollections.scala
 */
object CartesianProductV3 {

  trait Crosser[ A, B, C ] {
    def cross( as: Stream[ A ], bs: Stream[ B ] ): Stream[ C ]
  }

  trait LowPriorityCrosserImplicits {
    private type T[ X ] = Stream[ X ]

    implicit def crosser2[ A, B ] = new Crosser[ A, B, ( A, B ) ] {
      def cross( as: T[ A ], bs: T[ B ] ): T[ ( A, B ) ] =
        //for { a <- as; b <- bs } yield ( a, b )
        as.flatMap { a => bs.map { b => ( a, b ) } }
    }
  }

  object Crosser extends LowPriorityCrosserImplicits {
    private type T[ X ] = Stream[ X ]

    implicit def crosser3[ A, B, C ] = new Crosser[ ( A, B ), C, ( A, B, C ) ] {
      def cross( abs: T[ ( A, B ) ], cs: T[ C ] ): T[ ( A, B, C ) ] = for { ( a, b ) <- abs; c <- cs } yield ( a, b, c )
    }

    implicit def crosser4[ A, B, C, D ] = new Crosser[ ( A, B, C ), D, ( A, B, C, D ) ] {
      def cross( abcs: T[ ( A, B, C ) ], ds: T[ D ] ): T[ ( A, B, C, D ) ] = for { ( a, b, c ) <- abcs; d <- ds } yield ( a, b, c, d )
    }
  }

  implicit class Crossable[ A ]( xs: Stream[ A ] ) {
    def cross[ B, C ]( ys: Stream[ B ] )( implicit crosser: Crosser[ A, B, C ] ): Stream[ C ] = crosser.cross( xs, ys )
  }

  import pt.inescn.utils.HList._
  import pt.inescn.utils.HList
  import pt.inescn.utils.HCons
  import pt.inescn.utils.HNil

  sealed trait EMapper[ HL <: HList ] {
    type Out <: HList
    def apply( hl: HL ): Out
  }

  /*
  def combineHList_0[A, B, C]( l1 : List[A], l2: List[B], l3: List[C])  = {
    l1.flatMap { x1 => l2.flatMap { x2 => l3.map { x3 => HCons(x1, HCons(x2, HCons(x3, HNil))) } } }
  }
 */
  import scala.language.higherKinds

  //  object EMapper extends LowPrioMapper {
  /*
  object EMapper extends LowestPrioMapper {
    implicit def cell2Mapper[ X, H1[ _ ], H2[ _ ], T <: HList ]( implicit evTail: EMapper[ T ] ): EMapper[ HCons[ H1[ X ], HCons[ H2[ X ], T ] ] ] =
      new EMapper[ HCons[ H1[ X ], HCons[ H2[ X ], T ] ] ] {
        type Out = HCons[ H1[ X ], HCons[ H2[ X ], evTail.Out ] ]
        def apply( hc: HCons[ H1[ X ], HCons[ H2[ X ], T ] ] ) = {
          println( s"??? 1 apply(Cons(${hc.head},${hc.tail.head})) + ${hc.tail.tail}" )
          HCons( hc.head, HCons( hc.tail.head, evTail( hc.tail.tail ) ) )
        }
      }
  }
  */

  /*  
[error] /home/hmf/git/dl4jtest/src/main/scala/pt/inescn/scratchpad/examples/CartesianProductV3.scala:81: overloaded method value flatMap with alternatives:
[error]   [B, That](f: _$1 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$1],B,That])That <and>
[error]   [B, That](f: _$1 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$1],B,That])That <and>
[error]   [B, That](f: _$1 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$1],B,That])That <and>
[error]   [B, That](f: _$1 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$1],B,That])That
[error]  cannot be applied to (X => pt.inescn.utils.HCons[X,pt.inescn.utils.HNil.type])
[error]           val crss = hc.head.flatMap { x : X => HCons(x, HNil) }
[error]                              ^
[error] /home/hmf/git/dl4jtest/src/main/scala/pt/inescn/scratchpad/examples/CartesianProductV3.scala:93: overloaded method value flatMap with alternatives:
[error]   [B, That](f: _$3 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$3],B,That])That <and>
[error]   [B, That](f: _$3 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$3],B,That])That <and>
[error]   [B, That](f: _$3 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$3],B,That])That <and>
[error]   [B, That](f: _$3 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$3],B,That])That
[error]  cannot be applied to (X => pt.inescn.utils.HCons[X,pt.inescn.utils.HNil.type])
[error]           val crss = hc.head.flatMap { x : X => HCons(x, HNil) }
[error]                              ^
[error] two errors found
*   [error] /home/hmf/git/dl4jtest/src/main/scala/pt/inescn/scratchpad/examples/CartesianProductV3.scala:81: overloaded method value flatMap with alternatives:
[error]   [B, That](f: _$1 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$1],B,That])That <and>
[error]   [B, That](f: _$1 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$1],B,That])That <and>
[error]   [B, That](f: _$1 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$1],B,That])That <and>
[error]   [B, That](f: _$1 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$1],B,That])That
[error]  cannot be applied to (X => pt.inescn.utils.HCons[X,pt.inescn.utils.HNil.type])
[error]           val crss = hc.head.flatMap { x : X => HCons(x, HNil) }
[error]                              ^
[error] /home/hmf/git/dl4jtest/src/main/scala/pt/inescn/scratchpad/examples/CartesianProductV3.scala:93: overloaded method value flatMap with alternatives:
[error]   [B, That](f: _$3 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$3],B,That])That <and>
[error]   [B, That](f: _$3 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$3],B,That])That <and>
[error]   [B, That](f: _$3 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$3],B,That])That <and>
[error]   [B, That](f: _$3 => scala.collection.GenTraversableOnce[B])(implicit bf: scala.collection.generic.CanBuildFrom[scala.collection.immutable.Stream[_$3],B,That])That
[error]  cannot be applied to (X => pt.inescn.utils.HCons[X,pt.inescn.utils.HNil.type])
[error]           val crss = hc.head.flatMap { x : X => HCons(x, HNil) }
[error]                              ^
[error] two errors found
*/


  object EMapper extends LowPrioMapper {
    implicit def cell2Mapper[ X, Y, H1[ _ ] >: Stream[ _ ], H2[ _ ] >: Stream[ _ ], T <: HList ]( implicit evTail: EMapper[ T ] ): EMapper[ HCons[ H1[ X ], HCons[ H2[ Y ], T ] ] ] =
      new EMapper[ HCons[ H1[ X ], HCons[ H2[ Y ], T ] ] ] {
        type Out = HCons[ H1[ X ], HCons[ H2[ Y ], evTail.Out ] ]
        def apply( hc: HCons[ H1[ X ], HCons[ H2[ Y ], T ] ] ) = {
          println( s"??? 1 apply(Cons(${hc.head},${hc.tail.head})) + ${hc.tail.tail}" )
          //val crss = hc.head.flatMap { x : X => hc.tail.head.map { y  : Y => HCons(x, HCons(y, HNil)) } }
          //val crss = hc.head.flatMap { x: X => HCons( x, HNil ) }
          HCons( hc.head, HCons( hc.tail.head, evTail( hc.tail.tail ) ) )
        }
      }
  }

  trait LowPrioMapper extends LowestPrioMapper {
    implicit def cellMapper[ X, H[ _ ] <: Stream[ _ ], T <: HNil ]( implicit evTail: EMapper[ T ] ): EMapper[ HCons[ H[ X ], T ] ] =
      new EMapper[ HCons[ H[ X ], T ] ] {
        type Out = HCons[ H[ X ], evTail.Out ]
        def apply( hc: HCons[ H[ X ], T ] ) = {
          println( s"??? 2 apply(Cons(${hc.head},${hc.tail}))" )
          //val crss = hc.head.flatMap { x : X => HCons(x, HNil) }
          HCons( hc.head, evTail( hc.tail ) )
        }
      }
  }

  /*
  trait LowPrioMapper extends LowestPrioMapper {
    implicit def cellMapper[ X, H[ _ ], T <: HList ]( implicit evTail: EMapper[ T ] ): EMapper[ HCons[ H[ X ], T ] ] =
      new EMapper[ HCons[ H[ X ], T ] ] {
        type Out = HCons[ H[ X ], evTail.Out ]
        def apply( hc: HCons[ H[ X ], T ] ) = {
          println( s"??? 2 apply(Cons(${hc.head},${hc.tail}))" )
          HCons( hc.head, evTail( hc.tail ) )
        }
      }
  }*/

  trait LowestPrioMapper {
    implicit def nilMapper[ HC <: HNil ]: EMapper[ HC ] =
      new EMapper[ HC ] {
        type Out = HNil
        def apply( hc: HC ) = {
          println( "??? end." )
          HNil
        }
      }
  }

  /* 
   
   * Solution 1
   
  object EMapper extends LowPrioMapper {
    implicit def cell2Mapper[ X, Y, H1[ _ ] <: Stream[_], H2[ _ ] <: Stream[_], T <: HList ]( implicit evTail: EMapper[ T ] ): 
         EMapper[ HCons[ H1[ X ], HCons[ H2[ Y ], T ] ] ] =
      new EMapper[ HCons[ H1[ X ], HCons[ H2[ Y ], T ] ] ] {
        type Out = HCons[ H1[ X ], HCons[ H2[ Y ], evTail.Out ] ]
        def apply( hc: HCons[ H1[ X ], HCons[ H2[ Y ], T ] ] ) = {
          println( s"??? 1 apply(Cons(${hc.head},${hc.tail.head})) + ${hc.tail.tail}" )
          //val crss = hc.head.flatMap { x : X => hc.tail.head.map { y  : Y => (x, y) } }
          HCons( hc.head, HCons( hc.tail.head, evTail( hc.tail.tail ) ) )
        }
      }
  }

    trait LowPrioMapper extends LowestPrioMapper {
    implicit def cellMapper[ X, H[ _ ] <: Stream[_], T <: HNil ]( implicit evTail: EMapper[ T ] ): EMapper[ HCons[ H[ X ], T ] ] =
      new EMapper[ HCons[ H[ X ], T ] ] {
        type Out = HCons[ H[ X ], evTail.Out ]
        def apply( hc: HCons[ H[ X ], T ] ) = {
          println( s"??? 2 apply(Cons(${hc.head},${hc.tail}))" )
          HCons( hc.head, evTail( hc.tail ) )
        }
      }
    }
    
  trait LowestPrioMapper {
    implicit def nilMapper[ HC <: HNil ]: EMapper[ HC ] =
      new EMapper[ HC ] {
        type Out = HNil
        def apply( hc: HC ) = {
          println( "??? end." )
          HNil
        }
      }
  }
   
   */

  def combineHList_0[ H <: HList ]( list: H )( implicit ev: EMapper[ H ] ) = {
    ev( list )
  }

  def main( args: Array[ String ] ) {
    import pt.inescn.scratchpad.StreamBuilder._

    // Use only streams
    val s1 = linspace( 0, 1 ).take( 3 )
    val s2 = linspace( 0.0, 0.1 ).take( 3 )
    val s3 = List( "a", "b" ).toStream
    val e1 = s1 cross s2 cross s3
    println( e1.mkString( "[", ";", "]" ) )

    // Very large streams
    // `java.lang.OutOfMemoryError: GC overhead limit exceeded` - slow due to memoization
    // If we use a `val`, this means we can access all stream members later. The Scala
    // Stream library uses memoizaton to that we can access the value later. This data
    // storage makes iteration slower and when to much data is stored, we get a OOM error. 
    // Always use `def`
    val s4 = ( 1 to 2 ).toStream // to break - 2500
    val s5 = ( 1 to 2 ).toStream
    val s6 = ( 1 to 2 ).toStream
    val e2 = s4 cross s5 cross s6
    //println(e2.mkString("[",";","]"))
    e2.foreach( println )

    val t1 = List( 1, 2, 3 ).toStream :: List( "a", "b" ).toStream :: List( 4.0, 5.0 ).toStream :: HNil
    // TODO combineHList_0( t1 )
  }

}

/*

1. http://stackoverflow.com/questions/39489047/poly-function-from-monomorphic-functions-that-operate-on-coproduct
2. http://stackoverflow.com/questions/16219545/scala-cross-cartesian-product-with-multiple-sources-and-heterogeneous-types
3. https://github.com/msavva/transphoner/blob/master/libtransphone/src/main/scala/org/babysherlock/util/EnrichedCollections.scala


*/
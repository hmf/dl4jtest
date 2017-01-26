package pt.inescn.scratchpad.examples

/**
 *
 *  sbt "run-main pt.inescn.scratchpad.examples.CartesianProductV1"
 *
 * @see http://stackoverflow.com/questions/16219545/scala-cross-cartesian-product-with-multiple-sources-and-heterogeneous-types
 * @see https://github.com/msavva/transphoner/blob/master/libtransphone/src/main/scala/org/babysherlock/util/EnrichedCollections.scala
 */
object CartesianProductV1 {
  trait Crosser[ A, B, C ] {
    def cross( as: Traversable[ A ], bs: Traversable[ B ] ): Traversable[ C ]
  }

  trait LowPriorityCrosserImplicits {
    private type T[ X ] = Traversable[ X ]

    implicit def crosser2[ A, B ] = new Crosser[ A, B, ( A, B ) ] {
      def cross( as: T[ A ], bs: T[ B ] ): T[ ( A, B ) ] = for { a <- as; b <- bs } yield ( a, b )
    }
  }

  object Crosser extends LowPriorityCrosserImplicits {
    private type T[ X ] = Traversable[ X ]

    implicit def crosser3[ A, B, C ] = new Crosser[ ( A, B ), C, ( A, B, C ) ] {
      def cross( abs: T[ ( A, B ) ], cs: T[ C ] ): T[ ( A, B, C ) ] = for { ( a, b ) <- abs; c <- cs } yield ( a, b, c )
    }

    implicit def crosser4[ A, B, C, D ] = new Crosser[ ( A, B, C ), D, ( A, B, C, D ) ] {
      def cross( abcs: T[ ( A, B, C ) ], ds: T[ D ] ): T[ ( A, B, C, D ) ] = for { ( a, b, c ) <- abcs; d <- ds } yield ( a, b, c, d )
    }
  }

  implicit class Crossable[ A ]( xs: Traversable[ A ] ) {
    def cross[ B, C ]( ys: Traversable[ B ] )( implicit crosser: Crosser[ A, B, C ] ): Traversable[ C ] = crosser.cross( xs, ys )
  }
  
  def main( args: Array[ String ] ) {
      val e1 = List(1, 2, 3) cross Seq("a", "b") cross Set(0.5, 7.3)
      //e1.foreach( println )
      //println(e1)
      println(e1.mkString("[",";","]"))
  }

}

/*

1. http://stackoverflow.com/questions/39489047/poly-function-from-monomorphic-functions-that-operate-on-coproduct
2. http://stackoverflow.com/questions/16219545/scala-cross-cartesian-product-with-multiple-sources-and-heterogeneous-types
3. https://github.com/msavva/transphoner/blob/master/libtransphone/src/main/scala/org/babysherlock/util/EnrichedCollections.scala


*/
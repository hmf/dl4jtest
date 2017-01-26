package pt.inescn.scratchpad.examples

/**
 *
 *  sbt "run-main pt.inescn.scratchpad.examples.CartesianProductV2"
 *
 * @see http://stackoverflow.com/questions/16219545/scala-cross-cartesian-product-with-multiple-sources-and-heterogeneous-types
 * @see https://github.com/msavva/transphoner/blob/master/libtransphone/src/main/scala/org/babysherlock/util/EnrichedCollections.scala
 */
object CartesianProductV2 {
  trait Crosser[ A, B, C ] {
    def cross( as: Stream[ A ], bs: Stream[ B ] ): Stream[ C ]
  }

  trait LowPriorityCrosserImplicits {
    private type T[ X ] = Stream[ X ]

    implicit def crosser2[ A, B ] = new Crosser[ A, B, ( A, B ) ] {
      def cross( as: T[ A ], bs: T[ B ] ): T[ ( A, B ) ] = 
        //for { a <- as; b <- bs } yield ( a, b )
        as.flatMap { a => bs.map { b => (a,b) } }
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
  
  def main( args: Array[ String ] ) {
      import pt.inescn.scratchpad.StreamBuilder._
      
      // Use only streams
      val s1 = linspace( 0, 1 ).take( 3 )
      val s2 = linspace( 0.0, 0.1 ).take( 3 )
      val s3 = List("a", "b").toStream
      val e1 = s1 cross s2 cross s3
      println(e1.mkString("[",";","]"))
      
      // Very large streams
      // `java.lang.OutOfMemoryError: GC overhead limit exceeded` - slow due to memoization
      // If we use a `val`, this means we can access all stream members later. The Scala
      // Stream library uses memoizaton to that we can access the value later. This data
      // storage makes iteration slower and when to much data is stored, we get a OOM error. 
      // Always use `def`
      val s4 = ( 1 to 250 ).toStream  // to break - 2500
      val s5 = ( 1 to 250 ).toStream  // to break - 2500
      val s6 = ( 1 to 250 ).toStream  // to break - 2500
      // breaks at (4,370,2209)
      //val e2 = s4 cross s5 cross s6
      def e2 = s4 cross s5 cross s6       // NB: use def
      //println(e2.mkString("[",";","]"))
      e2.foreach( println )
      
  }

}

/*

1. http://stackoverflow.com/questions/39489047/poly-function-from-monomorphic-functions-that-operate-on-coproduct
2. http://stackoverflow.com/questions/16219545/scala-cross-cartesian-product-with-multiple-sources-and-heterogeneous-types
3. https://github.com/msavva/transphoner/blob/master/libtransphone/src/main/scala/org/babysherlock/util/EnrichedCollections.scala


*/
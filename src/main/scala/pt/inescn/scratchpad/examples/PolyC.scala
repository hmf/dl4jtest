package pt.inescn.scratchpad.examples

/**
 * This file implements the concept of a Poly(morphic) implicit.
 * It is  based on Shapeless's Poly class - see the shapeless guide.
 * If we look at Sections 7.2.1 How Poly works, we read:
 *
 * pg 88
 * "There is some subtle scoping behaviour here that allows the compiler to
 * locate instances of Case without any additional imports. Case has an extra type
 * parameter P referencing the singleton type of the Poly . The implicit scope
 * for Case[P, A] includes the companion objects for Case , P , and A . We’ve as-
 * signed P to be myPoly.type and the companion object for myPoly.type is
 * myPoly itself. In other words, Cases defined in the body of the Poly are
 * always in scope no matter where the call site is."
 *
 * Here we show how we can apply the correct implicit by passing the object
 * that holds these functions without the need for client importing. Their
 * are various other ways for explicits to be detected, but a client is "forced"
 * to use import. For more information in implicit see links below.
 *
 * Note that the object with the implicits is passed on as a paramnter. Only at
 * this call site does the compiler have thje full (object) type.
 *
 * @see https://github.com/underscoreio/shapeless-guide/blob/develop/dist/shapeless-guide.pdf
 * @see http://docs.scala-lang.org/tutorials/FAQ/chaining-implicits.html
 * @see http://docs.scala-lang.org/tutorials/FAQ/finding-implicits.html
 * @see http://docs.scala-lang.org/tutorials/FAQ/context-bounds.html
 * @see https://github.com/1ambda/scala/tree/master/type-level-programming
 * @see http://www.lihaoyi.com/post/ImplicitDesignPatternsinScala.html
 *
 *  https://github.com/1ambda/scala/tree/master/type-level-programming
 * http://www.hyperlambda.com/posts/hlist-map-in-scala/
 */

import scala.language.higherKinds

/*
 * This type class is used to carry the type information so that the compiler
 * can use that to find the correct implicits. The type is kept in `P` and the 
 * type `A` is the input type. The output is explicitly typed via the  `Result`
 * type. The apply method does the actual transformation. 
 */
trait CaseC[ P, T, A[ _ ] ] {
  type Result
  def apply( a: A[ T ] ): Result
}

/**
 * A user must extend this trait with the required implicits. It should be an object.
 * The implicit uses  the `Case` type to store the implementing object's type.
 * The `Case` class is then used to apply the transformation.
 */
trait PolyC {
  def apply[ T, A[ _ ] ]( arg: A[ T ] )( implicit cse: CaseC[ this.type, T, A ] ): cse.Result = 
    cse.apply( arg )
}

/**
 * sbt "run-main pt.inescn.utils.PolyCExample"
 */
object PolyCExample {

  /**
   * Example of the use of implicits.
   */
  object myPolyC extends PolyC {
    implicit def listIntCaseC = {
      new CaseC[ this.type, Int, List ] {
        type Result = Int
        def apply( lst: List[ Int ] ): Result = lst.length
      }
    }
    implicit def listDoubleCaseC = {
      new CaseC[ this.type, Double, List ] {
        type Result = Int
        def apply( lst: List[ Double ]): Result = lst.length
      }
    }
  }

  // sbt "run-main pt.inescn.utils.HListExample"
  def main( args: Array[ String ] ) {

    def callPoly_3[ T, C[ _ ], ACC ]( a: C[ T ], p: PolyC )( implicit cse1: CaseC[ p.type, T, C ] ) = {
      val v1 = p( a )
      v1
    }

   val p8 = callPoly_3( List( 1, 2 ), myPolyC )
   println(s"Container version Poly: $p8")
  }

}
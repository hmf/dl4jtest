package pt.inescn.utils

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

/*
 * This type class is used to carry the type information so that the compiler
 * can use that to find the correct implicits. The type is kept in `P` and the 
 * type `A` is the input type. The output is explicitly typed via the  `Result`
 * type. The apply method does the actual transformation. 
 */
trait Case[ P, A ] {
  type Result
  def apply( a: A ): Result
}

/**
 * A user must extend this trait with the required implicits. It should be an object.
 * The implicit uses  the `Case` type to store the implementing object's type.  
 * The `Case` class is then used to apply the transformation. 
 */
trait Poly {
  def apply[ A ]( arg: A )( implicit cse: Case[ this.type, A ] ): cse.Result = cse.apply( arg )
}


/**
 * sbt "run-main pt.inescn.utils.PolyExample"
 */
object PolyExample {
  
  /**
   * Example of the use of implicits.
   */
  object myPoly extends Poly {

    /*
    implicit def anyCase = {
      new Case[ this.type, Any ] {
        type Result = Any
        def apply( v: Any ): Any = v
      }
    }*/

    implicit def intCase = {
      new Case[ this.type, Int ] {
        type Result = Double
        def apply( num: Int ): Double = num / 2.0
      }
    }

    implicit def stringCase = {
      new Case[ this.type, String ] {
        type Result = Int
        def apply( str: String ): Int = str.length
      }
    }

  }
  
  // sbt "run-main pt.inescn.utils.HListExample"
  def main( args: Array[ String ] ) {

    // We can just pass the object and the compiler finds the appropriate implicit
    //val p1 = myPoly.apply(123)
    val p1 = myPoly( 123 )
    println( s"Poly(123) = $p1" )
    //val p2 = myPoly.apply( "456" )
    val p2 = myPoly( "456" )
    println( s"Poly('123') = $p2" )

    // Nothing new here, we just do it in another context
    def callPoly1( a: Int, b: String ) = {
      val v1 = myPoly( a )
      val v2 = myPoly( b )
      ( v1, v2 )
    }

    // Standard use of the implicit parameter list. As expected, companion object in scope when import is used.
    def callPoly2( a: Int, b: String )( implicit cse1: Case[ _, Int ], cse2: Case[ _, String ] ) = {
      val v1 = cse1( a )
      val v2 = cse2( b )
      ( v1, v2 )
    }

    // Standard use of the implicitly function. As expected, companion object in scope due to its object reference
    def callPoly3( a: Int, b: String ) = {
      val t0 = implicitly[ Case[ myPoly.type, Int ] ] // The self-referencing of the this.type allows us to avoid importing the implicits
      val v1 = t0( a )
      val t1 = implicitly[ Case[ myPoly.type, String ] ] // The self-referencing of the this.type allows us to avoid importing the implicits
      val v2 = t1( b )
      ( v1, v2 )
    }

    // Non-standard use of the implicit parameters. The companion object in scope due to its object reference
    def callPoly4( a: Int, b: String, p: Poly )( implicit cse1: Case[ p.type, Int ], cse2: Case[ p.type, String ] ) = {
      val v1 = p( a )
      val v2 = p( b )
      ( v1, v2 )
    }

    // Generalized version of callPoly4. The companion object in scope due to its object reference
    def callPoly4_1[ T1, T2 ]( a: T1, b: T2, p: Poly )( implicit cse1: Case[ p.type, T1 ], cse2: Case[ p.type, T2 ] ) = {
      val v1 = p( a )
      val v2 = p( b )
      ( v1, v2 )
    }

    // Standard definition of the implicitly function
    // def implicitly[T](implicit e: T) = e 
    // We can get the Case if we want to
    def implicitlyPoly[A]( p: Poly )( implicit cse: Case[ p.type, A ] ) = cse

    /*
     * Once we get the  Case we can use it. However we need to add the implicit
     * parameters explicitly. If not the information at the original call site is lost
     * (after the call parameter list, the type is Poly and not the extending Poly 
     * object. 
     */
    def callPoly5( a: Int, b: String, p: Poly )( implicit cse: Case[ p.type, Int ] , cse2: Case[ p.type, String ] ) = {
      val t0 = implicitlyPoly[Int]( p ) 
      val v1 = t0( a )
      val t1 = implicitlyPoly[String]( p ) 
      val v2 = t1( b )
      ( v1, v2 )
    }

    
    def callPoly6( a: Int, b: String, p: Poly)( implicit cse: Case[ p.type, Int ] , cse2: Case[ p.type, String ] ) = {
      val t0 = implicitly[ Case[ p.type, Int ] ] 
      val v1 = t0( a )
      val t1 = implicitly[ Case[ p.type, String ] ] 
      val v2 = t1( b )
      ( v1, v2 )
    }
   

    // We need to declare the implicits in order to propogate the context
    def callCallPoly4( a: Int, b: String, p: Poly )( implicit cse1: Case[ p.type, Int ], cse2: Case[ p.type, String ] ) = {
      callPoly4( a, b, p )
    }

    // We need to declare the implicits in order to propogate the context
    def callCallPoly5[ T1, T2 ]( a: T1, b: T2, p: Poly )( implicit cse1: Case[ p.type, T1 ], cse2: Case[ p.type, T2 ] ) = {
      //callPoly5( a, b, p )
      callPoly4_1( a, b, p )
    }

    /*
    def callPoly( a: Int, b: String, p: Poly ) = {
      //import p.apply
      //import p._
      //import myPoly._
      //val t1 = implicitly[Case[ p.type, String ] ] 

      val v1 = p( a ) // this does not compile because pt.inescn.utils.Case[p.type,Int] <> pt.inescn.utils.Case[myPoly.type,Int]
      val v2 = p( b ) // this does not compile because pt.inescn.utils.Case[p.type,Int] <> pt.inescn.utils.Case[myPoly.type,Int]
      (v1, v2)
    }*/

    val c1 = implicitlyPoly[Int]( myPoly )
    val v0 = c1( 1 )

    // Ok 
    callPoly1( 1, "2" )

    // Needs import
    //import myPoly._
    //callPoly2( 1, "2")

    // No import required   
    //import myPoly._
    callPoly3( 1, "2")

    // Compiler finds implicits based on object type
    val ( v3, v4 ) = callPoly4( 1, "2", myPoly )
    println( s"v3 = $v3" )
    println( s"v4 = $v4" )
    val ( v3_1, v4_1 ) = callPoly4_1( 1, "2", myPoly )

    // Compiler finds implicits based on object type
    val ( v5, v6 ) = callPoly5( 1, "2", myPoly )
    println( s"v3 = $v5" )
    println( s"v4 = $v6" )

    // We need to declare the implicits in order to propagate the context
    val ( v7, v8 ) = callCallPoly4( 1, "2", myPoly )

    // We need to declare the implicits in order to propagate the context
    val ( v9, v10 ) = callCallPoly5( 1, "2", myPoly )

    // Avoid import
    //import myPoly._
    //callPoly( 1, "2", myPoly )
  }

}
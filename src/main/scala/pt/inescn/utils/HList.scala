package pt.inescn.utils

import scala.language.higherKinds

// ***************************************************
// https://github.com/underscoreio/shapeless-guide/blob/develop/dist/shapeless-guide.pdf
//  Sections 7.2.1 How Poly works
// https://github.com/1ambda/scala/tree/master/type-level-programming
// http://www.lihaoyi.com/post/ImplicitDesignPatternsinScala.html
//
// http://docs.scala-lang.org/tutorials/FAQ/chaining-implicits.html
// http://docs.scala-lang.org/tutorials/FAQ/finding-implicits.html
// http://docs.scala-lang.org/tutorials/FAQ/context-bounds.html

trait Case[ P, A ] {
  type Result
  def apply( a: A ): Result
}

trait Poly {
  def apply[ A ]( arg: A )( implicit cse: Case[ this.type, A ] ): cse.Result = cse.apply( arg )
}

sealed trait HList {
  type Prepend[ A ] <: HList
  type Append[ L <: HList ] <: HList
  type Mapped <: HList

  def ::[ U ]( v: U ): Prepend[ U ]
  def prepend[ A ]( a: A ): Prepend[ A ]
  def append[ L <: HList ]( l: L ): Append[ L ]

  def map( p: Poly ): Mapped
}

/**
 * @see https://github.com/dragisak/type-level/blob/master/src/main/scala/com/dragisak/typelevel/HList.scala
 * @see https://apocalisp.wordpress.com/2010/06/08/type-level-programming-in-scala/
 * @see https://apocalisp.wordpress.com/2010/07/06/type-level-programming-in-scala-part-6a-heterogeneous-list%C2%A0basics/
 * @see https://github.com/runarorama
 * @see http://blog.higher-order.com/ (Rúnar Bjarnason)
 * @see http://pchiusano.github.io/ (Paul Chiusano)
 *
 * Mapping
 * @see https://github.com/milessabin/shapeless/issues/73
 * @see http://www.hyperlambda.com/posts/hlist-map-in-scala/
 * @see https://xuwei-k.github.io/shapeless-sxr/shapeless-2.10-2.0.0-M1/polyntraits.scala.html
 *
 * Typelevel
 * @see http://gigiigig.github.io/tlp-step-by-step/introduction.htmls
 * @see https://github.com/1ambda/scala/tree/master/type-level-programming/src
 * @see http://typelevel.org/blog/
 * @see https://www.scala-exercises.org/
 * @see http://slick.lightbend.com/talks/scalaio2014/Type-Level_Computations.pdf
 *
 * https://skillsmatter.com/explore?q=tag%3Atypelevel
 *
 *  https://github.com/milessabin/shapeless/tree/master/examples/src/main/scala/shapeless/examples
 *
 *  HList
 *  https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/syntax/hlists.scala
 * https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/hlists.scala
 *    /**
 * Maps a higher rank function across this `HList`.
 * */
 * def map(f : Poly)(implicit mapper : Mapper[f.type, L]) : mapper.Out = mapper(l)
 *
 * /**
 * Flatmaps a higher rank function across this `HList`.
 * */
 * def flatMap(f : Poly)(implicit mapper : FlatMapper[f.type, L]) : mapper.Out = mapper(l)
 *
 * Mapper
 * https://github.com/milessabin/shapeless/blob/fcc15a6897ad9aad10e19d0b2432ccf0a9630515/core/src/main/scala/shapeless/ops/hlists.scala
 */

final case class HCons[ H, T <: HList ]( head: H, tail: T ) extends HList {

  type Prepend[ A ] = HCons[ A, HCons[ H, T ] ]
  type Append[ L <: HList ] = HCons[ H, T#Append[ L ] ]
  type Mapped = HCons[ H, T#Mapped ]

  def ::[ U ]( v: U ): Prepend[ U ] = HCons( v, this )

  def prepend[ A ]( a: A ): Prepend[ A ] = HCons( a, this )
  def append[ L <: HList ]( l: L ): Append[ L ] = HCons( head, tail.append( l ) )
  //def map(p: Poly)  = HCons( p(head), tail.map( p ) )
  def map( p: Poly ): Mapped = HCons( head, tail.map( p ) )
}

//sealed class HNil extends HList {
case object HNil extends HList {
  type Prepend[ A ] = HCons[ A, HNil.type ]
  type Append[ L <: HList ] = L
  type Mapped = this.type

  def ::[ T ]( v: T ): Prepend[ T ] = HCons( v, this ) // No need to type explicitly, same as prepend

  def prepend[ A ]( a: A ): Prepend[ A ] = HCons( a, this )
  def append[ L <: HList ]( l: L ): Append[ L ] = l
  def map( p: Poly ): Mapped = this
}

// aliases for building HList types and for pattern matching
object HList {
  type ::[ H, T <: HList ] = HCons[ H, T ]

  val :: = HCons // alias for pattern matching

  // if we use this instead of an explicit object (see below) we have to `import pt.inescn.utils.HList.HNil`
  // to this this embedded object as well as `import pt.inescn.utils.HNil`to get the type (class). 
  // The `import HList.HNil` also causes the  IDE to complain of ambiguous reference being  imported twice
  //val HNil = new HNil 
  type HNil = HNil.type
}

// If we use the sealed class type
//object HNil extends HNil

// sbt "run-main pt.inescn.utils.HListExample"
object HListExample {
  import HList._

  def y = HList
  val z = HList
  type w = String :: Boolean :: Double :: HNil

  // construct an HList similar to the Tuple3 ("str", true, 1.0)
  val x = "str" :: true :: 1.0 :: HNil

  // get the components by calling head/tail
  val s: String = x.head
  val b: Boolean = x.tail.head
  val d: Double = x.tail.tail.head
  // compile error
  //val e = x.tail.tail.tail.head

  // or, decompose with a pattern match

  val f: ( String :: Boolean :: Double :: HNil ) => String = {
    case "s" :: false :: _        => "test"
    case h :: true :: 1.0 :: HNil => h
    // compilation error because of individual type mismatches and length mismatch
    //case 3 :: "i" :: HNil => "invalid"
    case _                        => "unknown"
  }

  val f1: ( w ) => String = {
    case "s" :: false :: _        => "test"
    case h :: true :: 1.0 :: HNil => h
    // compilation error because of individual type mismatches and length mismatch
    //case 3 :: "i" :: HNil => "invalid"
    case _                        => "unknown"
  }

  /*
  def cartesian7[ A, B ]( list: Stream[ Stream[ A ] ], acc: Seq[ HList ], f: Seq[ HList ] => B ): Stream[ B ] = {
    list match {
      case Stream.Empty => List( f( acc ) ).toStream
      case h #:: t      => h.flatMap { x => cartesian7( t, x +: acc, f ) }
    }
  }*/

  import scala.language.implicitConversions

  // http://stackoverflow.com/questions/5598085/where-does-scala-look-for-implicits/5598107#5598107
  // http://stackoverflow.com/questions/20616525/scala-type-override
  //  Iterable[+A] : map and flatMap
  sealed trait Mapper[ A, T[ A ], B ] {
    //type Out <: T[B]
    type Out[ X ] //= T[X]
    def map( l: T[ A ], f: A => B ): Out[ B ]
  }

  object Mappers {

    implicit def typedMapper[ A, T[ A ] <: Iterable[ A ], B ]: Mapper[ A, T, B ] =
      new Mapper[ A, T, B ] {
        override type Out[ X ] = Iterable[ X ]
        //override type Out <: Iterable[ B ]
        //def map( l: T[ A ], f: A => B ) : this.Out = {
        def map( l: T[ A ], f: A => B ): Out[ B ] = {
          println( "map" )
          l.map( f )
        }
      }

    implicit def IntMapper = typedMapper[ Int, List, Int ]
  }

  //def testMapper[ A, T[ A ], B ]( l: T[ A ], f: A => B )( implicit mapper: Mapper[ A, T, B ] ): T[B] = {
  def testMapper[ A, T[ A ], B ]( l: T[ A ], f: A => B )( implicit mapper: Mapper[ A, T, B ] ): Mapper[ A, T, B ]#Out[ B ] = {
    println( mapper )
    mapper.map( l, f )
  }

  ////////////////////////////////////////
  //trait Mapper1[ A, T[ _ ], B ] {
  trait Mapper1[ A, T[ A ], B ] {
    def map( ta: T[ A ] )( f: A => B ): T[ B ]
    def mapx( ta: T[ A ], f: A => B ): T[ B ]
  }

  // Note that Iterable[A]#map has type [B](A => B)Iterable[B]. You can't have typedMapper
  // like yours from above just yet, because T#map is not certain to return another T;
  // it only promises an Iterable.
  implicit def iterableMapper[ A, B ]: Mapper1[ A, Iterable, B ] = new Mapper1[ A, Iterable, B ] {
    // Multiple param lists support the type inferencer
    //override def map( i: Iterable[ A ] )( f: A => B ) = i.map( f )
    override def map( i: Iterable[ A ] )( f: A => B ): Iterable[ B ] = i.map( f )
    //override def map( i: Iterable[ A ] )( f: A => B ) : T[ B ] = i.map( f ) // Fails
    override def mapx( i: Iterable[ A ], f: A => B ): Iterable[ B ] = i.map( f )
  }

  // Curried and arg-swapped version of Mapper
  type MapperOf[ A, B ] = { type l[ T[ _ ] ] = Mapper1[ A, T, B ] }
  def map1[ A, B, T[ _ ]: MapperOf[ A, B ]#l ]( ta: T[ A ] )( f: A => B ): T[ B ] = implicitly[ Mapper1[ A, T, B ] ].map( ta )( f )
  def map2[ A, B, T[ _ ]: MapperOf[ A, B ]#l ]( ta: T[ A ], f: A => B ): T[ B ] = implicitly[ Mapper1[ A, T, B ] ].map( ta )( f )
  def map3[ A, B, T[ _ ]: MapperOf[ A, B ]#l ]( ta: T[ A ], f: A => B ): T[ B ] = implicitly[ Mapper1[ A, T, B ] ].mapx( ta, f )

  //def map4[ A, B, T[_] ]( ta: T[ A ] , f: A => B ): T[ B ] = implicitly[ Mapper1[ A, T, B ] ].map( ta ) ( f )
  val t1 = map1( List( -1, -2, -3 ): Iterable[ Any ] )( _.toString )
  println( t1 )
  //map1( List( 1, 2, 3 ) )( _ * 2 ) // NOPE! The inferencer has already decided T = List, before
  // looking for implicits, so the resolution fails to notice that
  // iterableMapper would work.
  val t3 = map1[ Int, Int, Iterable ]( List( 1, 2, 3 ) )( _ * 2 ) // Works  

  val t4 = map2( List( -1, -2, -3 ): Iterable[ Any ], { x: Any => x.toString } )
  println( t4 )
  val t5 = map2( List( -1, -2, -3 ): Iterable[ Any ], { x: Any => x.toString } )
  println( t5 )

  ///////////////

  def genCartesian[ A, B ]( list: HList, acc: HList ): HList = {
    list match {
      case HNil => acc
      case h :: t =>
        println( h )
        genCartesian( t, h :: acc )
    }
  }

  /* TODO
    def doF[ A, B ]( list: HList, acc: HList, f : A => B ): HList = {
    list match {
      case HNil => acc
      case h :: t =>
        println( h )
        val nh = f(h)
        genCartesian( t, nh :: acc )
    }
  }
*/
  def genCartesianL[ A, T[ A ], B ]( list: HList, acc: HList )( implicit mapper: Mapper[ A, T, B ] ): HList = {
    list match {
      case HNil => acc
      case h :: t =>
        println( h )
        //val t = mapper.map( h, x => x )
        genCartesian( t, h :: acc )
    }
  }

  // ***************************************************
  // https://github.com/underscoreio/shapeless-guide/blob/develop/dist/shapeless-guide.pdf
  //  Sections 7.2.1 How Poly works
  // https://github.com/1ambda/scala/tree/master/type-level-programming
  /*
  trait Case[P, A] {
    type Result
    def apply(a: A) : Result
  }
  
  trait Poly {
    def apply[A](arg: A)(implicit cse : Case[this.type, A]): cse.Result = cse.apply(arg)
  }
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

  def genCartesianM( list: HList, acc: HList ): HList = {
    list match {
      case HNil => acc
      case h :: t =>
        println( h )
        //val nh = myPoly.apply( h )
        genCartesian( t, h :: acc )
    }
  }

  /**
   * pg 88
   * There is some subtle scoping behaviour here that allows the compiler to 
   * locate instances of Case without any additional imports. Case has an extra type
   * parameter P referencing the singleton type of the Poly . The implicit scope
   * for Case[P, A] includes the companion objects for Case , P , and A . We’ve as-
   * signed P to be myPoly.type and the companion object for myPoly.type is
   * myPoly itself. In other words, Cases defined in the body of the Poly are 
   * always in scope no matter where the call site is.
   */

  def combineHList_1( list: HList ): HList = {
    list match {
      case HNil => list
      case h :: t =>
        val v1 = h
        h :: "End" :: HNil
    }
  }

  // ***************************************************

  // sbt "run-main pt.inescn.utils.HListExample"
  def main( args: Array[ String ] ) {
    val l1 = "One" :: 2 :: HNil
    val l2 = "Three" :: 4.0 :: HNil
    val l3 = l1.append( l2 )
    println( l3 )
    // Check the values
    val "One" :: 2 :: "Three" :: 4.0 :: HNil = l3
    // Check the types
    val checkl: HCons[ String, HCons[ Int, HCons[ String, HCons[ Double, HNil ] ] ] ] = l3

    val l4 = "Zero" :: l1
    val l5 = l1.prepend( "Zero" )
    val "Zero" :: "One" :: 2 :: HNil = l4
    val "Zero" :: "One" :: 2 :: HNil = l5

    val r1 = combineHList_1( l1 )
    println( s"Combine 1: $r1" )

    genCartesian( l3, HNil )
    genCartesian( HNil, HNil )

    val l6 = List( 1, 2 ) :: List( 3.0, 4.0 ) :: HNil
    val l7 = genCartesian( l6, HNil )
    println( l7 )

    import Mappers.IntMapper
    val m1: pt.inescn.utils.HListExample.Mapper[ Int, List, Int ] = IntMapper
    val l8 = testMapper( List( 1, 2, 3 ), { x: Int => x + 1 } )( IntMapper )
    val l9 = testMapper( List( 1, 2, 3 ), { x: Int => x + 1 } )
    println( l9 )

    val l10 = genCartesianL( l6, HNil )
    println( l7 )

    // http://docs.scala-lang.org/tutorials/FAQ/finding-implicits.html
    //val l11 = Seq(42, 42f, "Blah")
    //val p1 = myPoly.apply(123)
    val p1 = myPoly( 123 )
    println( s"Poly(123) = $p1" )
    val p2 = myPoly.apply( "456" )
    println( s"Poly('123') = $p2" )

    val p3 = 123 :: "456" :: HNil
    val p4 = genCartesianM( p3, HNil )
    println( "p4 = " + p4 )
    val p5 = p3.map( myPoly )
    println( "p5 = " + p5 )
    val v1 = p5.head
    val v2 = p5.tail.head

    // Nothing new here
    def callPoly1( a: Int, b: String ) = {
      val v1 = myPoly( a )
      val v2 = myPoly( b )
      ( v1, v2 )
    }

    // As expected, companion object in scope
    def callPoly2( a: Int, b: String )( implicit cse1: Case[ _, Int ], cse2: Case[ _, String ] ) = {
      val v1 = cse1( a )
      val v2 = cse2( b )
      ( v1, v2 )
    }

    def callPoly3( a: Int, b: String) = {
      val t0 = implicitly[ Case[ myPoly.type, Int ] ] // The self-referencing of the this.type allows us to avoid importing the implicits
      val v1 = t0( a )
      val t1 = implicitly[ Case[ myPoly.type, String ] ] // The self-referencing of the this.type allows us to avoid importing the implicits
      val v2 = t1( b )
      ( v1, v2 )
    }

    def callPoly4( a: Int, b: String, p: Poly)( implicit cse1: Case[ p.type, Int ], cse2: Case[ p.type, String ] ) = {
      val v1 = p( a )
      val v2 = p( b )
      (v1, v2)
    }

    def callPoly5[T1, T2]( a: T1, b: T2, p: Poly)( implicit cse1: Case[ p.type, T1 ], cse2: Case[ p.type, T2] ) = {
      val v1 = p( a )
      val v2 = p( b )
      (v1, v2)
    }
    
    // def implicitly[T](implicit e: T) = e 
    //def implicitlyPoly[T[_,_] , A, B](implicit e: T[A,B]) = e 
    //def implicitlyPoly[A, B](implicit e: Case[A,B]) = e 
    //def implicitlyPoly[A](p:Poly)(implicit e: Case[p.type,A]) = e 
    //def implicitlyPoly(v: Int, p:Poly)(implicit e: Case[p.type,Int]) = p(v) 
    def implicitlyPoly(p: Poly)( implicit cse: Case[ p.type, Int ]) = cse

    /*
    def callPoly5( a: Int, b: String, p: Poly) = {
      //import p._  
      
      //val t0 = implicitlyPoly[ Case, p.type, Int ] // The self-referencing of the this.type allows us to avoid importing the implicits
      //val t0 = implicitlyPoly[ Int ](p) // The self-referencing of the this.type allows us to avoid importing the implicits
      val t0 = implicitlyPoly(a, p) // The self-referencing of the this.type allows us to avoid importing the implicits
      //val t0 = implicitly[ Case[ p.type, Int ] ] // The self-referencing of the this.type allows us to avoid importing the implicits
      val v1 = t0( a )
      //val t1 = implicitly[ Case[ p.type, String ] ] // The self-referencing of the this.type allows us to avoid importing the implicits
      //val v2 = t1( b )
      ( v1, v2 )
    }*/

    def callPoly7( a: Int, b: String, p: Poly)( implicit cse: Case[ p.type, Int ]) = {
      //import myPoly._
     // import p._
      val t0 = implicitlyPoly(p) // The self-referencing of the this.type allows us to avoid importing the implicits
      val t1 = implicitly[Case[ p.type, Int ]] // The self-referencing of the this.type allows us to avoid importing the implicits
      val v1 = t0(a)
      val v2 = t1(a)
      (v1, v2)
    }
    
    /*
    def callPoly6( a: Int, b: String, p: Poly) = {
      val t0 = implicitly[ Case[ p.type, Int ] ] 
      val v1 = t0( a )
      val t1 = implicitly[ Case[ p.type, String ] ] 
      val v2 = t1( b )
      ( v1, v2 )
    }
    */

    // We need to declare the implicits in order to propogate the context
    def callCallPoly4( a: Int, b: String, p: Poly)( implicit cse1: Case[ p.type, Int ], cse2: Case[ p.type, String ] ) = {
      callPoly4( a, b, p)
    }
    
    // We need to declare the implicits in order to propogate the context
    def callCallPoly5[T1, T2]( a: T1, b: T2, p: Poly)( implicit cse1: Case[ p.type, T1 ], cse2: Case[ p.type, T2] ) = {
      callPoly5(a, b, p)
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

    val c1 = implicitlyPoly(myPoly)
    val v0 = c1(1)
     
    // Ok 
    callPoly1( 1, "2" )

    // Needs import
    //import myPoly._
    //callPoly2( 1, "2")

    // Needs import
    //import myPoly._
    //callPoly3( 1, "2")

    // Compiler finds implicits based on object type
    val (v3, v4) = callPoly4( 1, "2", myPoly)
    println( s"v3 = $v3" )
    println( s"v4 = $v4" )
    
    // Compiler finds implicits based on object type
    val (v5, v6) = callPoly5( 1, "2", myPoly)
    println( s"v3 = $v5" )
    println( s"v4 = $v6" )
    
    // Avoid import
    //import myPoly._
    //callPoly( 1, "2", myPoly )

    // We need to declare the implicits in order to propogate the context
    val (v7, v8) = callCallPoly4( 1, "2", myPoly)
    
    // We need to declare the implicits in order to propogate the context
    val (v9, v10) = callCallPoly5( 1, "2", myPoly)
    
  }

}

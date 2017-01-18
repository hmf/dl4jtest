package pt.inescn.scratchpad.debug

import scala.language.higherKinds

import pt.inescn.utils.Poly
import pt.inescn.utils.Case

/* If we define this here, the map(l) will not find a standard HList
trait Case[ P, A ] {
  type Result
  def apply( a: A ): Result
}

trait Poly {
  def apply[ A ]( arg: A )( implicit cse: Case[ this.type, A ] ): cse.Result = cse.apply( arg )
}
*/
sealed trait HList {
  type Prepend[ A ] <: HList
  type Append[ L <: HList ] <: HList

  def ::[ U ]( v: U ): Prepend[ U ]
  def prepend[ A ]( a: A ): Prepend[ A ]
  def append[ L <: HList ]( l: L ): Append[ L ]
}

// http://jto.github.io/articles/typelevel_quicksort/
final case class HCons[ H, T <: HList ]( head: H, tail: T ) extends HList {

  type Prepend[ A ] = HCons[ A, HCons[ H, T ] ]
  type Append[ L <: HList ] = HCons[ H, T#Append[ L ] ]

  def ::[ U ]( v: U ): Prepend[ U ] = HCons( v, this )

  def prepend[ A ]( a: A ): Prepend[ A ] = HCons( a, this )
  def append[ L <: HList ]( l: L ): Append[ L ] = HCons( head, tail.append( l ) )
}

//sealed class HNil extends HList {
sealed trait HNil extends HList
//object HNil extends HNil {
case object HNil extends HNil {
  type Prepend[ A ] = HCons[ A, HNil.type ]
  type Append[ L <: HList ] = L

  def ::[ T ]( v: T ): Prepend[ T ] = HCons( v, this ) // No need to type explicitly, same as prepend

  def prepend[ A ]( a: A ): Prepend[ A ] = HCons( a, this )
  def append[ L <: HList ]( l: L ): Append[ L ] = l
}

// aliases for building HList types and for pattern matching
object HList {
  type ::[ H, T <: HList ] = HCons[ H, T ]

  val :: = HCons // alias for pattern matching

  // if we use this instead of an explicit object (see below) we have to `import pt.inescn.utils.HList.HNil`
  // to this this embedded object as well as `import pt.inescn.utils.HNil`to get the type (class). 
  // The `import HList.HNil` also causes the  IDE to complain of ambiguous reference being  imported twice
  //val HNil = new HNil 
  //type HNil = HNil.type

  def map[ P <: Poly, H <: HList ]( p: P, hc: H )( implicit ev: Mapper[ P, H ] ) = ev( hc )
  def map0[ P <: Poly, H ]( p: P, hc: H )( implicit cse: pt.inescn.utils.Case[ P, H ] ) = cse( hc )
}

import scala.language.implicitConversions

sealed trait Mapper[ P, HL <: HList ] {
  type Out <: HList
  def apply( hl: HL ): Out
}

/**
 * pt.inescn.scratchpad.debug.Mapper[
 * pt.inescn.scratchpad.debug.myPoly.type,
 * pt.inescn.scratchpad.debug.HCons[String,pt.inescn.scratchpad.debug.HCons[Int,pt.inescn.scratchpad.debug.HNil.type]]])ev.Out
 */
object Mapper {
  implicit def cellMapper[ P <: Poly, H, T <: HList ]( implicit cse: pt.inescn.utils.Case[ P, H ], evTail: Mapper[ P, T ] ): Mapper[ P, HCons[ H, T ] ] =
    new Mapper[ P, HCons[ H, T ] ] {
      type Out = HCons[ cse.Result, evTail.Out ]
      def apply( hc: HCons[ H, T ] ) = {
        println( s"apply(Cons(${hc.head},${hc.tail})) = ${cse( hc.head )}" )
        HCons( cse( hc.head ), evTail( hc.tail ) )
      }
    }

  implicit def nilMapper[ S, HC <: HNil ]: Mapper[ S, HC ] =
    new Mapper[ S, HC ] {
      type Out = HNil
      def apply( hc: HC ) = {
        println( "end." )
        HNil
      }
    }
}

object myPoly extends Poly {
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
  implicit def doubleCase = {
    new Case[ this.type, Double ] {
      type Result = Int
      def apply( num: Double): Int = num.toInt
    }
  }
  implicit def listCase[ H <: HList ] = {
    new Case[ this.type, H ] {
      type Result <: H
      def apply( l: H ): H = l
    }
  }
}

// If we use the sealed class type
//object HNil extends HNil

// sbt "run-main pt.inescn.utils.HListExample"
object HListExpandType {
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
    val checkl: HCons[ String, HCons[ Int, HCons[ String, HCons[ Double, HNil.type ] ] ] ] = l3

    val l4 = "Zero" :: l1
    val l5 = l1.prepend( "Zero" )
    val "Zero" :: "One" :: 2 :: HNil = l4
    val "Zero" :: "One" :: 2 :: HNil = l5

    import Mapper._
    //val v0 = map(myPoly, HNil)(nilMapper)
    val v0 = map( myPoly, HNil )
    //val v1 = map(myPoly, l1)(cellMapper)
    val v1 = map( myPoly, l1 )
    //val v1 = map[myPoly.type, HList](myPoly, l1)
    println( v1 )
    val l6 = HNil
    val l7 = l6.append( "Null" :: HNil )
    val v1a = map( myPoly, l7 )
    // https://issues.scala-lang.org/browse/SI-8286
    // https://gist.github.com/puffnfresh/8540756#comment-991433
    // https://issues.scala-lang.org/browse/SI-5070
    // http://stackoverflow.com/questions/31905870/implicit-conversion-classes-for-type-aliased-function-types-fail-to-compile-in-s
    val v2 = map( myPoly, l3 )
    println( v2 )
    // Prepend works, it is not recursive
    val v3 = map( myPoly, l5 )
    println( v3 )
    // No unpacking here, so it is good too
    val v4 = map0( myPoly, l3 )
  }

}

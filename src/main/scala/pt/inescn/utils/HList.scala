package pt.inescn.utils

import scala.language.higherKinds

sealed trait HList {
  type Prepend[ A ] <: HList
  type Append[ L <: HList ] <: HList

  def ::[ U ]( v: U ): Prepend[ U ]
  def prepend[ A ]( a: A ): Prepend[ A ]
  def append[ L <: HList ]( l: L ): Append[ L ]

  def map[ B ]( f: Any => B ): HList
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
 */

final case class HCons[ H, T <: HList ]( head: H, tail: T ) extends HList {

  type Prepend[ A ] = HCons[ A, HCons[ H, T ] ]
  type Append[ L <: HList ] = HCons[ H, T#Append[ L ] ]

  def ::[ U ]( v: U ): Prepend[ U ] = HCons( v, this )

  def prepend[ A ]( a: A ): Prepend[ A ] = HCons( a, this )
  def append[ L <: HList ]( l: L ): Append[ L ] = HCons( head, tail.append( l ) )

  def map[ B ]( f: Any => B )  = HCons( f( head ), tail.map( f ) )

  /*
  def map[ B ]( f: Any => B ) = {
    def loop( list: HList, acc: HList ): HList = {
      list match {
        case HNil => acc
        case HCons( h, t ) =>
          println( h )
          loop( t, f(h) :: acc )
      }
    }
    loop( this, HNil )
  }*/
  
}

//sealed class HNil extends HList {
case object HNil extends HList {
  type Prepend[ A ] = HCons[ A, HNil.type ]
  type Append[ L <: HList ] = L

  def ::[ T ]( v: T ): Prepend[ T ] = HCons( v, this ) // No need to type explicitly, same as prepend

  def prepend[ A ]( a: A ): Prepend[ A ] = HCons( a, this )
  def append[ L <: HList ]( l: L ): Append[ L ] = l

  def map[ B ]( f: Any => B ) = HNil
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

  def genCartesian[ A, B ]( list: HList, acc: HList ): HList = {
    list match {
      case HNil => acc
      case h :: t =>
        println( h )
        genCartesian( t, h :: acc )
    }
  }

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

    genCartesian( l3, HNil )
    genCartesian( HNil, HNil )

    val l6 = List( 1, 2 ) :: List( 3.0, 4.0 ) :: HNil
    val l7 = genCartesian( l6, HNil )
    println( l7 )
    val l8 = l7.map { x => x }
    println( l8 )
  }

}
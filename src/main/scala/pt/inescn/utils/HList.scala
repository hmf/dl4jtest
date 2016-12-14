package pt.inescn.utils

sealed trait HList

/**
 * @see https://apocalisp.wordpress.com/2010/06/08/type-level-programming-in-scala/
 * @see https://apocalisp.wordpress.com/2010/07/06/type-level-programming-in-scala-part-6a-heterogeneous-list%C2%A0basics/
 * @see https://github.com/runarorama
 * @see http://blog.higher-order.com/ (Rúnar Bjarnason)
 * @see http://pchiusano.github.io/ (Paul Chiusano)
 */


final case class HCons[ H, T <: HList ]( head: H, tail: T ) extends HList {
  def ::[ U ]( v: U ) = HCons( v, this )
}

sealed class HNil extends HList {
  def ::[ T ]( v: T ) = HCons( v, this )
}

// aliases for building HList types and for pattern matching
object HList {
  type ::[ H, T <: HList ] = HCons[ H, T ]
  
  val :: = HCons
  val HNil = new HNil
}

//object HNil extends HNil

object HListExample {
  import HList._
  
  def y = HList
  val z = HList
  type  w = String :: Boolean :: Double :: HNil
  
  
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
    case _                        =>  "unknown" 
  }
  
  val f1: ( w ) => String = {
    case "s" :: false :: _        => "test"
    case h :: true :: 1.0 :: HNil => h
    // compilation error because of individual type mismatches and length mismatch
    //case 3 :: "i" :: HNil => "invalid"
    case _                        =>  "unknown" 
  }
  
  
}
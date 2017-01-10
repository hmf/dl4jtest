package pt.inescn.scratchpad

/**
 * Example of how
 *
 * @see https://groups.google.com/forum/#!topic/shapeless-dev/1EI204GKdm8
 *
 */

object TestHList {
// https://groups.google.com/forum/#!topic/shapeless-dev/1EI204GKdm8
  //import pt.inescn.utils.HList._ // Don't import the :: type otherwise it will create a conflict with the Lists's 
  import pt.inescn.utils.HNil
  import pt.inescn.utils.HList.{ :: => #:: } // rename the type for compatibility
  import pt.inescn.utils.HList
  import pt.inescn.utils.HList.HNil

  // Avoid issues with Lists :: constructor/de-constructor
  //type  w = String :: Boolean :: Double :: HNil
  type w = String #:: Boolean #:: Double #:: HNil

  val y = 0.001 :: HNil

  val l1 = List( 1 )
  val l2 = 0 :: l1

  l2 match {
    case h :: t => println( "ok" )
    case Nil    => println( "ok" )
  }

  ( l2: List[ Int ] ) match {
    case h :: t => println( "ok" )
    case Nil    => println( "ok" )
  }
}
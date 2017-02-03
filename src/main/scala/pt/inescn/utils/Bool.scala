package pt.inescn.utils

import scala.language.higherKinds

/**
 * https://apocalisp.wordpress.com/2010/06/13/type-level-programming-in-scala-part-3-boolean/
 */
sealed trait Bool {
  type If[ T <: Up, F <: Up, Up ] <: Up
}
sealed trait True extends Bool {
  type If[ T <: Up, F <: Up, Up ] = T
}
sealed trait False extends Bool {
  type If[ T <: Up, F <: Up, Up ] = F
}

object Bool {
  type &&[ A <: Bool, B <: Bool ] = A#If[ B, False, Bool ]
  type ||[ A <: Bool, B <: Bool ] = A#If[ True, B, Bool ]
  type Not[ A <: Bool ] = A#If[ False, True, Bool ]

  class BoolRep[ B <: Bool ]( val value: Boolean )
  implicit val falseRep: BoolRep[ False ] = new BoolRep( false )
  implicit val trueRep: BoolRep[ True ] = new BoolRep( true )

  def toBoolean[ B <: Bool ]( implicit b: BoolRep[ B ] ): Boolean = b.value

}

// https://github.com/sarveshseri/scala-resources

object BoolExample {

  def main( args: Array[ String ] ) {
    type Rep[ A <: Bool ] = A#If[ Int, Long, AnyVal ]

    val v1 = implicitly[ Rep[ True ] =:= Int ]
    // val v2 = implicitly[ Rep[False] =:= Int ] // Cannot prove that Rep[pt.inescn.utils.False] =:= Int
    val v3 = implicitly[ Rep[ False ] =:= Long ]

    import Bool._

    val v4 = implicitly[ True && False =:= False ]
    val v5 = implicitly[ True && False || Not[ False ] =:= True ]

    val v6 = toBoolean[ True && False || Not[ False ] ]
    println( v6 )
    val v7 = toBoolean[ True && False ]
    println( v7 )
  }
}
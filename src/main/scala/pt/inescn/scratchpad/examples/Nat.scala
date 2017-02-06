package pt.inescn.scratchpad.examples

import scala.language.higherKinds

/**
 * https://apocalisp.wordpress.com/2010/06/16/type-level-programming-in-scala-part-4a-peano-number-basics/
 *
 * sbt "run-main pt.inescn.utils.NatExample"
 */

/**
 * Same construction as the `If` (Church Boolean) used by the Bool type.
 * If the type is Zero (_0) then the If (Match) returns the false (zero) member.
 * If the type is Succ[N], this will return the Type NonZero[N]
 * This allows the de-construction of the Nat. See example code.
 */
sealed trait Nat {
  type Match[ NonZero[ N <: Nat ] <: Up, IfZero <: Up, Up ] <: Up

  type Compare[ N <: Nat ] <: Comparison
}

/**
 * For _0#Compare, the result is EQ if the other Nat is _0 and LT if it is not.
 */
sealed trait _0 extends Nat {
  type Match[ NonZero[ N <: Nat ] <: Up, IfZero <: Up, Up ] = IfZero

  type Compare[ N <: Nat ] = N#Match[ ConstLT, EQ, Comparison ]
  type ConstLT[ A ] = LT
}

/*
 * For Succ[N]#Compare, the result is GT if the other Nat is _0 and it recurses if it is not. 
 * The recursion compares M-1 to N. (Remember that N is Succ[N] – 1.)
 */
sealed trait Succ[ N <: Nat ] extends Nat {
  type Match[ NonZero[ T <: Nat ] <: Up, IfZero <: Up, Up ] = NonZero[ N ]

  type Compare[M <: Nat] =  M#Match[N#Compare, GT, Comparison]  
}

object Nat {
  type _1 = Succ[ _0 ]
  type _2 = Succ[ _1 ]
  type _3 = Succ[ _2 ]
  type _4 = Succ[ _3 ]
  type _5 = Succ[ _4 ]
  type _6 = Succ[ _5 ]
  type _7 = Succ[ _6 ]
  type _8 = Succ[ _7 ]
  type _9 = Succ[ _8 ]
}

sealed trait Comparison {
  type Match[ IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up ] <: Up

  type gt = Match[ False, False, True, Bool ]
  type ge = Match[ False, True, True, Bool ]
  type eq = Match[ False, True, False, Bool ]
  type le = Match[ True, True, False, Bool ]
  type lt = Match[ True, False, False, Bool ]
}

sealed trait GT extends Comparison {
  type Match[ IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up ] = IfGT
}
sealed trait LT extends Comparison {
  type Match[ IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up ] = IfLT
}
sealed trait EQ extends Comparison {
  type Match[ IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up ] = IfEQ
}

object NatExample {

  def main( args: Array[ String ] ) {

    // Example of de-constructing a Nat
    type ConstFalse[ A ] = False
    type Is0[ A <: Nat ] = A#Match[ ConstFalse, True, Bool ]

    val v1 = implicitly[ Is0[ _0 ] =:= True ]

    // Example of using the If (Match)
    import Nat._

    //val v2 = implicitly[Is0[_9] =:= True] // fails compilation
    val v2 = implicitly[ Is0[ _9 ] =:= False ] // fails compilation

    import Bool._
    
    // Testing Nat matching
    val v3 = toBoolean[ _0#Compare[_0]#eq ]
    println(v3)
    val v4 =  implicitly[_0#Compare[_0]#eq =:= True]
    
    // Converting to GT, LT, GE to True/False
    type t1 = GT#ge
    val v5 = implicitly[ t1 =:= True ]
    type t2 = Comparison#Match[ False, True, True, Bool ]
    val v6 = implicitly[ t2 =:= Comparison#ge ]

    type t3 = _0#Compare[_0]
    val t3 = implicitly[ t3 =:= EQ ]

    type t4 = _0#Compare[_1]
    val t4 = implicitly[ t4 =:= LT ]

    type t5 = _7#Compare[_0]
    val t5 = implicitly[ t5 =:= GT ]

    type t6 = _7#Compare[_1]
    val t6 = implicitly[ t6 =:= GT ]

/*
 sealed trait Succ[ N <: Nat ] extends Nat {
  type Match[ NonZero[ T <: Nat ] <: Up, IfZero <: Up, Up ] = NonZero[ N ]
  type Compare[M <: Nat] =  M#Match[N#Compare, GT, Comparison]  
}
         pt.inescn.utils.Nat._3#Match[
  [N <: pt.inescn.utils.Nat]N#Match[
  [A]pt.inescn.utils.LT,pt.inescn.utils.EQ,pt.inescn.utils.Comparison],
       pt.inescn.utils.GT,pt.inescn.utils.Comparison]     
 */
    val t7 = implicitly[ _1#Compare[_3] =:= LT ]

    println( toBoolean[ _0#Compare[_0]#eq ] )  // True
    println( toBoolean[ _0#Compare[_0]#lt ] ) // False
    println( toBoolean[ _3#Compare[_4]#le ] ) // True    
  }

}
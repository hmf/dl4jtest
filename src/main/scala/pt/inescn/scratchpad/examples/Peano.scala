package pt.inescn.scratchpad.examples

/**
 * This is an example of the use of recursive types to describe natural number. 
 * In addition to this i also shows the `Aux` pattern. This pattern (as implemented
 * by Sum, Prod and Fact) allows the use of input types and the generation of a 
 * result (assigned to the type alias Out).
 * We also show how a type can be "converted" to a value. 
 * 
 * References:
 * - Saddling the horse with type-classes in Scala
 *   @see http://rudairandamacha.blogspot.pt/search?updated-min=2012-01-01T00:00:00%2B01:00&updated-max=2013-01-01T00:00:00%2B01:00&max-results=2
 * - Introduction to Type-Level Programming
 *   @see http://rudairandamacha.blogspot.com/2012/02/type-level-programming.html
 * - Shapeless by Miles Sabin,
 *   @see https://github.com/milessabin/shapeless
 *
 * NOTE: debugging - 
 *  Using the compiler
 *  ~/dev/scala/scala-2.12.1/bin/scalac -print ./src/main/scala/pt/inescn/scratchpad/examples/Peano.scala
 *  Usig the REPL
 *  ~/dev/scala/scala-2.12.1/bin/scala -Xprint:typer -e "for (i <- 0 to 100) yield i"
 *   May also work, but you cannot define packages. Must be a valid script. 
 *  ~/dev/scala/scala-2.12.1/bin/scala -print ./src/main/scala/pt/inescn/scratchpad/examples/Peano.scala
 *  ~/dev/scala/scala-2.12.1/bin/scala -print
 *     Then write ans test the code.
 *   
 *   -Xprint:<phase>
 *     scalac -Xshow-phases
 *   Parser phase not enough
 *     ~/dev/scala/scala-2.12.1/bin/scalac -Xprint:parser ./src/main/scala/pt/inescn/scratchpad/examples/Peano.scala
 *   Too much information
 *     ~/dev/scala/scala-2.12.1/bin/scalac -Xprint:typer ./src/main/scala/pt/inescn/scratchpad/examples/Peano.scala
 *   
 * @see http://stackoverflow.com/questions/9891407/getting-the-desugared-part-of-a-scala-for-comprehension-expression
 * @see http://www.lihaoyi.com/Ammonite/
 * 
 * sbt "run-main pt.inescn.scratchpad.examples.Peano"
 */
object Peano {
  trait Nat
  trait Succ[ N <: Nat ] extends Nat

  object Nat {
    class _0 extends Nat
    type _1 = Succ[ _0 ]
    type _2 = Succ[ _1 ]
    type _3 = Succ[ _2 ]
    type _4 = Succ[ _3 ]
    type _5 = Succ[ _4 ]
    type _6 = Succ[ _5 ]
    type _7 = Succ[ _6 ]
    type _8 = Succ[ _7 ]
    type _9 = Succ[ _8 ]

    def toInt[ N <: Nat ]( implicit ev: ToInt[ N ] ) = ev()
    def toInt[ A <: Nat, B <: Nat ]( f: Fact[ A ] )( implicit ev: FactAux[ A, B ], iv: ToInt[ B ] ) = iv()
    def toInt[ A <: Nat, B <: Nat, C <: Nat ]( f: Sum[ A, B ] )( implicit ev: SumAux[ A, B, C ], iv: ToInt[ C ] ) = iv()
    def toInt[ A <: Nat, B <: Nat, C <: Nat ]( f: Prod[ A, B ] )( implicit ev: ProdAux[ A, B, C ], iv: ToInt[ C ] ) = iv()
  }

  trait ToInt[ N <: Nat ] {
    def apply(): Int
  }

  object ToInt {
    import Nat._0

    implicit val toInt0 = new ToInt[ _0 ] { def apply() = 0 }
    implicit def toInt[ N <: Nat ]( implicit ev: ToInt[ N ] ) =
      new ToInt[ Succ[ N ] ] { def apply() = 1 + ev() }
  }

  /**
   * Important note: make sure that each case has a different function name.
   * If not, compilation will fail silently without warning of multiple available implicits.
   */
  trait SumAux[ A <: Nat, B <: Nat, C <: Nat ]

  object SumAux {
    import Nat._0

    implicit def add0[ A <: Nat ] = new SumAux[ A, _0, A ] {}
    // A + S(B) =  S(A) + B = C
    // recursion occurs when B is reduced to 0. so we only need to consider S(A',0)
    implicit def add[ A <: Nat, B <: Nat, C <: Nat ]( implicit ev: SumAux[ Succ[ A ], B, C ] ) = new SumAux[ A, Succ[ B ], C ] {}
  }

  trait ProdAux[ A <: Nat, B <: Nat, C <: Nat ]

  object ProdAux {
    import Nat._0

    implicit def prod0[ A <: Nat ] = new ProdAux[ A, _0, _0 ] {}
    // A * S(B) =  (A * B) + A 
    // A * B = D : recursion
    // D + A : do sum
    implicit def prod[ A <: Nat, B <: Nat, C <: Nat, D <: Nat ]( implicit ev1: ProdAux[ A, B, D ], ev2: SumAux[ A, D, C ] ) =
      new ProdAux[ A, Succ[ B ], C ] {}
  }

  trait FactAux[ A <: Nat, B <: Nat ]

  object FactAux {
    import Nat.{ _0, _1 }

    implicit def fact0 = new FactAux[ _0, _1 ] {}
    // S(A)! = A! * S(A)  
    // A!  = D : recursion
    // D * S(A) : do product
    implicit def fact[ A <: Nat, B <: Nat, C <: Nat, D <: Nat ]( implicit ev1: FactAux[ A, D ], ev2: ProdAux[ Succ[ A ], D, C ] ) =
      new FactAux[ Succ[ A ], C ] {}
  }

/*
 * What follows are a set of type definitions that allow us to use the types XXXAux to generate results.
 * In other word, we provide inputs and get output. 
*/

  trait Fact[ A <: Nat ] {
    type Out <: Nat
  }

  object Fact {
    implicit def fact[ A <: Nat, B <: Nat ]( implicit ev: FactAux[ A, B ] ) = new Fact[ A ] { type Out = B }
  }

trait Sum[ A <: Nat, B <: Nat ] {
    type Out <: Nat
  }

  object Sum {
    implicit def sum[ A <: Nat, B <: Nat, C <: Nat ]( implicit ev: SumAux[ A, B, C ] ) = new Sum[ A, B ] { type Out = C }
  }

trait Prod[ A <: Nat, B <: Nat ] {
    type Out <: Nat
  }

  object Prod {
    implicit def sum[ A <: Nat, B <: Nat, C <: Nat ]( implicit ev: ProdAux[ A, B, C ] ) = new Prod[ A, B ] { type Out = C }
  }
 
  def main( args: Array[ String ] ) {
    import Nat._

    val pp0 = toInt[ _0 ]
    println( pp0 )

    // val x = toInt[X]
    implicitly[ SumAux[ _2, _3, _5 ] ]
    implicitly[ SumAux[ _4, _5, _9 ] ]
    val t0 = implicitly[ Sum[ _2, _3 ] ] // This is an object
    val r0 = toInt( implicitly[ Sum[ _2, _3 ] ] ) // uses the Nat object toInt function
    assert( r0 == 5 )

    implicitly[ ProdAux[ _2, _3, _6 ] ]
    implicitly[ ProdAux[ _2, _4, _8 ] ]
    val t1 = implicitly[ Prod[ _2, _3 ] ]
    val r1 = toInt( implicitly[ Prod[ _2, _3 ] ] ) // uses the Nat object toInt function
    assert( r1 == 6 )

    val x = FactAux.fact0
    implicitly[ FactAux[ _1, _1 ] ]
    implicitly[ FactAux[ _2, _2 ] ]
    implicitly[ FactAux[ _3, _6 ] ]
    val t2 = implicitly[ Fact[ _4 ] ]
    val r2 = toInt( implicitly[ Fact[ _4 ] ] ) // uses the Nat object toInt function
    assert( r2 == 24 )
    
    //type T = t2.Out
  }

}
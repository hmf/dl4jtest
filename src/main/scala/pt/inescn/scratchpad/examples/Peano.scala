package pt.inescn.scratchpad.examples

/**
 * sbt "run-main pt.inescn.scratchpad.examples.Peano"
 */
object Peano {
  trait Nat
  trait Succ[ N <: Nat ] extends Nat

  object Nat {
    class _0 extends Nat
   type _1 = Succ[_0]
   type _2 = Succ[_1]
   type _3 = Succ[_2]
   type _4 = Succ[_3]
   type _5 = Succ[_4]
   type _6 = Succ[_5]
   type _7 = Succ[_6]
   type _8 = Succ[_7]
   type _9 = Succ[_8]
   
    def toInt[N <: Nat](implicit ev : ToInt[N]) = ev() 
  }

  trait ToInt[N <: Nat] {
    def apply() : Int
  }
  
  object ToInt {
    import Nat._0
    
    implicit val toInt0  = new ToInt[_0] { def apply() = 0 }
    implicit def toInt[N <: Nat](implicit ev : ToInt[N])  = 
      new ToInt[Succ[N]] { def apply() = 1 + ev() }
  }

  /**
   * Important note: make sure that each case has a different function name.
   * If not, compilation will fail silently without warning of multiple available implicits.
   */
  trait Sum[A <: Nat, B <: Nat, C <: Nat]
  
  object Sum {
    import Nat._0
    
    implicit def add0[A <: Nat] =  new Sum[A, _0, A] {}
    // A + S(B) =  S(A) + B = C
    // recursion occurs when B is reduced to 0. so we only need to consider S(A',0)
    implicit def add[A <: Nat, B <: Nat, C <: Nat](implicit ev : Sum[Succ[A],B,C]) =  new Sum[A, Succ[B], C] { }    
  }

  trait Prod[A <: Nat, B <: Nat, C <: Nat]
  
  object Prod {
    import Nat._0
    
    implicit def prod0[A <: Nat] =  new Prod[A, _0, _0] {}
    // A * S(B) =  (A * B) + A 
    // A * B = D : recursion
    // D + A : do sum
    implicit def prod[A <: Nat, B <: Nat, C <: Nat, D <: Nat](implicit ev1 : Prod[A,B,D], ev2 : Sum[A,D,C]) =  
      new Prod[A, Succ[B], C] { }    
  }

  trait Fact[A <: Nat, B <: Nat]
  
  object Fact {
    import Nat.{_0, _1}
    
    implicit def fact0 =  new Fact[_0, _1] {}
    // S(A)! = A! * S(A)  
    // A!  = D : recursion
    // D * S(A) : do product
    //implicit def fact[A <: Nat, B <: Nat, C <: Nat, D <: Nat](implicit ev1 : Fact[A,D], ev2 : Prod[A,D,C]) =  
    implicit def fact[A <: Nat, B <: Nat, C <: Nat, D <: Nat](implicit ev1 : Fact[A,D],  ev2 : Prod[Succ[A],D,C]) =  
      new Fact[Succ[A], C] {  println("Fact[Succ[A], C]") }    
  }
  
  def main( args: Array[ String ] ) {
    import Nat._

    val pp0 = toInt[_0]
    println( pp0 )

    // TODO: extract solution
    type S[X <: Nat] = Sum[_2,_3, X]
    // val x = toInt[X]
    implicitly[Sum[_2,_3,_5]]
    implicitly[Sum[_4,_5,_9]]
    // TODO: equals
    // implicitly[Sum[_5,_5, X] =:= Sum[_6,_4,X] ]

    implicitly[Prod[_2,_3,_6]]
    implicitly[Prod[_2,_4,_8]]

    val x = Fact.fact0
    type F[X <: Nat] = Fact[_1,_1]
    implicitly[Fact[_1,_1]]
    implicitly[Fact[_2,_2]]
    implicitly[Fact[_3,_6]] 
  }

}
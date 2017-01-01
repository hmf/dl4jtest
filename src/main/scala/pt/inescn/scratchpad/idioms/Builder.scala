package pt.inescn.scratchpad.idioms

trait A {
  val x: Int
  def specializedFunc: Int
}

object A {
  
  // Builder function
  def apply( x: Int ): A = new MySpecialA( x )

  // This class is hidden, it can only be constructed via the A object
  private class MySpecialA( val x: Int ) extends A {
    val specializedFunc = 42
  }
}

object Builder {
  val a1 = A(1)
  println(a1.specializedFunc)
  
  // Compile error: not found: type MySpecialA
  //import A._
  //val a2 = new MySpecialA(2)
}
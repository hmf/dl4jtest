package pt.inescn.scratchpad.idioms

/**
 * This is an example of the builder pattern that hides the class's constructor.
 * We do this by using the `private qualifier`. 
 */

trait A {
  val x: Int
  def specializedFunc: Int
}

// This class is hidden, it cannotbe constructed
private class MySpecialB( val x: Int ) extends A {
    val specializedFunc = 24
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
  //val a3 = new MySpecialB(2)
  
}
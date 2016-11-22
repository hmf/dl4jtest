package pt.inescn.scratchpad

/**
 *  Example of the cake pattern
 */


trait FooAble {
  def foo() = "here is your Foo"
}

trait MyFooAble extends FooAble {
  override def foo() = "here is my Foo"
}

trait BazAble {
  def baz() = "baz too"
}

class BarUsingFooAble {
  this: FooAble => //see type annotation

  def bar() = "Bar calls Foo: " + foo()
}

class BarUsingFooBazAble {
  this: FooAble with BazAble =>

  def bar() = s"Bar calls Foo: ${foo()} and Baz: ${baz()}"
}

trait FooAbleComponent {
  val fooAble: AFooAble
  class AFooAble {
    def foo() = "here is a foo"
  }
}

trait BazAbleComponent {
  val bazAble: ABazAble
  class ABazAble {
    def baz() = "a baz too"
  }
}

class ABarUsingFooAble {
  this: FooAbleComponent with BazAbleComponent =>

  def bar() = s"bar calls foo: ${fooAble.foo()} and baz: ${bazAble.baz()}"
}

/*
 * Example of the Parfait pattern. See Dick Wall. 
 */

object BarHelper {
  implicit object IFooAble extends FooAble {}
  implicit object IBazAble extends BazAble {}
}

class IBarUsingFooAble(implicit val fooAble: FooAble) {
  def bar() = s"IBar calls IFoo: ${fooAble.foo()}"
}

class IBarUsingFooBazAble(implicit val fooAble: FooAble, implicit val bazAble: BazAble) {
  def bar() = s"IBar calls IFoo: ${fooAble.foo()} and IBaz : ${bazAble.baz()}"
}

/**
 * sbt "run-main pt.inescn.scratchpad.DependencyInjection"
 */
object DependencyInjection {
  def main( args: Array[ String ] ) {

    // Cake pattern 
    val barWithFoo = new BarUsingFooAble with FooAble
    println( barWithFoo.bar() )

    val barWithMyFoo = new BarUsingFooAble with MyFooAble
    println( barWithMyFoo.bar() )

    val barWithFooBaz = new BarUsingFooBazAble with MyFooAble with BazAble
    println( barWithFooBaz.bar() )

    val aBarWithFoo = new ABarUsingFooAble with FooAbleComponent with BazAbleComponent {
      val bazAble = new ABazAble() //or any other implementation
      val fooAble = new AFooAble() //or any other implementation
    }
    println( aBarWithFoo.bar() )
    
    // Parfait pattern
    import BarHelper._
    
    val aIBarUsingFooAble = new IBarUsingFooAble()
    println( aIBarUsingFooAble.bar() )
    
    val aIBarUsingFooBazAble = new IBarUsingFooBazAble()
    println( aIBarUsingFooBazAble.bar() )
    
  }
  
}
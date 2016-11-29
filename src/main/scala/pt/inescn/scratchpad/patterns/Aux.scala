package pt.inescn.scratchpad.patterns

/**
 * http://gigiigig.github.io/tlp-step-by-step/introduction.html
 * http://gigiigig.github.io/posts/2015/09/13/aux-pattern.html
 */
object Aux {

  // Path Dependent Types
  // ..............................................

  class Foo {
    class Bar
    def bar: Bar = ???
  }

  // Aux.Foo
  val foo1 = new Foo
  // Aux.Foo
  val foo2 = new Foo

  // Aux.Foo.Bar
  val a: Foo#Bar = new foo1.Bar
  // Aux.Foo.Bar
  val b: Foo#Bar = new foo2.Bar

  val c: foo1.Bar = new foo1.Bar
  // type mismatch; found : pt.inescn.scratchpad.patterns.Aux.foo1.Bar required:  pt.inescn.scratchpad.patterns.Aux.foo2.Bar
  //val d: foo2.Bar = new foo1.Bar

  // Parameter Dependent Types
  // ..........................................................

  def foo( f: Foo ): f.Bar = f.bar

  val e: foo1.Bar = foo( foo1 )
  // type mismatch; found : pt.inescn.scratchpad.patterns.Aux.foo1.Bar required: pt.inescn.scratchpad.patterns.Aux.foo2.Bar
  // val f: foo2.Bar = foo( foo1 )

  // Abstract Type 
  // .............................

  trait AFoo {
    type T
    def value: T
  }

  object FooString extends AFoo {
    type T = String
    def value: T = "ciao"
  }

  object FooInt extends AFoo {
    type T = Int
    def value: T = 1
  }

  def getValue( f: AFoo ): f.T = f.value

  val fs: String = getValue( FooString )
  val fi: Int = getValue( FooInt )

  // type is not an alias!
  type ST = String

  // this is not just an alias anymore, it is actually a function, that takes T as parameter and returns Either[String, T] as a result
  type Result[ T ] = Either[ String, T ]

  // Infix Operator
  // ................................

  object Foo {
    def bar( s: String ) = println( s )
  }

  Foo.bar( "hello" ) // standard
  Foo bar "hello" // infix 

  // In the same way we can use the infix operator for types, basically what we can do is:
  trait OFoo[ A, B ]

  type Test1 = OFoo[ Int, String ] // standard
  type Test2 = Int OFoo String // infix

  trait ::[ A, B ]

  type Test3 = ::[ Int, String ]
  type Test4 = Int :: String

  // Phantom Types
  // See PhantomTypes.scala
   
  
}
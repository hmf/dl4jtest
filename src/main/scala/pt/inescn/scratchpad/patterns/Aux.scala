package pt.inescn.scratchpad.patterns

/**
 *
 * sbt "run-main pt.inescn.scratchpad.patterns.Aux"
 *
 * http://gigiigig.github.io/tlp-step-by-step/introduction.html
 * http://gigiigig.github.io/posts/2015/09/13/aux-pattern.html
 */
object Aux {

  // Path Dependent Types
  // ..............................................

  class Foo {
    class Bar
    def bar: Bar = new Bar
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

  // Implicit Parameters
  // see also http://www.cakesolutions.net/teamblogs/demystifying-implicits-and-typeclasses-in-scala
  // see als http://eed3si9n.com/implicit-parameter-precedence-again
  implicit val value = 3
  def fooi( implicit i: Int ) = println( i )
  fooi

  trait Printer[ T ] {
    def print( t: T ): String
  }

  implicit val sp: Printer[ Int ] = new Printer[ Int ] {
    def print( i: Int ) = i.toString
  }

  def foo[ T ]( t: T )( implicit p: Printer[ T ] ) = p.print( t )

  val res = foo( 3 )
  println( s"res: ${res}" )
  /*
   * Remove the companion object: 
   *could not find implicit value for parameter p:  pt.inescn.scratchpad.patterns.Aux.Printer[Boolean]
   * not enough arguments for method foo: (implicit p: pt.inescn.scratchpad.patterns.Aux.Printer[Boolean])String. Unspecified value parameter p.
   */
  val res1 = foo( false )
  println( s"res1: ${res1}" )

  // The compiler, when looking for an implicit for a certain type, will look automatically in his companion object
  // Must provide all of them
  object Printer {
    implicit val sb: Printer[ Boolean ] = new Printer[ Boolean ] {
      def print( i: Boolean ) = i.toString
    }
    implicit val sp: Printer[ Int ] = new Printer[ Int ] {
      def print( i: Int ) = i.toString
    }

    // See section on : Multi-step Resolution
    implicit def optionPrinter[ V ]( implicit pv: Printer[ V ] ): Printer[ Option[ V ] ] =
      new Printer[ Option[ V ] ] {
        def print( ov: Option[ V ] ) = ov match {
          case None      => "None"
          case Some( v ) => s"Option[${pv.print( v )}]"
        }
      }

    implicit def listPrinter[ V ]( implicit pv: Printer[ V ] ): Printer[ List[ V ] ] =
      new Printer[ List[ V ] ] {
        def print( ov: List[ V ] ) = ov match {
          case Nil          => "Nil"
          case l: List[ V ] => s"List[${l.map( pv.print ).mkString( ", " )}]"
        }
      }
  }
  val res2 = foo( false )
  println( s"res2: ${res2}" )

  // Converting types

  trait Resolver[ T, R ] {
    def resolve( t: T ): R
  }

  object Resolver {
    implicit val ib: Resolver[ Int, Boolean ] = new Resolver[ Int, Boolean ] {
      def resolve( i: Int ): Boolean = i > 1
    }
    implicit val sd: Resolver[ String, Double ] = new Resolver[ String, Double ] {
      def resolve( i: String ): Double = i.length.toDouble
    }
  }

  def foor[ T, R ]( t: T )( implicit p: Resolver[ T, R ] ): R = p.resolve( t )

  val res3: Boolean = foor( 3 )
  val res4: Double = foor( "ciao" )

  // Multi-step Resolution
  def print[ T ]( t: T )( implicit p: Printer[ T ] ) = p.print( t )

  val res5 = print( Option( List( 1, 3, 6 ) ) )
  println( s"res5: ${res5}" )

  // http://www.cakesolutions.net/teamblogs/demystifying-implicits-and-typeclasses-in-scala
  /*
  trait CanFoo[A] {  
    def foos(x: A): String
   }
  
  case class Wrapper(wrapped: String)
  object WrapperCanFoo extends CanFoo[Wrapper] {  
    def foos(x: Wrapper) = x.wrapped
  }
  */
  trait CanFoo[ A ] {
    def foos( x: A ): String
  }

  object CanFoo {
    def apply[ A: CanFoo ]: CanFoo[ A ] = implicitly
  }

  case class Wrapper( wrapped: String )
  implicit object WrapperCanFoo extends CanFoo[ Wrapper ] {
    def foos( x: Wrapper ) = x.wrapped
  }

  def wfoo[ A: CanFoo ]( thing: A ) = CanFoo[ A ].foos( thing )

  wfoo( Wrapper( "hi" ) ) // "hi"

  // CanFoo and companion object in scope
  implicit class CanFooOps[ A: CanFoo ]( thing: A ) {
    def foo = CanFoo[ A ].foos( thing )
  }

  def ufoo[ A: CanFoo ]( thing: A ) = thing.foo

  ufoo( Wrapper( "hi" ) ) // "hi"

  // https://apocalisp.wordpress.com/2010/06/10/type-level-programming-in-scala-part-2-implicitly-and/

  // Same type =:=
  implicitly[ Int =:= Int ]
  // Cannot prove that Int =:= String.
  // - not enough arguments for method implicitly: (implicit e: 
  // =:=[Int,String])=:=[Int,String]. Unspecified value parameter e.
  //implicitly[Int =:= String]

  // Conformance
  implicitly[ Int <:< AnyVal ]
  // Multiple markers at this line:
  // - Cannot prove that Int =:= AnyVal.
  // - not enough arguments for method implicitly: (implicit e: 
  // =:=[Int,AnyVal])=:=[Int,AnyVal]. Unspecified value parameter e.
  //implicitly[Int =:= AnyVal]

  // http://www.scala-lang.org/files/archive/spec/2.12/03-types.html
  // conversion not in 2.12
  // implicitly[Int <: Long]

  trait TFoo[ A ] {
    type B
    def value: B
  }

  implicit def tfi = new TFoo[ Int ] {
    type B = String
    val value = "Foo"
  }
  
  implicit def tfb = new TFoo[ Boolean] {
    type B = String
    val value = "TFoo"
  }
  
  implicit def tfs = new TFoo[ String ] {
    type B = Boolean
    val value = false
  }

  // we can change the return type of a function using dependent type and the implicit resolution
  def tfoo[ T ]( t: T )( implicit f: TFoo[ T ] ): f.B = f.value
  val res6: String = tfoo( 2 )
  val res7: Boolean = tfoo( "" )

  trait TBar[ T ] {
    def value : T
  }

  implicit def bi = new TBar[ Int ] {
    type B = String
    val value = 100
  }

  implicit def bb = new TBar[ Boolean ] {
    type B = String
    val value = true
  }
  
  implicit def bs = new TBar[ String ] {
    type B = Boolean
    val value = "-TBar[String]"
  }

  // let’s suppose that we want to use this type as type parameter in the next parameter, 
  // for instance to get the Bar instance for that type
  // Multiple markers at this line:
  // - illegal dependent method type: parameter may only be referenced in a subsequent parameter section
  // - value zero is not a member of pt.inescn.scratchpad.patterns.Aux.Bar[f.B]
  //def sfoo[T](t: T)(implicit f: TFoo[T], m: Bar[f.B]): f.B = m.zero

  /**
   * "What we are doing here is defining a type alias where A0 is mapped to Foo A and B0 is mapped to type B,
   * what I didn’t understand at the beginning is that the relation type B = B0 works both ways, so if we fix the
   * type for B like with type B = Boolean, B0 will get this type too.
   */
  type Aux[ A0, B0 ] = TFoo[ A0 ] { type B = B0 }

  def kungfoo[ T, R ]( t: T )( implicit f: Aux[ T, R ], m: TBar[ R ] ): R = m.value

  val res8: String = kungfoo( 2 )                                 // [Int, String]
  val res9 = kungfoo[Boolean, String]( true )              // [Boolean, String]
  val res10 = kungfoo( "H" )                                        // [String, Boolean]
  println( s"res10: ${res10}" )
  
  
  def main( args: Array[ String ] ) {
  }
}


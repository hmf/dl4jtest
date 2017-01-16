package pt.inescn.scratchpad.examples

/**
 * 
 * Scala implicits: creating an HList that can be mapped over
 * Posted on October 11, 2015 by Sarunas Valaskevicius 
 * 
 * Why?
 * ------
 * One of the main features of Scala are implicits. They add the power to create type classes in a similar 
 * way to Haskell. However, as much as they are useful, they are also somewhat complex to grok.
 * 
 * When is an implicit passed? What if there are a few matches available? Where does Scala look for them?
 * To get a better understanding I’ve spent some time re-creating an HList functionality.
 * 
 * What is an HList?
 * -------------------
 * An HList, or a heterogeneous list, records elements of different types. These types are known at compile time, 
 * and all operations are fully type-checked in this phase.
 * The code snippet [above/ of the HList]  is based on the Apocalisp blog 
 * [https://apocalisp.wordpress.com/2010/07/06/type-level-programming-in-scala-part-6a-heterogeneous-list%C2%A0basics/].
 * 
 * To use it, we can define values such as this:
 *   val x = 12 :: "asd" :: () :: false :: "XX" :: 10 :: HNil
 *   
 *  Mapping over an HList
 *  One of the “not exactly trivial” operations on an HList is to map over it. To do so, we have to match the input 
 *  type with all types in the HList and apply the given function to the matching elements. Resulting type is a 
 *  modified HList, with some elements replaced by the given function.
 *  
 *  A mapper can be defined [as:/below]
 *  
 *  It takes an HList hl: HL, a function f from S to B, and generates an HList of statically generated type Out. 
 *  To both generate the resulting type, and the actual logic implementing the mapping, there is an implicit variable 
 *  ev used, that finds suitable Mapper instances for the given parameters.
 *  To create the required Mapper instances, a few implicit definitions are required:
 *  
 *  - one to check if the current HList head matches the requested function input type S;
 *  - another to iterate over HList if the current head does not match the type S;
 *  - and the last one to finish the iteration when there is an element of HNil found.
 *  
 *  Also, as type equality can only be done by finding an implicit evidence, and there is no functionality in scala to 
 *  support an explicit else case when there is no such evidence found, we’ll need to play with scala’s implicit 
 *  precedence rules.
 *  
 *  Fortunately, such precedence rules exist, and one of them tells that methods in derived classes are more 
 *  specific and will take precedence over the parent class methods. See Scala reference’s 6.26.3 
 *  [http://www.scala-lang.org/docu/files/ScalaReference.pdf] section.
 *  
 *  Given this knowledge, we can define the mapper instances as [below]:
 *  
 *  The implicit instance that will be checked first is typeMatchedMapper - which defines that the function f can 
 *  process and replace the current head element. Next one - iteratedMapper - is simply passing the function 
 *  f to the next tail processing Mapper without modifying the head. Finally, there is endOfIterationMapper, that only processes HNil, and returns it unchanged.
 *  
 *  Results
 *  --------
 *  Given the Mapper definitions above, we can test the map function by:
 *  
 *  The element x is printed as:
 *  HCons(12,HCons(asd,HCons((),HCons(false,HCons(XX,HCons(10,HNil$@a90bd3))))))
 *  
 *  Mapping a function from String to String:
 *  recurse
 *  apply
 *  recurse
 *  recurse
 *  apply
 *  recurse
 *  end.
 *  HCons(12,HCons(asdasd,HCons((),HCons(false,HCons(XXXX,HCons(10,HNil$@a90bd3))))))
 *  
 *  And similarly with others. If there is no matching element in the HList, the list is returned unchanged.
 *  Note: while writing our own HList is fun and provides a challenge for an exercise, for production code you 
 *  should look for an existing library - e.g. Shapeless [https://github.com/milessabin/shapeless].
 *  
 * @see http://www.hyperlambda.com/posts/hlist-map-in-scala/
 * 
 * sbt "run-main pt.inescn.scratchpad.examples.HListPriorityMapping"
 */
object HListPriorityMapping {

  // TODO: package this properly
  import pt.inescn.utils.HList._
  import pt.inescn.utils.HList
  import pt.inescn.utils.HCons
  import pt.inescn.utils.HNil

  sealed trait Mapper[ S, HL <: HList, B ] {
    type Out <: HList
    def apply( hl: HL, f: S => B ): Out
  }

  def map[ S, B, T <: HList ]( hc: T, f: S => B )( implicit ev: Mapper[ S, T, B ] ) = ev( hc, f )

  object Mapper extends LowPrioMapper {
    implicit def typeMatchedMapper[ S, H, T <: HList, B ]( implicit ev: H =:= S, iTail: Mapper[ S, T, B ] ): Mapper[ S, H :: T, B ] =
      new Mapper[ S, H :: T, B ] {
        type Out = B :: iTail.Out
        def apply( hc: H :: T, f: S => B ) = {
          println( "apply" )
          HCons( f( ev( hc.head ) ), iTail( hc.tail, f ) )
        }
      }
  }
  trait LowPrioMapper extends LowestPrioMapper {
    implicit def iteratedMapper[ S, H, T <: HList, B ]( implicit iTail: Mapper[ S, T, B ] ): Mapper[ S, H :: T, B ] =
      new Mapper[ S, H :: T, B ] {
        type Out = H :: iTail.Out
        def apply( hc: H :: T, f: S => B ) = {
          println( "recurse" )
          HCons( hc.head, iTail( hc.tail, f ) )
        }
      }
  }
  trait LowestPrioMapper {
    implicit def endOfIterationMapper[ S, HC <: HNil, B ]: Mapper[ S, HC, B ] =
      new Mapper[ S, HC, B ] {
        type Out = HNil
        def apply( hc: HC, f: S => B ) = {
          println( "end." )
          HNil
        }
      }
  }

  def main( args: Array[ String ] ) {

    val x = 12 :: "asd" :: () :: false :: "XX" :: 10 :: HNil

    println( x )

    println( map( x, ( a: String ) => a + a ) )
    println( map( x, ( a: Int ) => a + a ) )
    println( map( x, ( a: Boolean ) => a.toString ) )
    println( map( x, ( a: ( Boolean, Int ) ) => a.toString ) )
  }
}
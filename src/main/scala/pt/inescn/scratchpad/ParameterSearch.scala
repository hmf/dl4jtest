package pt.inescn.scratchpad

// Parameters are value classes

/**
 *
 * @see http://stackoverflow.com/questions/19642053/when-to-use-val-or-def-in-scala-traits
 */
trait Parameter[ T ] {
  def value: T // must be a def or var so that we can override and initialize from the extended class
  def na : T
}

trait ParameterRange[ T, B, E, S ] {
  var begin: Parameter[ T ]
  var end: Parameter[ T ]
  var step: Parameter[ T ]
}

// Can also use `abstract class`
trait BEGIN
trait END
trait STEP
trait MISSING

object ParameterRange {

  def apply[ T ](begin: Parameter[ T ], end: Parameter[ T ], step: Parameter[ T ] ): ParameterRange[ T, BEGIN, END, STEP] = 
    new HiddenParameterRange[T, BEGIN, END, STEP](begin, end, step)

  private class HiddenParameterRange[ T, B, E, S ](override val begin: Parameter[ T ], override val end: Parameter[ T ], override val step: Parameter[ T ]) 
    extends ParameterRange[ T, B, E, S ] {
    
    def this(begin: Parameter[ T ]) {
      this(begin, begin.na, begin.na)
    }
    
  }

  def from[T, B, E, S]( begin: Parameter[ T ]): ParameterRange[T, BEGIN, MISSING, MISSING] = 
    new HiddenParameterRange[T, BEGIN, MISSING, MISSING](begin)
  
  def to[ T ]( begin: Parameter[ T ], end: Parameter[ T ], step: Parameter[ T ] ): ParameterRange[T, BEGIN, END, STEP] = 
    new HiddenParameterRange[T, BEGIN, END, STEP](begin, end, step)
}

object test {
  val x = ParameterRange(learningRate(0.0), learningRate(0.01), learningRate(0.001))
  val y = ParameterRange.to(learningRate(0.0), learningRate(0.01), learningRate(0.001))
  
  import ParameterRange._
  val z = to(learningRate(0.0), learningRate(0.01), learningRate(0.001))
  
}



case class learningRate( override val value: Double ) extends Parameter[ Double ] { def na = Double.NaN }
case class pValue( override val value: Double ) extends Parameter[ Double ]

import scala.language.higherKinds

//import pt.inescn.utils.HList._  // cannot use this because of clash between List's and hList's :: type
import pt.inescn.utils.HList
import pt.inescn.utils.HNil
import pt.inescn.utils.HList.{ :: => #: } // rename the type for compatibility
//import pt.inescn.utils.HList.HNil

trait Model[ T, U[ T ], P ] {
  //type params = HList
  //val params : List[Parameter]
  val params: HList

  def fit( data: U[ T ] ): Unit
  def predict( datum: T ): P
}

object ModelAParams {
  type w = learningRate #: pValue #: HNil
}

// Ok
//class ModelA[ T, U[ T ] ]( val params: learningRate #: pValue #: HNil ) extends Model[ T, U, Double ] {
class ModelA[ T, U[ T ] ]( val params: ModelAParams.w ) extends Model[ T, U, Double ] {
  def fit( data: U[ T ] ): Unit = {}
  def predict( datum: T ): Double = { 0.0 }
}

//import scala.collection.parallel.ParIterableLike

trait ParameterSearch {}

/**
 * Generate cross product.
 * Cartesian product of arbitrary lists.
 *
 *  https://amplab.cs.berkeley.edu/wp-content/uploads/2015/07/163-sparks.pdf
 *
 * http://stackoverflow.com/questions/8321906/lazy-cartesian-product-of-several-seqs-in-scala
 * http://stackoverflow.com/questions/19382978/cartesian-product-stream-scala
 * http://stackoverflow.com/questions/13483931/functional-style-early-exit-from-depth-first-recursion
 */
object GridPramaterSearch extends ParameterSearch {

  def cartesian[ A ]( list: List[ List[ A ] ] ): List[ List[ A ] ] = {
    list match {
      case Nil    => List( List() )
      case h :: t => h.flatMap( i => cartesian( t ).map( i :: _ ) )
    }
  }

  def cartesian0[ A ]( list: List[ List[ A ] ] ): List[ List[ A ] ] = {
    list match {
      case Nil    => List( List() )
      case h :: t => h.flatMap( i => cartesian( t ).map( i :: _ ) )
    }
  }

  def cartesian2[ A, B ]( list: Seq[ Seq[ A ] ], acc: Seq[ A ], f: Seq[ A ] => B ): Unit = {
    list match {
      case Nil    => f( acc )
      case h :: t => h.map { x => cartesian2( t, x +: acc, f ) }
    }
  }

  def cartesian3[ A, B ]( list: Seq[ Seq[ A ] ], acc: Seq[ A ], f: Seq[ A ] => B ): Seq[ B ] = {
    list match {
      case Nil    => List( f( acc ) )
      case h :: t => h.flatMap { x => cartesian3( t, x +: acc, f ) }
    }
  }

  import annotation.tailrec

  /**
   * @see http://blog.dmitryleskov.com/programming/scala/stream-hygiene-i-avoiding-memory-leaks/
   * @see http://stackoverflow.com/questions/21141853/scala-streams-how-to-avoid-to-keeping-a-reference-to-the-head-and-other-elemen
   */
  //@tailrec 
  def cartesian4[ A, B ]( list: Stream[ Seq[ A ] ], acc: Seq[ A ], f: Seq[ A ] => B ): Stream[ B ] = {
    list match {
      case Stream.Empty => List( f( acc ) ).toStream
      case h #:: t      => h.toStream.flatMap { x => cartesian4( t, x +: acc, f ) }
    }
  }

  /**
   * Avoiding out of memory errors
   * 1.  Stream parameter is used by-name
   * 2. Do NOT hold the return as a val
   *
   * @see http://blog.dmitryleskov.com/programming/scala/stream-hygiene-i-avoiding-memory-leaks/
   * @see http://blog.dmitryleskov.com/programming/scala/stream-hygiene-ii-hotspot-kicks-in/
   * @see https://github.com/stew/dogs/blob/master/core/src/main/scala/dogs/Streaming.scala
   * @see http://stackoverflow.com/questions/21141853/scala-streams-how-to-avoid-to-keeping-a-reference-to-the-head-and-other-elemen
   */
  def cartesian5[ A, B ]( list: => Stream[ Seq[ A ] ], f: Seq[ A ] => B ): Stream[ B ] = {
    def loop( lst: => Stream[ Seq[ A ] ], acc: Seq[ A ] ): Stream[ B ] = {
      lst match {
        case Stream.Empty => List( f( acc ) ).toStream
        case h #:: t      => h.toStream.flatMap { x => loop( t, x +: acc ) }
      }
    }
    loop( list, List() )
  }

  def cartesian6[ A, B ]( list: Stream[ Seq[ A ] ], acc: Seq[ A ], f: Seq[ A ] => B ): Stream[ B ] = {
    list match {
      case Stream.Empty => List( f( acc ) ).toStream
      case h #:: t      => h.toStream.flatMap { x => cartesian4( t, x +: acc, f ) }
    }
  }

  /**
   * Example of a tail recursive call
   * @see http://stackoverflow.com/questions/10290189/how-to-make-this-recursive-method-tail-recursive-in-scala
   */
  def product[ T ]( listOfLists: List[ List[ T ] ] ): List[ List[ T ] ] = {
    @tailrec def innerProduct( listOfLists: List[ List[ T ] ], accum: List[ List[ T ] ] ): List[ List[ T ] ] =
      listOfLists match {
        case Nil       => accum
        case xs :: xss => innerProduct( xss, for ( y <- xs; a <- accum ) yield y :: a )
      }

    innerProduct( listOfLists.reverse, List( Nil ) )
  }

}

/**
 *
 * sbt "run-main pt.inescn.scratchpad.SearchParameters"
 */
object SearchParameters {

  def main( args: Array[ String ] ) {

    val l = List( List( 1, 2, 3 ), List( 4, 5, 6 ) )
    val c1 = GridPramaterSearch.cartesian( l )
    c1.foreach { x => println( x.mkString( "<", ",", ">" ) ) }

    val c2 = GridPramaterSearch.cartesian0( l )
    //c2.foreach { x => println( x.mkString("<", ",", ">") ) }

    GridPramaterSearch.cartesian2( l, List(), { x: Seq[ Int ] => println( x.mkString( "<", ",", ">" ) ) } )

    // Accumulate list before executing `for each` - will OOM
    val c3 = GridPramaterSearch.cartesian3( l, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c3.foreach { println }

    val c4 = GridPramaterSearch.cartesian4( l.toStream, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c4.foreach { println }

    //val l4a = List( 1 to 2500, 1 to 2500, 1 to 2500 )
    val l4a = List( 1 to 3, 1 to 3, 1 to 3 )
    // OOM at <575,1785,2> - use as def not val
    def c4a = GridPramaterSearch.cartesian4( l4a.toStream, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    // OOM , noting to circumvent this
    //val c4a = GridPramaterSearch.cartesian3( l4a, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c4a.foreach { println }

    // Cartesian 3 while constructing the list - java.lang.OutOfMemoryError: GC overhead limit exceeded (slow due to memoization)
    // Cartesian 3 printing the list is ok, but memoization causes problems - java.lang.OutOfMemoryError: GC overhead limit exceeded 
    // Depth of the search is the same as the the number of lists to process - so this is not an issue
    //val l1 = List( 1 to 250, 1 to 250, 1 to 250 )
    // -----> val l1 = List( 1 to 2500, 1 to 2500, 1 to 2500 )
    val l1 = List( 1 to 3, 1 to 3, 1 to 3 )
    //val c5 = GridPramaterSearch.cartesian3( l1, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    //val c5 = GridPramaterSearch.cartesian4(l1.toStream, List(), { x: Seq[Int] => x.mkString("<", ",", ">")  } )
    def c5 = GridPramaterSearch.cartesian4( l1.toStream, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c5.foreach { println }

    //import pt.inescn.utils.HList.HNil.{ :: => #: } // rename the type for compatibility

    //val w = "str" :: true :: 1.0 :: HList.HNil
    val x = "str" :: true :: 1.0 :: HNil
    val p1 = new learningRate( 0.001 ) :: new pValue( 0.05 ) :: HNil
    val m1 = new ModelA( p1 )
    // cannot use (compile)
    // m1.fit(1)
    //val p2 = new pValue( 0.05 ) :: new learningRate( 0.001 )  :: HNil
    // Compilation error
    // type mismatch;
    // [error]  found   : pt.inescn.utils.HCons[pt.inescn.scratchpad.pValue,pt.inescn.utils.HCons[pt.inescn.scratchpad.learningRate,pt.inescn.utils.HNil]]
    // [error]  required: pt.inescn.scratchpad.ModelAParams.w
    // [error]     (which expands to)  pt.inescn.utils.HCons[pt.inescn.scratchpad.learningRate,pt.inescn.utils.HCons[pt.inescn.scratchpad.pValue,pt.inescn.utils.HNil]]
    //val m2 = new ModelA(p2)   
    val p3 = 0.001 :: 0.05 :: HNil
    // Compilation error
    // type mismatch;
    // [error]  found   : pt.inescn.utils.HCons[Double,pt.inescn.utils.HCons[Double,pt.inescn.utils.HNil]]
    // [error]  required: pt.inescn.scratchpad.ModelAParams.w
    // [error]     (which expands to)  pt.inescn.utils.HCons[pt.inescn.scratchpad.learningRate,pt.inescn.utils.HCons[pt.inescn.scratchpad.pValue,pt.inescn.utils.HNil]]
    //val m3 = new ModelA(p3)   

    val ps = 0.001 to 1 by 0.1
    ps.toList.foreach( println )
    //val ps1 =  new learningRate( 0.001 ).value to  new learningRate( 1 ).value
    //val ps1 =  new learningRate( 0.001 ) to  new learningRate( 1 )
    println( new learningRate( 0.001 ).value )
    //println(new learningRate( 0.001 ).valuex)
  }
}
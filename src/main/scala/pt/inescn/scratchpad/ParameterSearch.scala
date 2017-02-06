package pt.inescn.scratchpad

import StreamBuilder._

//case class learningRate( override val value: Double ) extends SParameter[ Double ] { def apply( v: Double ) = new learningRate( v ) }
//case class ParamThree( override val value: Double ) extends SParameter[ Double ] { def apply( v: Double ) = new learningRate( v ) }

//import pt.inescn.scratchpad.ParamOne

import scala.language.higherKinds

/* TODO: remove
//import pt.inescn.utils.HList._  // cannot use this because of clash between List's and hList's :: type
import pt.inescn.scratchpad.examples.HList
import pt.inescn.scratchpad.examples.HNil
import pt.inescn.utils.HList.{ :: => #: } // rename the type for compatibility
//import pt.inescn.utils.HList.HNil
*/

import shapeless.{ :: => #:, HList, HNil }

trait Model[ T, U[ T ], P ] {
  //type params = HList
  //val params : List[Parameter]
  //val params: HList

  def fit( data: U[ T ] ): Unit
  def predict( datum: T ): P
}

/**
 *  An example of a parameter.
 */
case class ParamOne[ T ]( val value: T = 0.0 ) extends Parameter[ T ] with StrictSelf[ ParamOne[ T ] ] {
  type Self = ParamOne[ T ]

  def apply( v: T ): Self = new ParamOne( v )
}

case class ParamTwo[ T ]( val value: T = 0.0 ) extends Parameter[ T ] with StrictSelf[ ParamTwo[ T ] ] {
  type Self = ParamTwo[ T ]

  def apply( v: T ): Self = new ParamTwo( v )
}

case class ParamThree[T]( val value: T = 0 ) extends Parameter[ T ] with StrictSelf[ ParamThree[T]] {
  type Self = ParamThree[T]

  def apply( v: T ): Self = new ParamThree( v )
}

object ModelAParams {
  type w = ParamOne[ Double ] #: ParamTwo[ Double ] #: ParamThree[Int] #: HNil
}

// Ok
//class ModelA[ T, U[ T ] ]( val params: learningRate #: ParamThree #: HNil ) extends Model[ T, U, Double ] {
class ModelA[ T, U[ T ] ]( val params: ModelAParams.w ) extends Model[ T, U, Double ] {
  def fit( data: U[ T ] ): Unit = {
    val v1 = params.head
    val v2 = params.tail.head
    val v3 = params.tail.tail.head
  }
  def predict( datum: T ): Double = { 0.0 }
}

class ModelB[ T, U[ T ] ]( val v1 : ParamOne[_],  val v2 : ParamTwo[_], val3 : ParamThree[_]) extends Model[ T, U, Double ] {
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
      case Nil => f( acc )
      case h :: t =>
        println( s"acc = $acc" )
        println( s"h = $h" )
        println( s"t = $t" )
        h.map { x => cartesian2( t, x +: acc, f ) }
    }
  }

  def cartesian2a[ A, B ]( list: Seq[ Seq[ A ] ], acc: Seq[ A ], f: Seq[ A ] => B ): Seq[ B ] = {
    list match {
      case Nil =>
        println( s"acc* = $acc" )
        List( f( acc ) )
      case h :: t =>
        println( s"acc = $acc" )
        println( s"h = $h" )
        println( s"t = $t" )
        h.flatMap { x => cartesian2a( t, x +: acc, f ) }
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
    //@scala.annotation.tailrec
    def loop( lst: => Stream[ Seq[ A ] ], acc: Seq[ A ] ): Stream[ B ] = {
      lst match {
        case Stream.Empty => List( f( acc ) ).toStream
        case h #:: t      => h.toStream.flatMap { x => loop( t, x +: acc ) }
      }
    }
    loop( list, List() )
  }

  def cartesian6[ A, B ]( list: Stream[ Stream[ A ] ], acc: Seq[ A ], f: Seq[ A ] => B ): Stream[ B ] = {
    list match {
      case Stream.Empty => List( f( acc ) ).toStream
      case h #:: t      => h.flatMap { x => cartesian6( t, x +: acc, f ) }
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

  /**
   * @see http://codereview.stackexchange.com/questions/151408/cartesian-product-in-scala
   */
  def cartesianProduct[ T ]( in: Seq[ Seq[ T ] ] ): Seq[ Seq[ T ] ] = {
    @scala.annotation.tailrec
    def loop( acc: Seq[ Seq[ T ] ], rest: Seq[ Seq[ T ] ] ): Seq[ Seq[ T ] ] = {
      rest match {
        case Nil =>
          acc
        case seq :: remainingSeqs =>
          // Equivalent of: 
          // val next = seq.flatMap(i => acc.map(a => i+: a))
          val next = for {
            i <- seq
            a <- acc
          } yield i +: a
          loop( next, remainingSeqs )
      }
    }
    loop( Seq( Nil ), in.reverse )
  }

  /*
  def cartesian7[ A, B ]( list: Stream[ Stream[ A ] ], acc: Seq[ HList ], f: Seq[ HList ] => B ): Stream[ B ] = {
    list match {
      case Stream.Empty => List( f( acc ) ).toStream
      case h #:: t      => h.flatMap { x => cartesian7( t, x +: acc, f ) }
    }
  }*/

  //import pt.inescn.utils.HNil.{ :: => #: } // rename the type for compatibility

}

/**
 *
 * sbt "run-main pt.inescn.scratchpad.SearchParameters"
 */
object SearchParameters {

  def linSearch( from: Double, to: Double, by: Double ): Stream[ Double ] = {
    val len = ( ( to - from ) / by ).ceil.toInt
    linspace( from, by ).take( len + 1 )
  }

  def linISearch( from: Int, to: Int, by: Int ): Stream[ Int ] = {
    val len = ( ( to - from ) / by ).ceil.toInt
    linspace( from, by ).take( len + 1 )
  }

  def main( args: Array[ String ] ) {

    val l = List( List( 1, 2, 3 ), List( 4, 5, 6 ) )
    /*val c1 = GridPramaterSearch.cartesian( l )
    c1.foreach { x => println( x.mkString( "<", ",", ">" ) ) }

    val c2 = GridPramaterSearch.cartesian0( l )
    //c2.foreach { x => println( x.mkString("<", ",", ">") ) }
*/
    //GridPramaterSearch.cartesian2( l, List(), { x: Seq[ Int ] => println( x.mkString( "<", ",", ">" ) ) } )
    val c0 = GridPramaterSearch.cartesian2a( l, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    println( c0 )

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
    // ----> val l1 = List( 1 to 2500, 1 to 2500, 1 to 2500 )
    val l1 = List( 1 to 3, 1 to 3, 1 to 3 )
    //val c5 = GridPramaterSearch.cartesian3( l1, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    //val c5 = GridPramaterSearch.cartesian4(l1.toStream, List(), { x: Seq[Int] => x.mkString("<", ",", ">")  } )
    def c5 = GridPramaterSearch.cartesian5( l1.toStream, { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c5.foreach { println }

    println( "-----------------" )
    val l6 = List( ( 1 to 3 ).toStream, ( 1 to 3 ).toStream, ( 1 to 3 ).toStream )
    def c6 = GridPramaterSearch.cartesian6( l6.toStream, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c6.foreach { println }

    //import pt.inescn.utils.HList.HNil.{ :: => #: } // rename the type for compatibility

    //val w = "str" :: true :: 1.0 :: HList.HNil
    val x = "str" :: true :: 1.0 :: HNil
    val p1 = ParamOne( 0.001 ) :: ParamTwo( 0.05 ) :: ParamThree( 1 ) :: HNil
    val m1 = new ModelA( p1 )
    // cannot use (compile)
    // m1.fit(1)
    //val p2 = new ParamThree( 0.05 ) :: new learningRate( 0.001 )  :: HNil
    // Compilation error
    // type mismatch;
    // [error]  found   : pt.inescn.utils.HCons[pt.inescn.scratchpad.ParamThree,pt.inescn.utils.HCons[pt.inescn.scratchpad.learningRate,pt.inescn.utils.HNil]]
    // [error]  required: pt.inescn.scratchpad.ModelAParams.w
    // [error]     (which expands to)  pt.inescn.utils.HCons[pt.inescn.scratchpad.learningRate,pt.inescn.utils.HCons[pt.inescn.scratchpad.ParamThree,pt.inescn.utils.HNil]]
    //val m2 = new ModelA(p2)   
    val p3 = 0.001 :: 0.05 :: HNil
    // Compilation error
    // type mismatch;
    // [error]  found   : pt.inescn.utils.HCons[Double,pt.inescn.utils.HCons[Double,pt.inescn.utils.HNil]]
    // [error]  required: pt.inescn.scratchpad.ModelAParams.w
    // [error]     (which expands to)  pt.inescn.utils.HCons[pt.inescn.scratchpad.learningRate,pt.inescn.utils.HCons[pt.inescn.scratchpad.ParamThree,pt.inescn.utils.HNil]]
    //val m3 = new ModelA(p3)   

    val ps = 0.001 to 1 by 0.1
    ps.toList.foreach( println )
    //val ps1 =  new learningRate( 0.001 ).value to  new learningRate( 1 ).value
    //val ps1 =  new learningRate( 0.001 ) to  new learningRate( 1 )
    println( ParamOne( 0.001 ).value )
    //println(new learningRate( 0.001 ).valuex)

    val t1 = new ParameterRange( ParamOne( 0.0 ), ParamOne( 1.0 ), 0.4, linSearch )
    val t2 = new ParameterRange( ParamTwo( 0.0 ), ParamTwo( 1.0 ), 0.4, linSearch )
    val t3= new ParameterRange( ParamThree( 0 ), ParamThree( 10 ), 4, linISearch )
    //val t1 = t0.toStream
    //t1.foreach { x => println( x ) }
    
    val g1 = List( t1.toStream, t2.toStream, t3.toStream )
    //def g2 = GridPramaterSearch.cartesian5( l1.toStream, { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    def g2 = GridPramaterSearch.cartesian5( g1.toStream, { x: Seq[ Parameter[_] ] => x  } )
    //g2.foreach { println }

    
    //val m2 = new ModelB( ParamOne( 0.001 ), ParamTwo( 0.05 ), ParamThree(100) )
    //val m2 = new ModelB( ParamTwo( 0.001 ), ParamTwo( 0.05 ), ParamThree(100) )
    val pt1 = t1.toStream(1)
    val pt2 = t2.toStream(1)
    val pt3 = t3.toStream(1)
    val m2 = new ModelB( pt1, pt2, pt3 )
    val pa = g2(1)
    val pa_ = pa.toArray
    val pr1 = pa_(2)
    //val m2 = new ModelB( pr1, ParamTwo( 0.05 ), ParamThree(100) )
  }
}
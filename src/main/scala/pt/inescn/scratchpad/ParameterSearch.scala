package pt.inescn.scratchpad

trait ParameterSearch {}

/*
 * http://stackoverflow.com/questions/13483931/functional-style-early-exit-from-depth-first-recursion
 */

/**
 * Generate cross product.
 * Cartesian product of arbitrary lists.
 *
 * http://stackoverflow.com/questions/8321906/lazy-cartesian-product-of-several-seqs-in-scala
 * http://stackoverflow.com/questions/19382978/cartesian-product-stream-scala
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
    def loop( lst: => Stream[ Seq[ A ] ], acc: Seq[ A ] ): Stream[ B ]  = {
      lst match {
        case Stream.Empty => List( f( acc ) ).toStream
        case h #:: t      => h.toStream.flatMap { x => loop( t, x +: acc ) }
      }
    }
    loop( list, List() )
  }
  
  /*@tailrec
  def cartesian6[ A, B ]( list: => Stream[ Seq[ A ] ], f: Seq[ A ] => B ): Stream[ B ] = {
    def loop( lst: => Stream[ Seq[ A ] ], acc: Seq[ A ] ): Stream[ B ]  = {
      lst match {
        case Stream.Empty => List( f( acc ) ).toStream
        case h #:: t      => h.toStream.flatMap { x => loop( t, x +: acc ) }
      }
    }
    loop( list, List() )
  }*/

  /**
   * Example of a tail recursive call
   * @see http://stackoverflow.com/questions/10290189/how-to-make-this-recursive-method-tail-recursive-in-scala
   */
  def product[ T ]( listOfLists: List[ List[ T ] ] ): List[ List[ T ] ] = {
    @tailrec def innerProduct[ T ]( listOfLists: List[ List[ T ] ], accum: List[ List[ T ] ] ): List[ List[ T ] ] =
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

    val c3 = GridPramaterSearch.cartesian3( l, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c3.foreach { println }

    val c4 = GridPramaterSearch.cartesian4( l.toStream, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c4.foreach { println }

    // Cartesian 3 while constructing the list - java.lang.OutOfMemoryError: GC overhead limit exceeded (slow due to memoization)
    // Cartesian 3 printing the list is ok, but memoization causes problems - java.lang.OutOfMemoryError: GC overhead limit exceeded 
    // Depth of the search is the same as the the number of lists to process - so this is not an issue
    //val l1 = List( 1 to 250, 1 to 250, 1 to 250 )
    val l1 = List( 1 to 2500, 1 to 2500, 1 to 2500 )
    //val l1 = List(1 to 3, 1 to 3, 1 to 3)
    //val c5 = GridPramaterSearch.cartesian3( l1, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    //val c5 = GridPramaterSearch.cartesian4(l1.toStream, List(), { x: Seq[Int] => x.mkString("<", ",", ">")  } )
    def c5 = GridPramaterSearch.cartesian4( l1.toStream, List(), { x: Seq[ Int ] => x.mkString( "<", ",", ">" ) } )
    c5.foreach { println }
    
  }
}
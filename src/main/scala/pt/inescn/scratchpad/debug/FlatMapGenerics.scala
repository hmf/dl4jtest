package pt.inescn.scratchpad.debug

object FlatMapGenerics {
  import scala.language.implicitConversions

  object Test {
    /* Ok
    implicit def duplicate0[ X ]( hc: Stream[ X ] ): Stream[ _ ] = {
      val crss = hc.flatMap { x: X => List( x, x ) }
      crss
    }*/

    /*
    // Ok... but
    // Need to add X to Stream[_] output
    // If we add another function with the same signature, we get an error
    // The implicitis interact !! They can be chained
    implicit def duplicate1[ X, H[ _ ] ]( hc: H[ X ] ): Stream[ X ] = {
      val crss = hc.flatMap { x: X => List( x, x ) }
      crss
    }*/

    /*
    implicit def duplicate1[ X, H[ _ ] <: scala.collection.AbstractSeq[X] ]( hc: H[ X ] ): Stream[ X ] = {
      val crss = hc.toStream.flatMap { x: X => List( x, x ) }
      crss
    }*/
    
    import scala.language.higherKinds
    /*
    implicit def duplicate1[ X, H[ _ ] <: scala.collection.AbstractSeq[X] ]( hc: H[ X ] ): H[ X ] = {
      val crss = hc.flatMap { x: X => List( x, x ) }
      crss
    }*/
    /*
    implicit def duplicate1[ X, H[ _ ] <: List[X] ]( hc: H[ X ] ): H[ X ] = {
      val crss = hc.flatMap { x: X => List( x, x ) }
      crss
    }*/

    /*
    // If we ensure that H[X] = Stream[X] why do we get:
    // type mismatch in return:  crss.type (with underlying type scala.collection.immutable.Stream[X]) not H[X]
    import scala.language.higherKinds
    implicit def duplicate2[ X, H[ _ ] ]( hc: H[ X ] )(implicit ev: H[X] =:= Stream[X]): H[ X ] = {
      val crss = hc.flatMap { x: X => List( x, x ) }
      crss
    }*/
    
   /*
    import scala.language.higherKinds
    // Must use >:, if <: then error: overloaded method value flatMap with alternatives
    //implicit def duplicate3[ X, H[ _ ] >: Seq[ _ ] ]( hc: H[ X ] ) : Seq[X]= {
    implicit def duplicate3[ X, H[ _ ] >: Stream[ _ ] ]( hc: H[ X ] ) : Seq[X]= {
    //implicit def duplicate3[ X, H[ _ ] >: Stream[ _ ] ]( hc: H[ X ] ) : Stream[X]= {
    //implicit def duplicate3[ X, H[ _ ] <: Stream[ _ ] ]( hc: H[ X ] ) : Stream[X]= {
      val crss = hc.flatMap { x: X => List(x,x) }
      crss
    }*/

   
    /*
    import scala.language.higherKinds
    // Must use >:, if <: then error: overloaded method value flatMap with alternatives
    //implicit def duplicate3[ X, H[ _ ] >: Stream[ _ ] ]( hc: H[ X ] ) : Stream[X] = {
    implicit def duplicate3[ X, H[ _ ] >: Stream[ _ ] ]( hc: H[ X ] ) : Int = {
    //implicit def duplicate3[ X, H[ _ ] <: Stream[ _ ] ]( hc: H[ X ] ) : Int = {
      def doMore(h : H[X]) : Stream[X] = {
        val crss = h.flatMap { x: X => List(x,x) }
        crss
      }
      doMore(hc)
      1
    }*/
    
    /*
    //implicit def duplicate3[ X, H[ _ ] >: Stream[ _ ] ]( hc: H[ X ] ) : Int = {
    implicit def duplicate3[ X, H[ _ ] >: Stream[ _ ] ]( hc: H[ X ] ) : Int = {
      def doMore[Y, G[_] >: Stream[_] ](g : G[Y]) : Stream[Y] = {
        val crss = g.flatMap { x: X => List(x,x) }
        crss
      }
      doMore(hc)
      1
    }*/
    
    /*
    // overloaded method value flatMap with alternatives
    import scala.language.higherKinds
    implicit def duplicate[ X, H[ _ ] <: Stream[ _ ] ]( hc: H[ X ] ) : Stream[X]= {
      val crss = hc.flatMap { x: X => List(x,x) }
      crss
    }*/
    
    
  }

}
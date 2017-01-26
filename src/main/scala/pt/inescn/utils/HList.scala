package pt.inescn.utils

import scala.language.higherKinds

// https://xuwei-k.github.io/shapeless-sxr/shapeless-2.10-2.0.0-M1/shapeless/ops/coproduct.scala.html
//trait Mapper0[F <: Poly, C <: Coproduct] extends DepFn1[C] { type Out <: Coproduct }

// 
sealed trait HList {
  type Prepend[ A ] <: HList
  type Append[ L <: HList ] <: HList
  type Mapped <: HList

  def ::[ U ]( v: U ): Prepend[ U ]
  def prepend[ A ]( a: A ): Prepend[ A ]
  def append[ L <: HList ]( l: L ): Append[ L ]

  def map( p: Poly ): Mapped
}

/**
 * @see https://github.com/dragisak/type-level/blob/master/src/main/scala/com/dragisak/typelevel/HList.scala
 * @see https://apocalisp.wordpress.com/2010/06/08/type-level-programming-in-scala/
 * @see https://apocalisp.wordpress.com/2010/07/06/type-level-programming-in-scala-part-6a-heterogeneous-list%C2%A0basics/
 * @see https://github.com/runarorama
 * @see http://blog.higher-order.com/ (Rúnar Bjarnason)
 * @see http://pchiusano.github.io/ (Paul Chiusano)
 *
 * Mapping
 * @see https://github.com/milessabin/shapeless/issues/73
 * @see http://www.hyperlambda.com/posts/hlist-map-in-scala/
 * @see https://xuwei-k.github.io/shapeless-sxr/shapeless-2.10-2.0.0-M1/polyntraits.scala.html
 *
 * Typelevel
 * @see http://gigiigig.github.io/tlp-step-by-step/introduction.htmls
 * @see https://github.com/1ambda/scala/tree/master/type-level-programming/src
 * @see http://typelevel.org/blog/
 * @see https://www.scala-exercises.org/
 * @see http://slick.lightbend.com/talks/scalaio2014/Type-Level_Computations.pdf
 *
 * https://skillsmatter.com/explore?q=tag%3Atypelevel
 *
 *  https://github.com/milessabin/shapeless/tree/master/examples/src/main/scala/shapeless/examples
 *
 *  HList
 *  https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/syntax/hlists.scala
 * https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/hlists.scala
 *    /**
 * Maps a higher rank function across this `HList`.
 * */
 * def map(f : Poly)(implicit mapper : Mapper[f.type, L]) : mapper.Out = mapper(l)
 *
 * /**
 * Flatmaps a higher rank function across this `HList`.
 * */
 * def flatMap(f : Poly)(implicit mapper : FlatMapper[f.type, L]) : mapper.Out = mapper(l)
 *
 * Mapper
 * https://github.com/milessabin/shapeless/blob/fcc15a6897ad9aad10e19d0b2432ccf0a9630515/core/src/main/scala/shapeless/ops/hlists.scala
 */

final case class HCons[ H, T <: HList ]( head: H, tail: T ) extends HList {

  type Prepend[ A ] = HCons[ A, HCons[ H, T ] ]
  type Append[ L <: HList ] = HCons[ H, T#Append[ L ] ]
  type Mapped = HCons[ H, T#Mapped ]

  def ::[ U ]( v: U ): Prepend[ U ] = HCons( v, this )

  def prepend[ A ]( a: A ): Prepend[ A ] = HCons( a, this )
  def append[ L <: HList ]( l: L ): Append[ L ] = HCons( head, tail.append( l ) )
  //def map(p: Poly)  = HCons( p(head), tail.map( p ) )
  def map( p: Poly ): Mapped = HCons( head, tail.map( p ) )
}

//sealed class HNil extends HList {
sealed trait HNil extends HList
//object HNil extends HNil {
case object HNil extends HNil {
  type Prepend[ A ] = HCons[ A, HNil.type ]
  type Append[ L <: HList ] = L
  type Mapped = this.type

  def ::[ T ]( v: T ): Prepend[ T ] = HCons( v, this ) // No need to type explicitly, same as prepend

  def prepend[ A ]( a: A ): Prepend[ A ] = HCons( a, this )
  def append[ L <: HList ]( l: L ): Append[ L ] = l
  def map( p: Poly ): Mapped = this
}

// aliases for building HList types and for pattern matching
object HList {
  type ::[ H, T <: HList ] = HCons[ H, T ]

  val :: = HCons // alias for pattern matching

  // if we use this instead of an explicit object (see below) we have to `import pt.inescn.utils.HList.HNil`
  // to this this embedded object as well as `import pt.inescn.utils.HNil`to get the type (class). 
  // The `import HList.HNil` also causes the  IDE to complain of ambiguous reference being  imported twice
  //val HNil = new HNil 
  //type HNil = HNil.type

  //def map(f : Poly)(implicit mapper : Mapper0[f.type, L]) : mapper.Out = mapper(l)
  //def map[S, B, T <:HList](hc: T, f: S=>B)(implicit ev: Mapper0[S, T, B]) = ev(hc, f)
  //def map[P <: Poly, T <:HList](p: P, hc: T)(implicit ev: Mapper[P, T]) = ev(hc)
  // TODO: place as member of HList?
  def map[ P <: Poly, H <: HList ]( p: P, hc: H )( implicit ev: Mapper[ P, H ] ) = ev( hc )
}

import scala.language.implicitConversions

// TODO: move this into HList Object?
sealed trait Mapper[ P, HL <: HList ] {
  type Out <: HList
  def apply( hl: HL ): Out
}

// (cse: pt.inescn.utils.Case[p.type,H]) cse.Result
object Mapper {
  implicit def cellMapper[ P <: Poly, H, T <: HList ]( implicit cse: pt.inescn.utils.Case[ P, H ], evTail: Mapper[ P, T ] ): Mapper[ P, HCons[ H, T ] ] =
    new Mapper[ P, HCons[ H, T ] ] {
      type Out = HCons[ cse.Result, evTail.Out ]
      def apply( hc: HCons[ H, T ] ) = {
        println( s"apply(Cons(${hc.head},${hc.tail})) = ${cse( hc.head )}" )
        HCons( cse( hc.head ), evTail( hc.tail ) )
      }
    }

  implicit def nilMapper[ S, HC <: HNil ]: Mapper[ S, HC ] =
    new Mapper[ S, HC ] {
      type Out = HNil
      def apply( hc: HC ) = {
        println( "end." )
        HNil
      }
    }
}

// If we use the sealed class type
//object HNil extends HNil
sealed trait EMapper[ P, HL <: HList, ACC] {
  type Out //<: HList
  def apply( hl: HL, acc: ACC ): Out
}

/*
  def combineHList_0[A, B, C]( l1 : List[A], l2: List[B], l3: List[C])  = {
    l1.flatMap { x1 => l2.flatMap { x2 => l3.map { x3 => HCons(x1, HCons(x2, HCons(x3, HNil))) } } }
  }
 */
/*
object EMapper {
  //implicit def cellMapper[ X, P <: Poly, H <: scala.collection.Traversable[X], T <: HList ]
//  implicit def cellMapper[ X, A, P <: PolyC, H[ _ ] <: scala.collection.immutable.List[ _ ], T <: HList ]
//  ( implicit cse: pt.inescn.utils.CaseC[ P, X, H ], evTail: EMapper[ P, T, A ] ): EMapper[ P, HCons[ H[ X ], T ], A ] =
  implicit def cellMapper[ X, P <: PolyC, H[_] <: Iterable[_], T <: HList, ACC ]
  ( implicit evTail: EMapper[ P, T, ACC ] ) /*: EMapper[ P, HCons[ H[ X ], T ], X ]*/ =
    new EMapper[ P, HCons[ H[ X ], T ], ACC ] {
      //type Out = HCons[ cse.Result, evTail.Out ]
      type Out 
      def apply( hc: HCons[ H[ X ], T ], acc: ACC ) = {
        //println( s"??? apply(Cons(${hc.head},${hc.tail})) = ${cse( hc.head )}" )
        println( s"??? apply(Cons(${hc.head},${hc.tail}))" )
        val tmp = HCons(hc.head,acc)
        //def flatMap(acc : ACC) = hc.head.flatMap { x : X  => evTail(hc.tail, HCons(x,acc)) }
        //val nacc = HCons( hc.head, acc)
        //val tail = evTail( hc.tail, nacc )
        //val hd = cse( hc.head )
        //HCons( hd, tail )
        //HCons( hc.head, tail )
        null
      }
    }

  implicit def nilMapper[ S, HC <: HNil, A ]: EMapper[ S, HC, A ] =
    new EMapper[ S, HC, A ] {
      type Out = A
      def apply( hc: HC, acc: A ) = {
        println( "??? end." )
        acc
      }
    }
}
*/
// sbt "run-main pt.inescn.utils.HListExample"
object HListExample {
  import HList._

  def y = HList
  val z = HList
  type w = String :: Boolean :: Double :: HNil

  // construct an HList similar to the Tuple3 ("str", true, 1.0)
  val x = "str" :: true :: 1.0 :: HNil

  // get the components by calling head/tail
  val s: String = x.head
  val b: Boolean = x.tail.head
  val d: Double = x.tail.tail.head
  // compile error
  //val e = x.tail.tail.tail.head

  // or, decompose with a pattern match

  val f: ( String :: Boolean :: Double :: HNil ) => String = {
    case "s" :: false :: _        => "test"
    case h :: true :: 1.0 :: HNil => h
    // compilation error because of individual type mismatches and length mismatch
    //case 3 :: "i" :: HNil => "invalid"
    case _                        => "unknown"
  }

  val f1: ( w ) => String = {
    case "s" :: false :: _        => "test"
    case h :: true :: 1.0 :: HNil => h
    // compilation error because of individual type mismatches and length mismatch
    //case 3 :: "i" :: HNil => "invalid"
    case _                        => "unknown"
  }

  /*
  def cartesian7[ A, B ]( list: Stream[ Stream[ A ] ], acc: Seq[ HList ], f: Seq[ HList ] => B ): Stream[ B ] = {
    list match {
      case Stream.Empty => List( f( acc ) ).toStream
      case h #:: t      => h.flatMap { x => cartesian7( t, x +: acc, f ) }
    }
  }*/

  import scala.language.implicitConversions

  /* TODO
  // http://stackoverflow.com/questions/5598085/where-does-scala-look-for-implicits/5598107#5598107
  // http://stackoverflow.com/questions/20616525/scala-type-override
  //  Iterable[+A] : map and flatMap
  sealed trait Mapper[ A, T[ A ], B ] {
    //type Out <: T[B]
    type Out[ X ] //= T[X]
    def map( l: T[ A ], f: A => B ): Out[ B ]
  }

  object Mappers {

    implicit def typedMapper[ A, T[ A ] <: Iterable[ A ], B ]: Mapper[ A, T, B ] =
      new Mapper[ A, T, B ] {
        override type Out[ X ] = Iterable[ X ]
        //override type Out <: Iterable[ B ]
        //def map( l: T[ A ], f: A => B ) : this.Out = {
        def map( l: T[ A ], f: A => B ): Out[ B ] = {
          println( "map" )
          l.map( f )
        }
      }

    implicit def IntMapper = typedMapper[ Int, List, Int ]
  }

  //def testMapper[ A, T[ A ], B ]( l: T[ A ], f: A => B )( implicit mapper: Mapper[ A, T, B ] ): T[B] = {
  def testMapper[ A, T[ A ], B ]( l: T[ A ], f: A => B )( implicit mapper: Mapper[ A, T, B ] ): Mapper[ A, T, B ]#Out[ B ] = {
    println( mapper )
    mapper.map( l, f )
  }

  ////////////////////////////////////////
  //trait Mapper1[ A, T[ _ ], B ] {
  trait Mapper1[ A, T[ A ], B ] {
    def map( ta: T[ A ] )( f: A => B ): T[ B ]
    def mapx( ta: T[ A ], f: A => B ): T[ B ]
  }

  // Note that Iterable[A]#map has type [B](A => B)Iterable[B]. You can't have typedMapper
  // like yours from above just yet, because T#map is not certain to return another T;
  // it only promises an Iterable.
  implicit def iterableMapper[ A, B ]: Mapper1[ A, Iterable, B ] = new Mapper1[ A, Iterable, B ] {
    // Multiple param lists support the type inferencer
    //override def map( i: Iterable[ A ] )( f: A => B ) = i.map( f )
    override def map( i: Iterable[ A ] )( f: A => B ): Iterable[ B ] = i.map( f )
    //override def map( i: Iterable[ A ] )( f: A => B ) : T[ B ] = i.map( f ) // Fails
    override def mapx( i: Iterable[ A ], f: A => B ): Iterable[ B ] = i.map( f )
  }

  // Curried and arg-swapped version of Mapper
  type MapperOf[ A, B ] = { type l[ T[ _ ] ] = Mapper1[ A, T, B ] }
  def map1[ A, B, T[ _ ]: MapperOf[ A, B ]#l ]( ta: T[ A ] )( f: A => B ): T[ B ] = implicitly[ Mapper1[ A, T, B ] ].map( ta )( f )
  def map2[ A, B, T[ _ ]: MapperOf[ A, B ]#l ]( ta: T[ A ], f: A => B ): T[ B ] = implicitly[ Mapper1[ A, T, B ] ].map( ta )( f )
  def map3[ A, B, T[ _ ]: MapperOf[ A, B ]#l ]( ta: T[ A ], f: A => B ): T[ B ] = implicitly[ Mapper1[ A, T, B ] ].mapx( ta, f )

  //def map4[ A, B, T[_] ]( ta: T[ A ] , f: A => B ): T[ B ] = implicitly[ Mapper1[ A, T, B ] ].map( ta ) ( f )
  val t1 = map1( List( -1, -2, -3 ): Iterable[ Any ] )( _.toString )
  println( t1 )
  //map1( List( 1, 2, 3 ) )( _ * 2 ) // NOPE! The inferencer has already decided T = List, before
  // looking for implicits, so the resolution fails to notice that
  // iterableMapper would work.
  val t3 = map1[ Int, Int, Iterable ]( List( 1, 2, 3 ) )( _ * 2 ) // Works  

  val t4 = map2( List( -1, -2, -3 ): Iterable[ Any ], { x: Any => x.toString } )
  println( t4 )
  val t5 = map2( List( -1, -2, -3 ): Iterable[ Any ], { x: Any => x.toString } )
  println( t5 )

  ///////////////
  def genCartesianL[ A, T[ A ], B ]( list: HList, acc: HList )( implicit mapper: Mapper[ A, T, B ] ): HList = {
    list match {
      case HNil => acc
      case h :: t =>
        println( h )
        //val t = mapper.map( h, x => x )
        genCartesian( t, h :: acc )
    }
  }
* */

  def genCartesian[ A, B ]( list: HList, acc: HList ): HList = {
    list match {
      case HNil => acc
      case h :: t =>
        println( h )
        genCartesian( t, h :: acc )
    }
  }

  /* TODO
    def doF[ A, B ]( list: HList, acc: HList, f : A => B ): HList = {
    list match {
      case HNil => acc
      case h :: t =>
        println( h )
        val nh = f(h)
        genCartesian( t, nh :: acc )
    }
  }
*/

  // Traversable
  object myPoly extends Poly {
    implicit def intCase = {
      new Case[ this.type, Int ] {
        type Result = Double
        def apply( num: Int ): Double = num / 2.0
      }
    }
    implicit def stringCase = {
      new Case[ this.type, String ] {
        type Result = Int
        def apply( str: String ): Int = str.length
      }
    }
    implicit def doubleCase = {
      new Case[ this.type, Double ] {
        type Result = Int
        def apply( num: Double ): Int = num.toInt
      }
    }
    implicit def listIntCase = {
      new Case[ this.type, List[ Int ] ] {
        type Result = Int
        def apply( lst: List[ Int ] ): Int = lst.length
      }
    }
    implicit def listDoubleCase = {
      new Case[ this.type, List[ Double ] ] {
        type Result = Int
        def apply( lst: List[ Double ] ): Int = lst.length
      }
    }
    /*
    implicit def listCase[ X ] = {
      new Case[ this.type, List[ X ] ] {
        type Result = Int
        def apply( lst: List[ X ] ): Int = lst.length
      }
    }*/
  }

  /* TODO
  def genCartesianM[P <: Poly,H <: HList]( p: P, list: H, acc: H )( implicit cse: pt.inescn.utils.Case[ P, H ]): H = {
    list match {
      case HNil => acc
      case h :: t =>
        println( h )
        //val nh = p.apply( h )
        genCartesianM( p, t, h :: acc )
    }
  } */

  def combineHList[ A ]( l1: List[ A ], l2: List[ A ], l3: List[ A ] ): List[ List[ A ] ] = {
    l1.flatMap { x1 => l2.flatMap { x2 => l3.map { x3 => List( x1, x2, x3 ) } } }
  }

  def combineHList_0[ A, B, C ]( l1: List[ A ], l2: List[ B ], l3: List[ C ] ) = {
    l1.flatMap { x1 => l2.flatMap { x2 => l3.map { x3 => HCons( x1, HCons( x2, HCons( x3, HNil ) ) ) } } }
  }

  def combineHList_1[ A, B, C ]( l1: List[ A ], l2: List[ B ], l3: List[ C ] ) = {
    def hlist( x1: A, x2: B, x3: C ) = HCons( x1, HCons( x2, HCons( x3, HNil ) ) )

    type R = HCons[ A, HCons[ B, HCons[ C, HNil.type ] ] ]
    def map( l: List[ C ], x: A, y: B, m: ( A, B, C ) => R ) = {
      l.map { z => m( x, y, z ) }
    }

    type MP = ( List[ C ], A, B, ( A, B, C ) => R ) => List[ R ]
    def flatMap2( l: List[ B ], nl: List[ C ], x: A, m: MP ) = {
      //l.flatMap { y => map(l3, x, y, hlist) }
      l.flatMap { y => m( nl, x, y, hlist ) }
    }

    type MQ = ( List[ B ], List[ C ], A, MP ) => List[ R ]
    def flatMap3( l: List[ A ], nl: List[ B ], nnl: List[ C ], m: MQ ) = {
      l.flatMap { y => m( nl, nnl, y, map ) }
    }
    //l1.flatMap { x1 => l2.flatMap { x2 => l3.map { x3 =>  hlist(x1, x2, x3) } } }
    //l1.flatMap { x1 => l2.flatMap { x2 => map(l3, x1, x2, hlist) }  }
    //l1.flatMap { x1 => flatMap2(l2, l3, x1, map) }
    flatMap3( l1, l2, l3, flatMap2 )
  }

  def combineHList_2[ A, B, C ]( l1: List[ A ], l2: List[ B ], l3: List[ C ] ) = {

    type R = HCons[ A, HCons[ B, HCons[ C, HNil.type ] ] ]
    type MR = ( A, B, C ) => R
    def hlist( x1: A, x2: B, x3: C ) = HCons( x1, HCons( x2, HCons( x3, HNil ) ) )

    type MP = ( List[ C ], A, B, ( A, B, C ) => R ) => List[ R ]
    def map( l: List[ C ], x: A, y: B, m: MR ) = {
      l.map { z => m( x, y, z ) }
    }

    type MQ = ( List[ B ], List[ C ], A, MP, MR ) => List[ R ]
    def flatMap2( l: List[ B ], nl: List[ C ], x: A, m: MP, m0: MR ) = {
      //l.flatMap { y => map(l3, x, y, hlist) }
      l.flatMap { y => m( nl, x, y, m0 ) }
    }

    type MT = ( List[ A ], List[ B ], List[ C ], MQ, MP, MR ) => List[ R ]
    def flatMap3( l: List[ A ], nl: List[ B ], nnl: List[ C ], m: MQ, m0: MP, m1: MR ) = {
      l.flatMap { y => m( nl, nnl, y, m0, m1 ) }
    }
    //l1.flatMap { x1 => l2.flatMap { x2 => l3.map { x3 =>  hlist(x1, x2, x3) } } }
    //l1.flatMap { x1 => l2.flatMap { x2 => map(l3, x1, x2, hlist) }  }
    //l1.flatMap { x1 => flatMap2(l2, l3, x1, map) }
    flatMap3( l1, l2, l3, flatMap2, map, hlist )
  }

  
  def combineHList_3[ A, B, C ]( l1: List[ A ], l2: List[ B ], l3: List[ C ] ) = {
    type ACC2  = HCons[ B, HCons[ A, HNil.type ] ] 
    type ACC3 = HCons[ C, HCons[ B, HCons[ A, HNil.type ] ] ]
    
    //type R = HCons[ A, HCons[ B, HCons[ C, HNil.type ] ] ]
    type R = HCons[ C, HCons[ B, HCons[ A, HNil.type ] ] ]
    //type MR = ( A, B, C ) => R
    //def hlist( x1: A, x2: B, x3: C ) = HCons( x1, HCons( x2, HCons( x3, HNil ) ) )
    //def hlist( x1: A, x2: B, x3: C ) = HCons( x3, HCons( x2, HCons( x1, HNil ) ) )

    //type MP = ( List[ C ], ACC2, ( A, B, C ) => R ) => List[ R ]
    type MP = ( List[ C ], ACC2) => List[ R ]
    def map( l: List[ C ], acc: ACC2) = {
      //l.map { z => m( x, y, z ) }
      l.map { z => HCons(z, acc ) }
    }

    type ACC1  = HCons[ A, HNil.type ]
    //type MQ = ( List[ B ], List[ C ], ACC1, MP, MR ) => List[ R ]
    type MQ = ( List[ B ], List[ C ], ACC1, MP) => List[ R ]
    def flatMap2( l: List[ B ], nl: List[ C ], acc: ACC1, m: MP) = {
      //l.flatMap { y => map(l3, x, y, hlist) }
      l.flatMap { y => m( nl, HCons(y, acc)) }
    }

    type ACC0  = HNil.type
    type MT = ( List[ A ], List[ B ], List[ C ], ACC0, MQ, MP ) => List[ R ]
    //def flatMap3( l: List[ A ], nl: List[ B ], nnl: List[ C ], acc: ACC0, m: MQ, m0: MP, m1: MR ) = {
    def flatMap3( l: List[ A ], nl: List[ B ], nnl: List[ C ], acc: ACC0, m: MQ, m0: MP) = {
      l.flatMap { y => m( nl, nnl, HCons(y,acc), m0) }
    }
    //l1.flatMap { x1 => l2.flatMap { x2 => l3.map { x3 =>  hlist(x1, x2, x3) } } }
    //l1.flatMap { x1 => l2.flatMap { x2 => map(l3, x1, x2, hlist) }  }
    //l1.flatMap { x1 => flatMap2(l2, l3, x1, map) }
    //flatMap3( l1, l2, l3, HNil, flatMap2, map, hlist )
    flatMap3( l1, l2, l3, HNil, flatMap2, map)
  }

  
  
  def combineHList_4[ A, B, C ]( l1: List[ A ], l2: List[ B ], l3: List[ C ] ) = {
    type ACC2  = HCons[ B, HCons[ A, HNil.type ] ] 
    type ACC3 = HCons[ C, HCons[ B, HCons[ A, HNil.type ] ] ]
    
    type R = HCons[ C, HCons[ B, HCons[ A, HNil.type ] ] ]

    type MP = ( List[ C ], ACC2) => List[ R ]
    def map( l: List[ C ], acc: ACC2) = {
      l.map { z => HCons(z, acc ) }
    }

    type ACC1  = HCons[ A, HNil.type ]
    type MQ = ( List[ B ], List[ C ], ACC1, MP) => List[ R ]
    def flatMap2( l: List[ B ], nl: List[ C ], acc: ACC1, m: MP) = {
      l.flatMap { y => m( nl, HCons(y, acc)) }
    }

    type ACC0  = HNil.type
    type MT = ( List[ A ], List[ B ], List[ C ], ACC0, MQ, MP ) => List[ R ]
    def flatMap3( l: List[ A ], nl: List[ B ], nnl: List[ C ], acc: ACC0, m: MQ, m0: MP) = {
      l.flatMap { y => m( nl, nnl, HCons(y,acc), m0) }
    }
    flatMap3( l1, l2, l3, HNil, flatMap2, map)
  }

  // map[ P <: Poly, H <: HList ]( p: P, hc: H )( implicit ev: Mapper[ P, H ] ) = ev( hc )
  //def combineHList_1( list: HList ): HList = {
  def combineHList_1[ P <: PolyC, H <: HList ]( p: P, list: H )( implicit ev: EMapper[ P, H, Int ] ) = {
    ev( list, 0 )
  }

  // ***************************************************

  // sbt "run-main pt.inescn.utils.HListExample"
  def main( args: Array[ String ] ) {
    val l1 = "One" :: 2 :: HNil
    val l2 = "Three" :: 4.0 :: HNil
    val l3 = l1.append( l2 )
    println( l3 )
    // Check the values
    val "One" :: 2 :: "Three" :: 4.0 :: HNil = l3
    // Check the types
    val checkl: HCons[ String, HCons[ Int, HCons[ String, HCons[ Double, HNil.type ] ] ] ] = l3

    val l4 = "Zero" :: l1
    val l5 = l1.prepend( "Zero" )
    val "Zero" :: "One" :: 2 :: HNil = l4
    val "Zero" :: "One" :: 2 :: HNil = l5

    import Mapper._
    //val v0 = map(myPoly, HNil)(nilMapper)
    val v0 = map( myPoly, HNil )
    //val v1 = map(myPoly, l1)(cellMapper)
    val v1 = map( myPoly, l1 )
    println( v1 )
    val l6 = HNil
    val l7 = l6.append( "Null" :: HNil )
    val v1a = map( myPoly, l7 )
    val v2 = map( myPoly, l3 )
    println( v2 )
    val v3 = map( myPoly, l5 )
    println( v3 )

    println( "Combines:____________________" )

    object myPolyC extends PolyC {
        implicit def listIntCaseC = {
          new CaseC[ this.type, Int, List ] {
            type Result = Int
            def apply( lst: List[ Int ] ): Result = lst.length
          }
        }
        implicit def listDoubleCaseC = {
          new CaseC[ this.type, Double, List ] {
            type Result = Int
            def apply( lst: List[ Double ]): Result = lst.length
          }
        }
    }

    def callPoly_0[ T, C[ _ ], ACC ]( a: C[ T ], p: PolyC )( implicit cse1: CaseC[ p.type, T, C] ) = {
      val v1 = p( a )
      v1
    }

    val p8 = callPoly_0( List( 1, 2 ), myPolyC )

    val l8 = List( 1, 2 ) :: List( 3.0, 4.0 ) :: HNil
    //val r2 = combineHList_1( myPolyC, l8 )
    //println( s"Combine 1: $r2" )

    /*
    genCartesian( l3, HNil )
    genCartesian( HNil, HNil )

    val l9 = List( 1, 2 ) :: List( 3.0, 4.0 ) :: HNil
    val l10 = genCartesian( l9, HNil )
    println( l10 )
    */

    /*
    import Mappers.IntMapper
    val m1: pt.inescn.utils.HListExample.Mapper[ Int, List, Int ] = IntMapper
    val l10 = testMapper( List( 1, 2, 3 ), { x: Int => x + 1 } )( IntMapper )
    val l11 = testMapper( List( 1, 2, 3 ), { x: Int => x + 1 } )
    println( l11 )

    val l12 = genCartesianL( l9, HNil )
    println( l12 )
    */
    //val p3 = 123.0 :: "456" :: HNil
    //val p4 = genCartesianM( myPoly, p3, HNil )
    //println( "p4 = " + p4 )
    /*val p5 = p3.map( myPoly )
    println( "p5 = " + p5 )
    val v1 = p5.head
    val v2 = p5.tail.head
    */
  }

}

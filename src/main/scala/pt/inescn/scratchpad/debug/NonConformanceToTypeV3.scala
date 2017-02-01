package pt.inescn.scratchpad.debug

/**
 * sbt "run-main pt.inescn.scratchpad.debug.NonConformanceToTypeV3"
 */
object NonConformanceToTypeV3 {

  trait Parameter[ T ] {
    type Self <: Parameter[ T ]
    def apply( v: T ): Self
  }

  import scala.language.higherKinds

  // https://groups.google.com/forum/#!msg/scala-user/OjZMJeD7OXs/h_jBhmwnvaUJ
  // https://groups.google.com/forum/#!searchin/scala-user/conform$20to$20method$20apply$27s$20type$20parameter$20bounds%7Csort:relevance/scala-user/tDcwQTZt5H4/62Mfdy9zoZIJ
  //case class ParameterRange[ P[ X ] <: Parameter[ X ], T, U ](
  case class ParameterRange[ P <: Parameter[ T ]  with Parameter[ T ], T, U ](
      // val param: P with Parameter[ T ],  compile error
      val param: P, // If we place  with Parameter[ T ] we get 
      val start: T,
      val stop: T,
      val config: U ) {

    def toStream( f: ( T, T, U ) => Stream[ T ] ): Stream[ P#Self ] = {
      val st = f( start, stop, config )
      val r = st.map{ x: T => param( x ) }
      r
    }
  }
  
  case class Param1( v: Int ) extends Parameter[ Int ] { type Self = Param1; def apply( v: Int ) = new Param1( v ) }
  case class Param2( v: Char ) extends Parameter[ Char ] { type Self = Param2; def apply( v: Char ) = new Param2( v ) }
  case class Param3( v: List[ String ] ) extends Parameter[ List[ String ] ] { type Self = Param3; def apply( v: List[ String ] ) = new Param3( v ) }

  val p1 = Param1( 0 )                                                          // NonConformanceToTypeV2.Param1
  //val pr1: ParameterRange[ Parameter, Int, Int ] = ParameterRange( p1, 0, 10, 1 ) // NonConformanceToTypeV2.ParameterRange[NonConformanceToTypeV2.Parameter, Int, Int]
  val pr1 = ParameterRange( p1, 0, 10, 1 )                             // NonConformanceToTypeV2.ParameterRange[NonConformanceToTypeV2.Parameter, Int, Int]
  val p2 = Param2( '1' )                                                       // NonConformanceToTypeV2.Param2
  //val pr2: ParameterRange[ Parameter, Char, Int ] = ParameterRange( p2, '2', 'z', 1 )
  val pr2 = ParameterRange( p2, '2', 'z', 1 )                        // NonConformanceToTypeV2.ParameterRange[NonConformanceToTypeV2.Parameter, Char, Int]
  //val p3: Parameter[ List[ String ] ] = Param3( List( "" ) )  // NonConformanceToTypeV2.Parameter[List[String]]
  val p3 = Param3( List( "" ) )                                              // NonConformanceToTypeV2.Parameter[List[String]]
  //val pr3: ParameterRange[ Parameter, List[ String ], Int ] = ParameterRange( p3, List( "a", "b" ), List[ String ](), 2 )
  val pr3 = ParameterRange( p3, List( "a", "b" ), List[ String ](), 2 )  // NonConformanceToTypeV2.ParameterRange[NonConformanceToTypeV2.Parameter,  List[String], Int]

  // Example of using an existential type
  // The line with v1 fails to compile when the commented version of the ListWrapper is used 
  //case class ListWrapper[ L <: List[ T ], T ]( list: L )
  case class ListWrapper[ L[ X ] <: List[ X ], T ]( list: L[ T ] )
  val v1 = ListWrapper( List( 1, 2, 3 ) ) // Test1.ListWrapper[List, Int]

  // Alternative method
  case class ListWrapper1[ L <: List[ T ], T ]( list: L with List[ T ] )
  val v2 = ListWrapper1( List( 1, 2, 3 ) ) //  Test1.ListWrapper1[List[Int], Int]

  def typeOf[ T: Manifest ]( t: T ): Manifest[ T ] = manifest[ T ]

  def main( args: Array[ String ] ) {
    //println( typeOf( v1 ) )  // won't print
    println( typeOf( v2 ) )
  }
}
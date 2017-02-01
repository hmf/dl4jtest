package pt.inescn.scratchpad.debug

/**
 * sbt "run-main pt.inescn.scratchpad.debug.NonConformanceToType"
 */
object NonConformanceToType {

  trait Parameter[ T ] {
    type Self <: Parameter[ T ]
    def apply( v: T ): Self
  }

  import scala.language.higherKinds

  // https://groups.google.com/forum/#!msg/scala-user/OjZMJeD7OXs/h_jBhmwnvaUJ
  // https://groups.google.com/forum/#!searchin/scala-user/conform$20to$20method$20apply$27s$20type$20parameter$20bounds%7Csort:relevance/scala-user/tDcwQTZt5H4/62Mfdy9zoZIJ
  case class ParameterRange[ P <: Parameter[ T ], T, U ](
      val param: P,
      val start: T,
      val stop: T,
      val config: U ){

    def toStream( f: ( T, T, U ) => Stream[ T ] ): Stream[ P#Self ] = {
      val st = f( start, stop, config )
      val r = st.map{ x: T => param( x ) }
      r
    }
  }
  
  case class Param1( v: Int = -1 ) extends Parameter[ Int ] { type Self = Param1; def apply( v: Int ) = Param1( v ) }
  case class Param2( v: Char = ' ') extends Parameter[ Char ] { type Self = Param2; def apply( v: Char ) = Param2( v ) }
  case class Param3( v: String = "") extends Parameter[ String ] { type Self = Param3; def apply( v: String ) = Param3( v ) }

  //  NonConformanceToType.ParameterRange[NonConformanceToType.Parameter[Int], Int, Int]
  val pr1 = ParameterRange( Param1(), 0, 10, 1 )
  // NonConformanceToType.ParameterRange[NonConformanceToType.Parameter[Char], Char, Int]
  val pr2 = ParameterRange( Param2(), '2', 'z', 1 )
  // NonConformanceToType.ParameterRange[NonConformanceToType.Parameter[List[String]], List[String], Int]
  val pr3 = ParameterRange( Param3(), "", "", List("a", "b") )

  //case class ListWrapper[ L <: List[ T ], T ]( list: L )
  case class ListWrapper[ L[ X ] <: List[ X ], T ]( list: L[ T ] )
  val v1 = ListWrapper( List( 1, 2, 3 ) ) // Test1.ListWrapper[List, Int]

  case class ListWrapper1[ L <: List[ T ], T ]( list: L with List[ T ] )
  val v2 = ListWrapper1( List( 1, 2, 3 ) ) //  Test1.ListWrapper1[List[Int], Int]
  
  def main( args: Array[ String ] ) {
    def typeOf[T: Manifest](t: T): Manifest[T] = manifest[T]
    val v1 = typeOf(v2)
    println(v1)
    
    val s1 = pr1.toStream { (from, to, by) =>  
                                            val len = ( ( to - from ) / by ).ceil.toInt
                                             Stream.iterate( from ) { acc => acc + by } take len } 
    println(s1.mkString("<", ",", ">"))
    val s3 = pr3.toStream { (from, to, by) =>  by.toStream } 
    println(s3.mkString("<", ",", ">"))
      
  }
}
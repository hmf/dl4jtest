package pt.inescn.scratchpad.debug

// sbt "run-main pt.inescn.scratchpad.debug.HypeParamSearchV2"
object HypeParamSearchV2 {

  import pt.inescn.utils.HList._
  import pt.inescn.utils.HList
  import pt.inescn.utils.HCons
  import pt.inescn.utils.HNil

  
  trait Parameter[ T ] {
    type Self <: Parameter[ T ]
    def apply( v: T ): Self
  }
  
  import scala.language.higherKinds

  case class ParameterRange0[ P <: Parameter[ T ], T, U[_] ](
      val param: P,
      val seq: U[T]) {

    def toStream( f:  U[T]  => Stream[ T ] ): Stream[ P#Self ] = {
      val st = f( seq )
      val r = st.map{ x: T => param( x ) }
      r
    }
  }

  case class ParameterRange2[ P <: Parameter[ T ], T, U ](
      val param: P,
      val start: T,
      val config: U ){

    def toStream( f: ( T, U ) => Stream[ T ] ): Stream[ P#Self ] = {
      val st = f( start, config )
      val r = st.map{ x: T => param( x ) }
      r
    }
  }

  case class ParameterRange3[ P <: Parameter[ T ], T, U ](
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
  
  trait Model {
    type Params
    trait ParamsRange {
      type P
      def get: P
    }

    def getParamRanges: ParamsRange
    def params( p: Params ): Boolean = ???
    def fit: Boolean = ???
    def predict: Boolean = ???
  }

  class ModelA extends Model {

    case class Param1( v: Int = -1 ) extends Parameter[ Int ] { type Self = Param1; def apply( v: Int ) = new Param1( v ) }
    case class Param2( v: Double = -1) extends Parameter[ Double ] { type Self = Param2; def apply( v: Double ) = new Param2( v ) }
    case class Param3( v: String = "") extends Parameter[ String ] { type Self = Param3; def apply( v: String ) = new Param3( v ) }

    def getParamRanges = {
      val pr1  = ParameterRange3( Param1(), 0, 10, 1 )
      val pr2 = ParameterRange3( Param2(), 0.0, 1.0, 0.1 )
      val pr3  = ParameterRange0( Param3(), List( "a", "b", "c" ) )
      new ParamsRange {
        type P = ParameterRange3[ Param1, Int, Int ] :: 
                     ParameterRange3[ Param2, Double, Double ] :: 
                     ParameterRange0[ Param3, String, List ] :: 
                     HNil.type
        override def get: P = pr1 :: pr2 :: pr3 :: HNil
      }
    }
    type Params = Param1 :: Param2 :: Param3 :: HNil.type
    override def params( p: Params ): Boolean = true
    override def fit: Boolean = true
    override def predict: Boolean = true
  }
  
  
  class ModelB extends Model {

    case class Param1( v: Double = -1) extends Parameter[ Double ] { type Self = Param1; def apply( v: Double ) = new Param1( v ) }
    case class Param2( v: Int = -1) extends Parameter[ Int ] { type Self = Param2; def apply( v: Int ) = new Param2( v ) }
    case class Param3( v: String = "") extends Parameter[ String ] { type Self = Param3; def apply( v: String ) = new Param3( v ) }

    def getParamRanges = {
      val pr1 = new ParameterRange3( Param1(), 0.0, 1.0, 0.1 )
      val pr2 = new ParameterRange3( Param2(), 0, 10, 1 )
      val pr3  = ParameterRange0( Param3(), List( "d", "e", "f" ) )
      new ParamsRange {
        type P = ParameterRange3[ Param1, Double, Double ] :: 
                     ParameterRange3[ Param2, Int, Int ] :: 
                     ParameterRange0[ Param3, String, List ] :: 
                     HNil.type
        override def get: P = pr1 :: pr2 :: pr3 :: HNil
      }
    }
    type Params = Param1 :: Param2 :: Param3 :: HNil.type
    override def params( p: Params ): Boolean = true
    override def fit: Boolean = true
    override def predict: Boolean = true
  }

  def main( args: Array[ String ] ) {
    val m1 = new ModelA
    val param1_1 = new m1.Param1( 1 )
    val param1_2 = new m1.Param2( 1.0 )
    val param1_3 = new m1.Param3( "A" )
    val pr1a = param1_1 :: param1_2 :: param1_3 :: HNil
    val p1a = m1.params( pr1a )

    val m2 = new ModelB
    val param2_1 = new m2.Param1( 1.0 )
    val param2_2 = new m2.Param2( 1 )
    val param2_3 = new m2.Param3( "B" )
    //val p2be = m2.fit( pr1a ) // compilation will fail
    val pr2a = param2_1 :: param2_2 :: param2_3 :: HNil
    val p2b = m2.params( pr2a )

    // TODO: add indexing to the HList
    // TODO: add DSL bopilerplate - cross or parallel all streams
    val r1 = m1.getParamRanges
    val ps1 = r1.get
    val rng1 = ps1.head
    val rng2 = ps1.tail.head
    val rng3 = ps1.tail.tail.head
    
    val s1 = rng1.toStream { (from, to, by) =>  
                                            val len = ( ( to - from ) / by ).ceil.toInt
                                             Stream.iterate( from ) { acc => acc + by } take len } 
    println(s1.mkString("<", ",", ">"))
    val s2 = rng2.toStream { (from, to, by) =>  
                                            val len = ( ( to - from ) / by ).ceil.toInt
                                             Stream.iterate( from ) { acc => acc + by } take len } 
    println(s2.mkString("<", ",", ">"))
    val s3 = rng3.toStream { seq =>  seq.toStream } 
    println(s3.mkString("<", ",", ">"))

    val pr1_1_1 = s1(1)
    val pr1_2_1 = s2(1)
    val pr1_3_1 = s3(1)
    val pr1b = pr1_1_1 :: pr1_2_1 :: pr1_3_1 :: HNil
    val p1b = m1.params( pr1b )
    //val p2c = m2.params( pr1b ) // compile time failure
    
  }
}
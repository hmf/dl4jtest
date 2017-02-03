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

  // http://stackoverflow.com/questions/4315678/how-to-use-scalas-singleton-object-types
  // -uniqid -explaintypes
  // https://blogs.oracle.com/sundararajan/entry/mis_understaning_scala_s_singleton
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

  case class ParameterSeq[ P <: Parameter[ T ], T, U[_] ](
      val param: P,
      val seq: U[T]) {

    def toStream( f:  U[T]  => Stream[ T ] ): Stream[ P#Self ] = {
      val st = f( seq )
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
      val pr1  = ParameterRange( Param1(), 0, 10, 1 )
      val pr2 = ParameterRange( Param2(), 0.0, 1.0, 0.1 )
      val pr3  = ParameterSeq( Param3(), List( "a", "b", "c" ) )
      new ParamsRange {
        type P = ParameterRange[ Param1, Int, Int ] :: 
                     ParameterRange[ Param2, Double, Double ] :: 
                     ParameterSeq[ Param3, String, List ] :: 
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
      val pr1 = new ParameterRange( Param1(), 0.0, 1.0, 0.1 )
      val pr2 = new ParameterRange( Param2(), 0, 10, 1 )
      val pr3  = ParameterSeq( Param3(), List( "d", "e", "f" ) )
      new ParamsRange {
        type P = ParameterRange[ Param1, Double, Double ] :: 
                     ParameterRange[ Param2, Int, Int ] :: 
                     ParameterSeq[ Param3, String, List ] :: 
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
    
/*
    def linSearch( p : Parameter, from: Double, to: Double, by: Double ): Stream[ Double ] = {
      import pt.inescn.scratchpad.StreamBuilder._
      val len = ( ( to - from ) / by ).ceil.toInt
      linspace( from, by ).take( len + 1 )
    }

    def linISearch( p : Parameter, from: Int, to: Int, by: Int ): Stream[ Int ] = {
      import pt.inescn.scratchpad.StreamBuilder._
      val len = ( ( to - from ) / by ).ceil.toInt
      linspace( from, by ).take( len + 1 )
    }

    val pr1_1 = rng1.toStream( linISearch )
*/
    /*
    val p1_1 = rng1( 0 ).toStream( 0 )
    val p1_2 = rng1( 1 ).toStream
    val p1_3 = rng1( 2 ).toStream
    val p1p = new m1.Params( p1_1, p1_2 )
*/
  }
}
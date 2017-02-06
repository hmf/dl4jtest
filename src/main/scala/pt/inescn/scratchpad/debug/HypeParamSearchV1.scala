package pt.inescn.scratchpad.debug

// sbt "run-main pt.inescn.scratchpad.debug.HypeParamSearchV1"
object HypeParamSearchV1 {

  import pt.inescn.scratchpad.examples.HList._
  import pt.inescn.scratchpad.examples.HList
  import pt.inescn.scratchpad.examples.HCons
  import pt.inescn.scratchpad.examples.HNil

  trait ParameterRange[ T, U ] {
    val start: T
    val stop: T
    val config: U

    //def toStream : Stream[Parameter] = ???
  }
  trait Parameter

  trait Model {
    trait Params
    trait ParamsRange  {
      type P 
      def get : P
    }

    def getParamRanges: ParamsRange
    def fit( p: Params ): Boolean = ???
    def predict: Boolean = ???
  }

  class ModelA extends Model {

    case class Param1( v: Int ) extends Parameter
    case class Param2( v: Double ) extends Parameter
    //class ParamsX extends Params {
    class Params( param1: Param1, param2: Param2 ) extends super.Params

    def getParamRanges = {
      val pr1 = new ParameterRange[ Int, Int ] { val start = 0; val stop = 10; val config = 1;}
      val pr2 = new ParameterRange[ Double, Double ] { val start = 0.0; val stop = 1.0; val config = 0.1 }
      val pr3 = new ParameterRange[ List[ String ], Int ] { val start = List( "a", "b", "c" ); val stop = List[String](); val config = 1 }
      new ParamsRange { 
        type P = ParameterRange[ Int, Int ]  :: ParameterRange[ Double, Double ] :: ParameterRange[ List[ String ], Int ] :: HNil.type 
        val tmp : P = pr1 :: pr2 :: pr3 :: HNil
        override def get : P = pr1 :: pr2 :: pr3 :: HNil 
      }
    }
    override def fit( p: super.Params ): Boolean = true
    override def predict: Boolean = true
  }

  class ModelB extends Model {

    case class Param1( v: Double ) extends Parameter
    case class Param2( v: Int ) extends Parameter
    class Params( param1: Param1, param2: Param2 ) extends super.Params

    def getParamRanges = {
      val pr1 = new ParameterRange[ Double, Double ] { val start = 0.0; val stop = 1.0; val config = 0.1 }
      val pr2 = new ParameterRange[ Int, Int ] { val start = 0; val stop = 10; val config = 1 }
      val pr3 = new ParameterRange[ List[ String ], Int ] { val start = List( "a", "b", "c" ); val stop = List[String](); val config = 1 }
      new ParamsRange { 
        type P = ParameterRange[ Double, Double ] :: ParameterRange[ Int, Int ]  :: ParameterRange[ List[ String ], Int ] :: HNil.type 
        val tmp : P = pr1 :: pr2 :: pr3 :: HNil
        override def get : P = pr1 :: pr2 :: pr3 :: HNil 
      }
    }
    override def fit( p: super.Params ): Boolean = true
    override def predict: Boolean = true
  }

  def main( args: Array[ String ] ) {
    val m1 = new ModelA
    val param1_1 = new m1.Param1( 1 )
    val param1_2 = new m1.Param2( 1.0 )
    val p1 = new m1.Params( param1_1, param1_2 )
    val p1a = m1.fit( p1 )

    val m2 = new ModelB
    val param2_1 = new m2.Param1( 1.0 )
    val param2_2 = new m2.Param2( 1 )
    //val p2 = new m2.Params(param1_1, param1_2) // type mismatch
    val p2 = new m2.Params( param2_1, param2_2 )
    //val p2a = m2.fit(p1)  // wrong parameters type
    val p2b = m2.fit( p2 )

    val r1 = m1.getParamRanges
    val ps1  = r1.get
    val rng1 = ps1.head
    val rng2 = ps1.tail.head
    val rng3 = ps1.tail.tail.head
    
    /*
    val p1_1 = rng1( 0 ).toStream( 0 )
    val p1_2 = rng1( 1 ).toStream
    val p1_3 = rng1( 2 ).toStream
    val p1p = new m1.Params( p1_1, p1_2 )
*/
  }

}
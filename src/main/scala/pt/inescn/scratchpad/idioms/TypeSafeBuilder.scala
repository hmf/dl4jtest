package pt.inescn.scratchpad.idioms

/**
 * Shows how we can construct an object by ensuring that its construction is safe (has a given state).
 * It combines two patterns:
 * 1. Pimp my library
 *    In order to avoid building an unsafe object, the build call is only injected via an implicit.
 * 2. Phantom types
 *    Are used to hold state to ensure that the constructor parameters are valid (safe)
 *
 * TODO:
 * @see http://jim-mcbeath.blogspot.pt/2009/09/type-safe-builder-in-scala-using-church.html
 * 
 * @see http://blog.rafaelferreira.net/2008/07/type-safe-builder-pattern-in-scala.html
 *          Dick Wall has also some example on contructon builder. .
 */

/*
sealed abstract class Brand  /* This is one way of coding enum-like things in scala */
case object JhnonnyWalker extends Brand
case object ChivasRegal extends Brand
*/

sealed abstract class Preparation /* This is one way of coding enum-like things in scala */
case object Neat extends Preparation
case object OnTheRocks extends Preparation
case object WithWater extends Preparation

sealed abstract class Glass
case object Short extends Glass
case object Tall extends Glass
case object Tulip extends Glass

/**
 * This is the object we want to create safely. We want to ensure that parameters that art
 * passed actually exist. This check is to be done during compile-time.
 */
case class OrderOfScotch( val brand: String, val mode: Preparation, val isDouble: Boolean, val glass: Option[ Glass ] )

/**
 * This is what we want to build. Caveat here is that verifying the presence of non-optional parameters
 * (everything but the glass) is done by the Option.get method. If a field is still None, and we use it
 * then an exception will be thrown. This can occur when we call the build function.
 */
class ScotchBuilder {
  private var theBrand: Option[ String ] = None
  private var theMode: Option[ Preparation ] = None
  private var theDoubleStatus: Option[ Boolean ] = None
  private var theGlass: Option[ Glass ] = None

  def withBrand( b: String ) = { theBrand = Some( b ); this } /* returning this to enable method chaining. */
  def withMode( p: Preparation ) = { theMode = Some( p ); this }
  def isDouble( b: Boolean ) = { theDoubleStatus = Some( b ); this }
  def withGlass( g: Glass ) = { theGlass = Some( g ); this }

  def build() = new OrderOfScotch( theBrand.get, theMode.get, theDoubleStatus.get, theGlass )
}

// Can also use `abstract class`
trait TRUE
trait FALSE

/**
 * This is the safe version of the builder. It uses parametric types to hold state - they indicate
 * when one of the parameters have been filled in. We use TRUE and FALSE as types to indicate
 * when some object parameter has been filled in.
 *
 * Note that we have no build. We only provide a build (via pimp my library) when a 'safe'
 * set  of parameters are available (all type parameters must be 'TRUE' ).
 */
class SafeScotchBuilder[ HB, HM, HD ]( val theBrand: Option[ String ], val theMode: Option[ Preparation ],
                                       val theDoubleStatus: Option[ Boolean ], val theGlass: Option[ Glass ] ) {

  def withBrand( b: String ) = new SafeScotchBuilder[ TRUE, HM, HD ]( Some( b ), theMode, theDoubleStatus, theGlass )
  def withMode( p: Preparation ) = new SafeScotchBuilder[ HB, TRUE, HD ]( theBrand, Some( p ), theDoubleStatus, theGlass )
  def isDouble( b: Boolean ) = new SafeScotchBuilder[ HB, HM, TRUE ]( theBrand, theMode, Some( b ), theGlass ) 
  def withGlass( g: Glass ) = new SafeScotchBuilder[ HB, HM, HD ]( theBrand, theMode, theDoubleStatus, Some( g ) )
}

trait SafeBuild {
  def build(): OrderOfScotch
}

/**
 * Avoid mutability.
 */
object BuilderPattern {

  /**
   *  This is the companion object. It allows one to start the build process. We can start with any member
   *  and once we have a ScotchBuilder object the next calls will use the classes's definitions which mutate
   *  the object.
   */
  class ScotchBuilder( theBrand: Option[ String ], theMode: Option[ Preparation ], theDoubleStatus: Option[ Boolean ], theGlass: Option[ Glass ] ) {
    def withBrand( b: String ) = new ScotchBuilder( Some( b ), theMode, theDoubleStatus, theGlass )
    def withMode( p: Preparation ) = new ScotchBuilder( theBrand, Some( p ), theDoubleStatus, theGlass )
    def isDouble( b: Boolean ) = new ScotchBuilder( theBrand, theMode, Some( b ), theGlass )
    def withGlass( g: Glass ) = new ScotchBuilder( theBrand, theMode, theDoubleStatus, Some( g ) )

    // seems to be required for compilation but is not used for running?
    def build() = new OrderOfScotch( theBrand.get, theMode.get, theDoubleStatus.get, theGlass )
  }

  // Initial object with no members set
  def builder = new ScotchBuilder( None, None, None, None )

  import scala.language.implicitConversions

  /*
   *  Uses the pimp-my-library pattern to make build available. But do this only for the type that has all parameters filled in. 
   *  see http://www.artima.com/weblogs/viewpost.jsp?thread=179766
   *  
   *  Because of the anonymous object below, Scala needs to use reflection: import scala.language.reflectiveCalls
   *  This import should be set just before the call to build when the implicit conversion occurs (see comments below)
   *  see also http://stackoverflow.com/questions/26246879/scala-function-that-returns-an-anonymous-object
   * Below is the use of the structural type. 
   * 
  implicit def enableBuild( builder: SafeScotchBuilder[ TRUE, TRUE, TRUE ] ) = new {
    def build() = new OrderOfScotch( builder.theBrand.get, builder.theMode.get, builder.theDoubleStatus.get, builder.theGlass )
  }
  */
  /**
   * Use a class/trait to avoid use of reflection.
   */
  implicit def enableBuild( builder: SafeScotchBuilder[ TRUE, TRUE, TRUE ] ) = new SafeBuild {
    def build() = new OrderOfScotch( builder.theBrand.get, builder.theMode.get, builder.theDoubleStatus.get, builder.theGlass )
  }

  // Initial object with no members set
  def safeBuilder = new SafeScotchBuilder[ FALSE, FALSE, FALSE ]( None, None, None, None )

}

/**
 * sbt "run-main pt.inescn.scratchpad.idioms.TypeSafeBuilder"
 */
object TypeSafeBuilder {

  def main( args: Array[ String ] ) {
    import BuilderPattern._

    { // Example 1: not safe
      // Filled in everything, so no problem
      val o1 = builder // BuilderPattern.ScotchBuilder
      val o2 = builder withBrand ( "Takes" ) // BuilderPattern.ScotchBuilder
      val order = builder withBrand ( "Takes" ) isDouble ( true ) withGlass ( Tall ) withMode ( OnTheRocks ) build ()
      // Unsafe : java.util.NoSuchElementException: None.get
      //val order = builder  isDouble ( true ) withGlass ( Tall ) withMode ( OnTheRocks ) build ()
    }

    { // Example 2: Safe
      val has_nothing = safeBuilder // SafeScotchBuilder[FALSE, FALSE, FALSE]
      val has_brand = safeBuilder withBrand ( "hi" ) // SafeScotchBuilder[TRUE, FALSE, FALSE]
      val brand_double = safeBuilder withBrand ( "hi" ) isDouble ( false ) //SafeScotchBuilder[TRUE, FALSE, TRUE]
      // now we know Option.get will work when we build()
      //import scala.language.reflectiveCalls
      val order = safeBuilder withBrand ( "hi" ) isDouble ( false ) withGlass ( Tall ) withMode ( Neat ) build ()
      // value build is not a member of pt.inescn.scratchpad.idioms.SafeScotchBuilder[pt.inescn.scratchpad.idioms.TRUE,pt.inescn.scratchpad.idioms.FALSE,pt.inescn.scratchpad.idioms.TRUE]
      //val another_order = safeBuilder withBrand("hi") isDouble(false) withGlass(Tall) build()
    }

  }
}
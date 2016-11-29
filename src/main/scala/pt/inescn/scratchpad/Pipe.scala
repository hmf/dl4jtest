package pt.inescn.scratchpad

//import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.{ Try, Success, Failure } //Success or Failure

/**
 * @see https://medium.com/@anicolaspp/operator-in-scala-cbca7b939fc0#.c29s6e8w3
 * @see https://github.com/loverdos/scalapipes
 *         Uses "extend" + apply instead of an implicit conversion
 * @see http://stackoverflow.com/questions/34646526/pipeline-operator-in-scala
 * 
 * Issues with Pipe design and implementation
 * @see http://stackoverflow.com/questions/40788426/scala-operator-causes-compilation-error-but-not-why/40791933
 * @see http://stackoverflow.com/questions/40769848/scala-missing-parameter-type-in-type-class-operator/40770819
 * 
 * (all letters)
 * |
 * ^
 * &
 * = !
 * < >
 * :
 * + -
 * * / %
 * (all other special characters)
 */
class Pipe[ A ]( a: A ) {
  
  // Basic operators
  def |>[ B ]( f: A => B ) = f( a )     // TODO: Pipe(a) ?
  def $$[B](f: A => B): A = {f(a); a}
  def #!(str: String = ""): A = {print(str) ; println(a); a}

  // Monad operators for Try
  // http://stackoverflow.com/questions/9089893/scala-error-missing-parameter-type
  def !>[ B ]( f: A => B ) : Pipe[Try[B]] = Pipe(Try(f( a )))
  def &>[ B, C ]( f: B => C )(implicit ev: A =:= Try[B]) : Pipe[Try[C]] = Pipe(a.map(f))
  //def %>[ B, C ]( f: B => Try[C] )(implicit ev: A =:= Try[B]) : Pipe[Try[C]] = Pipe(a.flatMap(f))
  
  // map, flatMap and filter are not useful because we can use the Collections methods for this
  /*
  def map[B, C](f: B => C)(implicit ev: A =:= Seq[B]): Seq[C] = a.map(f)
  def flatMap[B,C](f: B => GenTraversableOnce[C])(items: Seq[B])(implicit ev: A =:= Seq[B]): Seq[C] = a.flatMap(f)
  def filter[B](f: B => Boolean)(items: Seq[B])(implicit ev: A =:= Seq[B]): Seq[B] = a.filter(f)*/
}

/**
 * @see https://medium.com/@anicolaspp/operator-in-scala-cbca7b939fc0#.c29s6e8w3
 */
object Pipe {
  def apply[ A ]( v: A ) = new Pipe( v )  
}

/**
 * https://medium.com/@anicolaspp/operator-in-scala-cbca7b939fc0#.c29s6e8w3
 */
object PipeOps {
  implicit def toPipe[ A ]( a: A ) = Pipe( a )
  //implicit def toPipe[ A ]( a: Try[A] ) = Pipe(Try( a ))
 // implicit def toTry[ A ]( a: A) =  Success( a )
}


/**
 * sbt "run-main pt.inescn.scratchpad.PipeCheck"
 */
object PipeCheck {

  /* examples:  http://www.codecommit.com/blog/scala/function-currying-in-scala
   * def add(x:Int, y:Int, z:Int) = x + y + z
  *  val addFive = add(5, _:Int, _:Int)
  *  addFive(3, 1)    // 9
  *  
  * def add(x:Int, y:Int, z:Int) = x + y + z
  * val addFive = (a:Int, b:Int) => add(5, a, b)
  * addFive(3, 1)    // 9
  */
  def modN(n: Int)(x: Int) = ((x % n) == 0)
  val t =  modN(2) _
  def s(x:Int) : Boolean =  modN(2) (x)
  def u = (x:Int) =>  modN(2) (x)
  
  def f(y: Seq[Int]) = y.filter { x: Int => x % 2 == 0 }
  def g = (y:Seq[Int]) => y.filter { x: Int => x % 2 == 0 }

    import PipeOps._

  def main( args: Array[ String ] ) {
    
    import PipeOps._
    
     val r1 = 5 |> (x => x + 1) |> ( x => x + 2)
     println(r1)
     
     // No pipe conversion here. Using standard List map
      val r2 = List(1,2,3,4,5) map (x => x + 1)
      println(r2)
      // Here the result is converted into a Pipe
      List(6,7,8,9) map (x => x + 1) $$ (println _)
      
      val r3 = List(1,2,3,4,5) filter (x => x % 2 == 0)
      println(r3)
      val r3_ = List(1,2,3,4,5) filter { x : Int => x % 2 == 0 }  #! ("r3 = ") //doesn't work, prints a lambda ID
      println(r3_)
      
      // conversion occur due to the pipe operator
      val r4 = List(1,2,3,4,5) |> f $$ (println)
      println(r4)
      
      val r5 = List(1,2,3,4,5) |> g #! ("r5 = ") //doesn't work, prints a lambda ID
      println(r5)
      
      val e1 = List(1,2,3,4) flatMap { x => if ((x % 2) == 0) Some(x) else None }
      println(e1)
      
      val r6 = 100 !> { x: Int   => x * 2 } 
      println(r6)
      
      // If you divide by an integer we get a division by zero exception
      // If you divide by a float we get a Success(infinity)
      val r7 = r6  &> {x : Int => x / 0}
      print("r7 = ") ; println(r7)
      
      //val r8 = 100.0 !> {x : Double => x / 0.0} #> {x:Double => x* 3.0} 
      val r8 = 100.0 !> {x : Double => x / 0.0}  
      println(r8)
      val r9 = r8 &> {x:Double => x* 3.0} 
      println(r9)
      
      val u = Success(100.0)
      val v = Try(100.0)
      
      //val r10 = (100.0 !> {x : Double => x / 0.0}) #> {x:Double => x* 3.0} 
      val r10 = 100.0 !> {x : Double => x / 0.0} &> {x:Double => x* 3.0} 

      // A way to circumvent lint's warnings
      val zero_i = 0
      
      // http://stackoverflow.com/questions/20195544/scala-how-to-understand-the-flatmap-method-of-try
      // http://www.codecommit.com/blog/ruby/monads-are-not-metaphors
      // http://typeclassopedia.bitbucket.org/
      // propagates the results
      print("1. ")
      println(Try(100 / 3) map (_ * 2) map ( _ - 100) )  // Success(-34)
      // but short-cuts on error
      print("2. ")
      println( Try(100 / zero_i) map (_ * 2) map ( _ - 100) )  // Failure(java.lang.ArithmeticException: / by zero)
      print("3. ")
      println(Try(100) map {_ /0 }  ) // Failure(java.lang.ArithmeticException: / by zero)
      // propagates the results
      print("4. ")
      println(  Try(100 / 3) flatMap {x :Int => Try(x*2) } flatMap {x :Int => Try(x-100)} ) // Success(-34)
      print("5. ")
      println( Try(100 / zero_i) flatMap {x :Int => Try(x*2) } flatMap {x :Int => Try(x-100)} ) // Failure(java.lang.ArithmeticException: / by zero)
      print("6. ")
      println( Try(100) flatMap {x :Int => Try(x/0) } flatMap {x :Int => Try(x-100)} ) // Failure(java.lang.ArithmeticException: / by zero)
      
  }
}
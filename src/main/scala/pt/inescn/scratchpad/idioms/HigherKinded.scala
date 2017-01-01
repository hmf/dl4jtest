package pt.inescn.scratchpad.idioms

/**
 * A basic container type that can hold any primitive or complex type
 */
case class AP[T](val v: T) {
  type I = T
}

import scala.language.higherKinds

//class AS[P <: AP[_],T <: P#I](r : AP[T]) {
//class AS[P <: AP[_],T <: P#I](r : AP[P#I]) {
//class AS[P <: AP[_],T <: P#I](r : P) {
//class AS[P <: AP[T],T <: P#I](r : P) {
/**
 * Use a higher-kinded type `P[T]` that allows us to extract the contained
 * type `T` from the container `AP[T]`. In this way we can declare new types 
 * based on the contained type `T`. We can also use a contained type `U` that
 * also enables run-time type conversion (checked at compilation-time of course). 
 * 
 * NOTE: this version uses unsafe `var` for the generator - a call to `map`.
 * may result in a null pointer exception.  Consider using the Safe Builder pattern. 
 */
class AS[P[T] <: AP[T],T,U](r : P[T], var generator : T => U) {
  
  type Gen = T => U
  
  def this(r : P[T]) {
    this(r, null) // Unsafe use of Null
  }
  
  def make[Q[S] <: AP[S],S](nv:  Q[S] ) : AS[Q,S,U] = new AS[Q,S,U](nv) 
  def to[V](g: T => V) = { val t = new AS[P,T,V](r) ; t.generator = g; t }
  def from : T           = r.v
  def map : U            = generator( from )
}

/**
 * Same as the `AS` type above but uses a safer Option types. This avoids
 * the null pointer exception however  an exception will still be possible
 * when we use the `Option.get` methods when `Nothing`is set.  
 */
class AR[P[T] <: AP[T],T,U](val r : Option[P[T]], val generator : Option[T => U]) {
  
  /* Possible
  def this() {
    this(None, None)
  }*/
  
  def this(nr : Option[P[T]]) {
    this(nr, None)
  }
  
  def make[Q[S] <: AP[S],S](nv:  Q[S] ) : AR[Q,S,U] = new AR[Q,S,U]( Some(nv)) 
  def to[V](g: T => V) = { new AR[P,T,V](r, Some(g))  }
  def from : T           = r.get.v
  def map : U            = generator.get( from )
}


/**
 * sbt "run-main pt.inescn.scratchpad.idioms.HigherKinded"
 */
object HigherKinded {

  def main( args: Array[ String ] ) {
    val ai = new AP(100)
    val as = new AP("1000")
    
    
    // Example of morphing higher-kined types in order to set and extract basic inner types
    // We set the contained type with an Int, so the higher type holds an Int
    val w = new AS(ai)                                              // AS[AP, Int, Nothing]
    val a = w.from                                                    // Int
    println(s"AP[Int] = $a")
    // We change the contained type to a String, so the higher type holds a String
    val w1 = w make as                                             // AS[AP, String, Nothing]
    val a1 = w1.from                                                 // String
    println(s"AP[String] = $a1")
    // We set the mapping contained type to a String
    val w2 = w1 to { x => x + "!"}                                // AS[AP, String, String]
    val a2 = w2.map                                                  // Changed string
    println(s"Mapped AP[String to String] = $a2")
    // We set the mapping contained type to an Int
    val w3 = w2 to { x => x.toInt * 10 }                     // AS[AP; String, Int]
    val a3 = w3.map
    println(s"Mapped AP[String to Int] = $a3")

    // Same as above but using the `Option` instead of a `var`
    // We set the contained type with an Int, so the higher type holds an Int
    val v = new AR(Some(ai))                                    // AR[AP, Int, Nothing]
    val b = v.from                                                    // Int
    println(s"AR[Int] = $b")
    // We change the contained type to a String, so the higher type holds a String
    val v1 = v make as                                              // AR[AP, String, Nothing]
    val b1 = v1.from                                                 // String
    println(s"AR[String] = $b1")
    // We set the mapping contained type to a String
    val v2 = v1 to { x => x + "!"}                                // AR[AP, String, String]
    val b2 = v2.map                                                  // Changed string
    println(s"Mapped AR[String to String] = $a2")
    // We set the mapping contained type to an Int
    val v3 = v2 to { x => x.toInt * 10 }                     // AR[AP; String, Int]
    val b3 = v3.map
    println(s"Mapped AP[String to Int] = $a3")
    
  }  
}
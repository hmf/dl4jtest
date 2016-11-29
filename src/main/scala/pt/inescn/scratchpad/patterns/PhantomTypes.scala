package pt.inescn.scratchpad.patterns


/**
 * Shows how we can associate a parametric type that can be used to hold state. 
 * Usually referred to as Phantom types. 
 * 
 * @see http://gigiigig.github.io/tlp-step-by-step/phantom-types.html
 * @see http://blog.rafaelferreira.net/2008/07/type-safe-builder-pattern-in-scala.html
 */
object PhantomTypes {
  trait Status
  trait Open extends Status
  trait Closed extends Status

  trait Door[ S <: Status ]
  object Door {
    def apply[ S <: Status ] = new Door[ S ] {}

    def open[ S <: Closed ]( d: Door[ S ] ) = Door[ Open ]
    def close[ S <: Open ]( d: Door[ S ] ) = Door[ Closed ]
  }

  val closedDoor = Door[ Closed ]
  val openDoor = Door.open( closedDoor )
  val closedAgainDoor = Door.close( openDoor )

  // - type mismatch; found :  pt.inescn.scratchpad.patterns.PhantomTypes.Door[pt.inescn.scratchpad.patterns.PhantomTypes.Closed] required: pt.inescn.scratchpad.patterns.PhantomTypes.Door[S]
	// inferred type arguments [pt.inescn.scratchpad.patterns.PhantomTypes.Closed] do not conform to  method close's type parameter bounds [S <: pt.inescn.scratchpad.patterns.PhantomTypes.Open]
  //val closedClosedDoor = Door.close(closedDoor)
  //val openOpenDoor = Door.open(openDoor)  
}
package pt.inescn.scratchpad.idioms

/**
 * Example from http://jim-mcbeath.blogspot.pt/2009/09/type-safe-builder-in-scala-part-4.html
 * It shows how to create a state machine using type-level programming so that we can record
 * what methods have been called and how many times. 
 * 
 * Note that this implementation uses structural typing and therefore requires the use of 
 *   `import scala.language.reflectiveCalls`
 * Structural typing s found in the use of the state machine type `TT` and the anonymous builder 
 * class created via the implicit ` specsOK(spec:CompleteSpecs)`
 * 
 * Synopsis: we want to calculate the are of a pyramid. We can do so using several combinations 
 * of 
 */
import scala.language.implicitConversions
import scala.language.reflectiveCalls 

object Pyramid {

    //A small collection of class types to define a state machine that counts
    abstract class COUNTER              { type Count <: COUNTER }
    abstract class MANY extends COUNTER { type Count = MANY }
    abstract class TWO  extends COUNTER { type Count = MANY }
    abstract class ZERO_OR_ONE extends COUNTER
    abstract class ONE  extends ZERO_OR_ONE { type Count = TWO }
    abstract class ZERO extends ZERO_OR_ONE { type Count = ONE }

    //We require positive values for our calls
    class Positive(val d:Double) {
        if (d<=0) throw new IllegalArgumentException("non-positive value")
    }
    implicit def doubleToPositive(d:Double) = new Positive(d)
    implicit def intToPositive(n:Int) = new Positive(n)

    //The class that manages the state of our specification
    class Specs private[Pyramid]() { self:Specs =>
        //Caller must set exactly two out of three of these
        val length:Double = 0
        val width:Double = 0
        val area:Double = 0

        //Caller must set exactly one of these two heights
        val height:Double = 0   //vertical height to the tip
        val edge:Double = 0     //from base to tip along an edge

        //We maintain compiler-time state to count the two types of calls
        type TT <: {
            type COUNT_LENGTH <: COUNTER
            type COUNT_WIDTH <: COUNTER
            type COUNT_AREA <: COUNTER
            type COUNT_BASE <: COUNTER          // length, width or area
            type COUNT_HEIGHT <: COUNTER
            type COUNT_EDGE <: COUNTER
            type COUNT_VERT <: COUNTER          // height or edge
        }

        class SpecsWith(bb:Specs) extends Specs {
            override val length = bb.length
            override val width = bb.width
            override val area = bb.area
            override val height = bb.height
            override val edge = bb.edge
        }

        def setLength(d:Positive) = new SpecsWith(this) {
            override val length:Double = d.d
            type TT = self.TT {
                type COUNT_LENGTH = self.TT#COUNT_LENGTH#Count
                type COUNT_BASE = self.TT#COUNT_BASE#Count
            }
        }

        def setWidth(d:Positive) = new SpecsWith(this) {
            override val width:Double = d.d
            type TT = self.TT {
                type COUNT_WIDTH = self.TT#COUNT_WIDTH#Count
                type COUNT_BASE = self.TT#COUNT_BASE#Count
            }
        }

        def setArea(d:Positive) = new SpecsWith(this) {
            override val area:Double = d.d
            type TT = self.TT {
                type COUNT_AREA = self.TT#COUNT_AREA#Count
                type COUNT_BASE = self.TT#COUNT_BASE#Count
            }
        }

        def setHeight(d:Positive) = new SpecsWith(this) {
            override val height:Double = d.d
            type TT = self.TT {
                type COUNT_HEIGHT = self.TT#COUNT_HEIGHT#Count
                type COUNT_VERT = self.TT#COUNT_VERT#Count
            }
        }

        def setEdge(d:Positive) = new SpecsWith(this) {
            override val edge:Double = d.d
            type TT = self.TT {
                type COUNT_EDGE = self.TT#COUNT_EDGE#Count
                type COUNT_VERT = self.TT#COUNT_VERT#Count
            }
        }

    }

    //Starting point: nothing is set
    def apply() = new Specs {
        type TT = {
            type COUNT_LENGTH = ZERO
            type COUNT_WIDTH = ZERO
            type COUNT_AREA = ZERO
            type COUNT_BASE = ZERO
            type COUNT_HEIGHT = ZERO
            type COUNT_EDGE = ZERO
            type COUNT_VERT = ZERO
        }
    }

    //Required ending point: two base measures, one height measure,
    //no single parameter more than once
    type CompleteSpecs = Specs {
        type TT <: {
            type COUNT_LENGTH <: ZERO_OR_ONE
            type COUNT_WIDTH <: ZERO_OR_ONE
            type COUNT_AREA <: ZERO_OR_ONE
            type COUNT_BASE = TWO
            type COUNT_HEIGHT <: ZERO_OR_ONE
            type COUNT_EDGE <: ZERO_OR_ONE
            type COUNT_VERT = ONE
        }
    }

    //Calc1 includes the first set of values that can be calculated
    class Calc1 private[Pyramid](spec:CompleteSpecs) {
        import java.lang.Math.sqrt

        //The three related base measures
        lazy val length = if (spec.length!=0) spec.length
                          else spec.area/spec.width
        lazy val width =  if (spec.width!=0) spec.width
                          else spec.area/spec.length
        lazy val area =   if (spec.area!=0) spec.area
                          else spec.length*spec.width

        //The two related height measures
        lazy val height = if (spec.height!=0) spec.height
            else sqrt(spec.edge*spec.edge-length*length/4-width*width/4)
        lazy val edge =   if (spec.edge!=0) spec.edge
            else sqrt(length*length/4+width*width/4+spec.height*spec.height)

        lazy val volume = length * width * height / 3
    }

    implicit def specsOK(spec:CompleteSpecs) = new {
        def build = new Calc1(spec)
    }

}

object State {
  
  def main( args: Array[ String ] ) {
    import Pyramid._       //we need the implicit conversion to be in scope
    val p = Pyramid().setLength(10).setWidth(8).setHeight(6).build
    p.length                //returns 10
    p.area                  //returns 80
    p.volume                //returns 160

    //Pyramid().setWidth(2).setHeight(2).build        //only one BASE param, need 2
    //Pyramid().setWidth(2).setLength(3).setArea(6).setHeight(2).build  //too many BASE params
    //Pyramid().setWidth(2).setWidth(3).setHeight(2).build  //setWidth called twice
  }
}

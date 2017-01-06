package pt.inescn.scratchpad.idioms

// http://jim-mcbeath.blogspot.pt/2009/09/type-safe-builder-in-scala-part-4.html
import scala.language.implicitConversions
//import scala.language.reflectiveCalls // TODO

    //A small collection of class types to define a state machine that counts
trait COUNTER              { type Count <: COUNTER }
trait MANY extends COUNTER { type Count = MANY }
trait TWO  extends COUNTER { type Count = MANY }
trait ZERO_OR_ONE extends COUNTER
trait ONE  extends ZERO_OR_ONE { type Count = TWO }
trait ZERO extends ZERO_OR_ONE { type Count = ONE }

object StateO {


    //The class that manages the state of our specification
    class Base() { self:Base =>
      
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

        class SpecsWith extends Base {
        }

        //def setLength = new SpecsWith {
        def setLength = new Base {
            type TT = self.TT {
                type COUNT_LENGTH = self.TT#COUNT_LENGTH#Count
                type COUNT_BASE = self.TT#COUNT_BASE#Count
            }
        }

        //def setWidth = new SpecsWith {
        def setWidth = new Base {
            type TT = self.TT {
                type COUNT_WIDTH = self.TT#COUNT_WIDTH#Count
                type COUNT_BASE = self.TT#COUNT_BASE#Count
            }
        }

        //def setArea = new SpecsWith {
        def setArea = new Base {
            type TT = self.TT {
                type COUNT_AREA = self.TT#COUNT_AREA#Count
                type COUNT_BASE = self.TT#COUNT_BASE#Count
            }
        }

        //def setHeight = new SpecsWith {
        def setHeight = new Base {
            type TT = self.TT {
                type COUNT_HEIGHT = self.TT#COUNT_HEIGHT#Count
                type COUNT_VERT = self.TT#COUNT_VERT#Count
            }
        }

        //def setEdge = new SpecsWith {
        def setEdge = new Base {
            type TT = self.TT {
                type COUNT_EDGE = self.TT#COUNT_EDGE#Count
                type COUNT_VERT = self.TT#COUNT_VERT#Count
            }
        }

    }

    //Starting point: nothing is set
    def apply() = new Base {
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
    type CompleteSpecs = Base {
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
    class Calc1(spec:CompleteSpecs) {
    }

    implicit def specsOK(spec:CompleteSpecs) = new {
        def build = new Calc1(spec)
    }

}

object StatefulBuilder {
  
  def main( args: Array[ String ] ) {
    import StateO._       //we need the implicit conversion to be in scope
    val p = StateO().setLength.setWidth.setHeight.build
    //val p = StateO().setLength
    //specsOK(p)

    //StateO().setWidth(2).setHeight(2).build        //only one BASE param, need 2
    //StateO().setWidth(2).setLength(3).setArea(6).setHeight(2).build  //too many BASE params
    //StateO().setWidth(2).setWidth(3).setHeight(2).build  //setWidth called twice

    
    val x0 = StateO()
    //val _ = specsOK(x0)
    //val x1 = x0.setWidth
    val x1 = x0.setLength
    val x2 = specsOK(x1)
  }
}

package pt.inescn.scratchpad.idioms


/**
 * This is an altered version of  the example from:
 *    http://jim-mcbeath.blogspot.pt/2009/09/type-safe-builder-in-scala-part-4.html
 * For the original, see `StatefullBuilderV1`. The main difference here is that we do not use structural types 
 * and hence do not use code reflection. To do this we create and explicit `BaseState` class that has
 * the state types. We then set each and every state type explicitly. This makes the code a little more
 * verbose but is still very readable.  We also define a builder explicitly so that no anonymous class/object 
 * is required. 
 * 
 * We removed superfluous code  regarding the Pyramid calculation. This code may now serve as a basis
 * for other work.
 * 
 * The IDE is still able to help us with type error debugging. 
 */

import scala.language.implicitConversions


object StateO {
    //A small collection of class types to define a state machine that counts
    trait COUNTER                                                { type Count <: COUNTER }
    trait MANY               extends COUNTER          { type Count = MANY }
    trait TWO                 extends COUNTER         { type Count = MANY }
    trait ZERO_OR_ONE extends COUNTER
    trait ONE                  extends ZERO_OR_ONE { type Count = TWO }
    trait ZERO                 extends ZERO_OR_ONE { type Count = ONE }

    /* 
     * Compiler-time state coded as a type
     * Can be a class, abstract class or trait 
     */
    trait BaseState { 
      
      type COUNT_LENGTH <: COUNTER
      type COUNT_WIDTH <: COUNTER
      type COUNT_AREA <: COUNTER
      type COUNT_BASE      <: COUNTER          // length, width or area
      type COUNT_HEIGHT <: COUNTER
      type COUNT_EDGE      <: COUNTER
      type COUNT_VERT      <: COUNTER          // height or edge
    }
  
    //The class that manages the state of our specification
    class Base() { self:Base =>
      
        //We maintain compiler-time state to count the two types of calls
       type ST <: BaseState

        def setSLength = new Base {
            type ST = BaseState {
                type COUNT_LENGTH = self.ST#COUNT_LENGTH#Count
                type COUNT_WIDTH  = self.ST#COUNT_WIDTH
                type COUNT_AREA     = self.ST#COUNT_AREA
                type COUNT_BASE      = self.ST#COUNT_BASE#Count  // length, width or area
                type COUNT_HEIGHT = self.ST#COUNT_HEIGHT
                type COUNT_EDGE      = self.ST#COUNT_EDGE
                type COUNT_VERT      = self.ST#COUNT_VERT // height or edge
            }
        }

        def setSWidth = new Base {
            type ST = BaseState {
                type COUNT_LENGTH = self.ST#COUNT_LENGTH
                type COUNT_WIDTH  = self.ST#COUNT_WIDTH#Count
                type COUNT_AREA     = self.ST#COUNT_AREA
                type COUNT_BASE     = self.ST#COUNT_BASE#Count // length, width or area
                type COUNT_HEIGHT = self.ST#COUNT_HEIGHT
                type COUNT_EDGE     = self.ST#COUNT_EDGE
                type COUNT_VERT     = self.ST#COUNT_VERT // height or edge
            }
        }
        
        def setSArea = new Base {
            type ST = BaseState  {
                type COUNT_LENGTH  = self.ST#COUNT_LENGTH
                type COUNT_WIDTH   = self.ST#COUNT_WIDTH
                type COUNT_AREA      = self.ST#COUNT_AREA#Count
                type COUNT_BASE      = self.ST#COUNT_BASE#Count // length, width or area
                type COUNT_HEIGHT = self.ST#COUNT_HEIGHT
                type COUNT_EDGE      = self.ST#COUNT_EDGE
                type COUNT_VERT      = self.ST#COUNT_VERT // height or edge
            }
        }

        def setSHeight = new Base {
            type ST = BaseState {
                type COUNT_LENGTH = self.ST#COUNT_LENGTH
                type COUNT_WIDTH  = self.ST#COUNT_WIDTH
                type COUNT_AREA     = self.ST#COUNT_AREA
                type COUNT_BASE     = self.ST#COUNT_BASE          // length, width or area
                type COUNT_HEIGHT = self.ST#COUNT_HEIGHT#Count
                type COUNT_EDGE      = self.ST#COUNT_EDGE
                type COUNT_VERT      = self.ST#COUNT_VERT#Count // height or edge
            }
        }

        def setSEdge = new Base {
            type ST = BaseState {
                type COUNT_LENGTH  = self.ST#COUNT_LENGTH
                type COUNT_WIDTH   = self.ST#COUNT_WIDTH
                type COUNT_AREA      = self.ST#COUNT_AREA
                type COUNT_BASE      = self.ST#COUNT_BASE // length, width or area
                type COUNT_HEIGHT  = self.ST#COUNT_HEIGHT
                type COUNT_EDGE       = self.ST#COUNT_EDGE#Count
                type COUNT_VERT       = self.ST#COUNT_VERT#Count // height or edge
            }
        }

    }

    /**
     * Starting point: nothing is set
     */
    def apply() = new Base {
      
        type ST = BaseState {
            type COUNT_LENGTH = ZERO
            type COUNT_WIDTH = ZERO
            type COUNT_AREA = ZERO
            type COUNT_BASE = ZERO
            type COUNT_HEIGHT = ZERO
            type COUNT_EDGE = ZERO
            type COUNT_VERT = ZERO
        }
          
    }
        
    /** 
     *  Required ending point: two base measures, one height measure,
     *  no single parameter more than once
     */
    type SCompleteSpecs = Base {
          type ST <: BaseState {
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
    class Calc2(spec:SCompleteSpecs) {
    }

    trait Builder {
      def build : Calc2
    }
    
    implicit def specsSOK(spec:SCompleteSpecs) = new Builder {
        def build = new Calc2(spec)
    }

}

object StatefullBuilderV2 {
  
  def main( args: Array[ String ] ) {
    
    import StateO._       //we need the implicit conversion to be in scope
    
    val p = StateO().setSLength.setSWidth.setSHeight.build

    // An easy way to debug the type is t call the implicit converter manually
    // The error will show the type in its expanded form
    val q = StateO()
    //val _ = specsSOK(q)
    val q0 = StateO().setSLength
    //val _ = specsSOK(q0)
    val q1 = StateO().setSLength.setSWidth
    //val _ = specsSOK(q1)
    //val q1 = StateO().setSLength.setSWidth.setSHeight.build
    val qn = StateO().setSLength.setSWidth.setSHeight
    val _  = specsSOK(qn)
    val c1 = qn.build
    
    // In the IDE if we hover over the `build` we cab see the type
    //StateO().setSWidth.setSHeight.build        //only one BASE param, need 2
    //StateO().setSWidth.setSLength.setSArea.setSHeight.build  //too many BASE params
    //StateO().setSWidth.setSWidth.setSHeight.build  //setWidth called twice
    
  }
}

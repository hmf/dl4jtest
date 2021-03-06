package pt.inescn.scratchpad.implicitlogic

/*
 * Copyright 2016 Jasper Moeys
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//import scala.annotation.compileTimeOnly

sealed trait Not[A] {
  override def toString: String = "Not"
}

object Not {
  private val notInstance = new Not[Any] { }
  
  implicit def makeNot[A]: Not[A] = notInstance.asInstanceOf[Not[A]]
  //@compileTimeOnly("The method `makeNotAmbig` should never be called.")
  implicit def makeNotAmbig[A](implicit a: A): Not[A] = sys.error("this method should never be called")
}
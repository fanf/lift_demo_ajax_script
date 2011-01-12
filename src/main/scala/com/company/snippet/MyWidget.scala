package com.company.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.util._
import net.liftweb.common._
import Helpers._
import net.liftweb.http._
import js._
import JsCmds._

import scala.xml._

/**
 * 
 * A complexe widget component, that may both be called
 * in AJAX or by direct HTTP rendering.
 * 
 *
 */
class MyWidget extends DispatchSnippet {

  def dispatch = { 
    
    case "displayWidget1" => { _ => displayWidget1 }
    case "displayWidget2" => { _ => 
      val (xml,js) = displayWidget2 
      xml ++ Script(js)
    }
    case "ajaxButtons" => { _ => ajaxButtons }
  }
    
    
    /**
     * The widget mixes HTML tag and JS script.
     * Works great with HTTP GET, not in Ajax
     */
    def displayWidget1() : NodeSeq = {
      <div>
        <p>I'm displayWidget1 and my JS won't work in AJAX</p>
        {
          Script(Alert("I'm alert displayWidget1"))
        }
      </div>
      
    }
    
    /**
     * Workaround patter: show the JS init code in the return, and use it:
     * - directly in AJAX call
     * - in a script tag in HTTP
     */
    def  displayWidget2() : (NodeSeq, JsCmd) = {
      (
        <div>
          <p>I'm displayWidget2 and my JS works in AJAX</p>
        </div>
      ,
        Alert("I'm alert displayWidget2")
      )
    }
    
    /**
     * A zone with ajax call to the two widgets
     */
    def ajaxButtons() : NodeSeq = {
      //here, I don't know that displayWidget1 need some JS initialization
      def ajaxDisplayWidget1 : JsCmd = SetHtml("ajaxW1", displayWidget1) 
      //here, the JS initialization is exposed and can be called after the ajax HTML update
      def ajaxDisplayWidget2 : JsCmd = {
        val (xml, js) = displayWidget2
        SetHtml("ajaxW2", xml) & js
      }
       
      <div>
        <p>{SHtml.ajaxButton(Text("Widget 1"), ajaxDisplayWidget1 _)}</p>
        <div id="ajaxW1">updated by widget 1 - no alert</div>      
        
        <p>{SHtml.ajaxButton(Text("Widget 2"), ajaxDisplayWidget2 _)}</p>
        <div id="ajaxW2">updated by widget 2 - alert show</div>
      </div>
    }
}
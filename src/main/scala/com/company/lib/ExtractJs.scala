package com.company.lib

import scala.xml._
import net.liftweb.http.js._
import JsCmds._ // For implicits
import JE._
import net.liftweb.util.Helpers._

/**
* Extract JS from a NodeSeq
* Useful when using Lift SetHtml/Replace and it doesn't run the embedeed JS
*
* CAUTION : the <lift:snippet>, if not eager eval, are expanded only later :
* if JS are expected in them it won't be found
*/
object ExtractJs {
  val regexp = """(?s).*// \<\!\[CDATA\[(.*)\]\]\>.*""".r
 
    
  def extractJsFromNodes(html : NodeSeq) : JsCmd = {
    var js = Noop
    
    def recurse(in: NodeSeq) : Unit = {
      in foreach {
        case node : Elem if node.label == "script" =>
          node.attribute("type") match {
            case Some(s) if s.toString == "text/javascript" =>
              node.toString match{
                case regexp(content) =>
                  js = js & JsRaw(content)
                case _ => println("should have been match")
                println(node.toString)
              }
            case _ => //do not process that script tag, not javascript
          }
        case node : Elem => recurse(node.child);
        case g:Group => recurse(g.child)
        case _ => // ?
      }
    }

    recurse(html)
    js
  }

  /*
* Extract script tag from content, build a sequence of JsCmd from them, pass the
* filtered content to a NodeSeq => JsCmd command and add extracted commands
*/
  private[this] def extractAndFilterAndExec(content:NodeSeq)(command: NodeSeq => JsCmd) : JsCmd = {
    val js = ExtractJs.extractJsFromNodes(content)
    //filter script tag from xml
    val xml = ("script [type=text/javascript]" #> NodeSeq.Empty)(content)
    command(xml) & js
  }

  //override JsCmd Replace and SetHtml

  /**
* Replacement for Lift JsCmd.Replace that:
* - parse HTML content to find <script> tags ;
* - filter them out of content ;
* - add the list of JS commands from <script> tags after the Replace command.
*/
  def execReplace(id: String, content: NodeSeq) : JsCmd = {
    extractAndFilterAndExec(content){ filteredXML => Replace(id, filteredXML) }
  }

  /**
* Replacement for Lift JsCmd.Replace that:
* - parse HTML content to find <script> tags ;
* - filter them out of content ;
* - add the list of JS commands from <script> tags after the Replace command.
*/
  def execSetHtml(id: String, content: NodeSeq) : JsCmd = {
    extractAndFilterAndExec(content){ filteredXML => SetHtml(id, filteredXML) }
  }
}
/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway.testkit


import com.wix.pay.eway.model.EwayAuthorization
import com.wix.pay.eway.model.parsers.{EwayAuthorizationParser, JsonEwayAuthorizationParser}
import org.specs2.matcher.{Matcher, Matchers}


/** Introduces Matchers for eWay data.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
trait EwayMatchers extends Matchers {
  def authorizationParser: EwayAuthorizationParser

  def beAuthorizationKey(authorization: Matcher[EwayAuthorization]): Matcher[String] = {
    authorization ^^ { authorizationParser.parse(_: String) aka "parsed authorization"}
  }
}

object EwayMatchers extends EwayMatchers {
  override val authorizationParser = new JsonEwayAuthorizationParser()
}
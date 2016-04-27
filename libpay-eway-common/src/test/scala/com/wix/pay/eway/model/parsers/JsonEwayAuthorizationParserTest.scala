/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway.model.parsers


import com.wix.pay.eway.model.EwayAuthorization
import org.specs2.mutable.SpecWithJUnit


/** The Unit-Test class for the [[JsonEwayAuthorizationParser]] class.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class JsonEwayAuthorizationParserTest extends SpecWithJUnit {
  val parser = new JsonEwayAuthorizationParser
  val transactionNum = "kuki buki"
  val amount = 333


  "stringify and then parse" should {
    "yield a transaction similar to the original one" in {
      val authorization = EwayAuthorization(transactionNum, amount)
      val authorizationKey = parser.stringify(authorization)

      parser.parse(authorizationKey) must be_==(authorization)
    }
  }
}

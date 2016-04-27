/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway.model.parsers


import com.wix.pay.eway.model.EwayMerchant
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope


/** The Unit-Test class for the [[JsonEwayMerchantParser]] class.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class JsonEwayMerchantParserTest extends SpecWithJUnit {

  trait Ctx extends Scope {
    val parser = new JsonEwayMerchantParser
    val customerId = "kuki buki"
    val refundPwd = "shuki tuki"
  }


  "stringify and then parse" should {
    "yield a merchant similar to the original one" in new Ctx {
      val merchant = EwayMerchant(customerId, refundPwd)
      val merchantKey = parser.stringify(merchant)

      parser.parse(merchantKey) must be_==(merchant)
    }
  }
}

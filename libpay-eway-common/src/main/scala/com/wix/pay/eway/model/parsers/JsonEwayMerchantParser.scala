/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway.model.parsers


import com.wix.pay.eway.model.EwayMerchant
import org.json4s.DefaultFormats
import org.json4s.native.Serialization


/** A concrete subclass of the [[EwayMerchantParser]], using JSON for parsing to and from eWay merchants.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class JsonEwayMerchantParser extends EwayMerchantParser {
  private implicit val formats = DefaultFormats

  override def parse(merchantKey: String): EwayMerchant = {
    Serialization.read[EwayMerchant](merchantKey)
  }

  override def stringify(merchant: EwayMerchant): String = {
    Serialization.write(merchant)
  }
}

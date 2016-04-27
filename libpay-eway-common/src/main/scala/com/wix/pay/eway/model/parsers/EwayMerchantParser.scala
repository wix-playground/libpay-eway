/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway.model.parsers


import com.wix.pay.eway.model.EwayMerchant


/** This trait defines the operations for parsing eWay merchant.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
trait EwayMerchantParser {
  def parse(merchant: String): EwayMerchant

  def stringify(merchant: EwayMerchant): String
}

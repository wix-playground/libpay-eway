/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway.model.parsers


import com.wix.pay.eway.model.EwayAuthorization


/** This trait defines the operations for parsing eWay's authorization key.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
trait EwayAuthorizationParser {
  def parse(authorizationKey: String): EwayAuthorization

  def stringify(authorization: EwayAuthorization): String
}

/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway.model.parsers


import com.wix.pay.eway.model.EwayAuthorization
import org.json4s.DefaultFormats
import org.json4s.native.Serialization


/** A concrete subclass of the [[EwayAuthorizationParser]], using JSON for parsing to and from eWay authorization key.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class JsonEwayAuthorizationParser extends EwayAuthorizationParser {
  private implicit val formats = DefaultFormats

  override def parse(authorizationKey: String): EwayAuthorization = {
    Serialization.read[EwayAuthorization](authorizationKey)
  }

  override def stringify(authorization: EwayAuthorization): String = {
    Serialization.write(authorization)
  }
}

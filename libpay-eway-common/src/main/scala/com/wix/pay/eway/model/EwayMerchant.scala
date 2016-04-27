/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway.model


/** A container to hold eWay merchant's credentials.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
case class EwayMerchant(customerId: String, refundPwd: String)

/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway.model


/** A container to hold eWay authorization details:
  *  - eWay's transaction number
  *  - the original amount, in cents (e.g., 1.00 AUD = 100)
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
case class EwayAuthorization(transactionNum: String, amount: Int)

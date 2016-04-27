/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway


/** An exception raised upon sending an invalid currency.
  *
  * eWay accepts only Australian Dollars ('AUD'), and so, using any other currency will cause this exception to be
  * raised.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
case class InvalidCurrencyException(currency: String)
  extends RuntimeException(s"Invalid currency: '$currency' (must be 'AUD').")

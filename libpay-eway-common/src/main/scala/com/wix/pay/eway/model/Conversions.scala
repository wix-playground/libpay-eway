package com.wix.pay.eway.model

import java.math.{BigDecimal => JBigDecimal}

object Conversions {
  def toEwayAmount(amount: Double): Int = {
    JBigDecimal.valueOf(amount).movePointRight(2).intValueExact()
  }
}

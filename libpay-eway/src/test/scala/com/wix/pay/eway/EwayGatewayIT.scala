/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway


import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.eway.model.Conversions._
import com.wix.pay.eway.model.parsers.{JsonEwayAuthorizationParser, JsonEwayMerchantParser}
import com.wix.pay.eway.model.{EwayAuthorization, EwayMerchant}
import com.wix.pay.eway.testkit.EwayGatewayDriver
import com.wix.pay.model.{CurrencyAmount, Customer, Name}
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import spray.http.StatusCodes

import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}


/** The Integration-Test class of the eWay gateway; validates and specifies eWay gateway integration.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class EwayGatewayIT extends SpecWithJUnit {
  val merchantParser = new JsonEwayMerchantParser()
  val transactionParser = new JsonEwayAuthorizationParser()
  val prob = new EmbeddedHttpProbe(9903, EmbeddedHttpProbe.NotFoundHandler)
  val someMerchantKey = merchantParser.stringify(EwayMerchant("87654321", "kuki buki"))
  val someErroneousMerchantKey = merchantParser.stringify(EwayMerchant("erroneous customer", "shuki tuki"))
  val someCurrencyAmount = CurrencyAmount("AUD", 33.3)
  val someNonCvnCreditCard = CreditCard(
    number = "4012888888881881",
    expiration = YearMonth(2050, 10),
    additionalFields = Some(CreditCardOptionalFields.withFields(
      holderName = Some("John Smith"))))
  val someCvnCreditCard = someNonCvnCreditCard.copy(
    additionalFields = Option(
      someNonCvnCreditCard.additionalFields.getOrElse[CreditCardOptionalFields](CreditCardOptionalFields())
        .copy(csc = Option("123"))))
  val someFailingCreditCard = someNonCvnCreditCard.copy(number = "4012888888882871")
  val someCustomer = Customer(
    name = Some(Name("kuki", "buki")),
    phone = Some("333-333-333"),
    email = Some("kuki.buki@shuki.tuki"))
  val randomTransactionId: () => String = () => {
    Random.nextDouble().toString.substring(2)
  }
  val transactionKeyFor: CurrencyAmount => String = currencyAmount => {
    transactionParser.stringify(EwayAuthorization(randomTransactionId(), toEwayAmount(currencyAmount.amount)))
  }

  val driver = new EwayGatewayDriver {
    override def ewayProb: EmbeddedHttpProbe = prob
  }

  trait Ctx extends Scope {
    val ewayGateway = new EwayGateway("http://localhost:9903", 1.seconds)
    prob.reset()
  }


  step {
    prob.doStart()
  }

  sequential


  "authorize request" should {
    val authorizationKey = transactionKeyFor(someCurrencyAmount)

    "successfully yield an authorization key upon valid request (no cvv)" in new Ctx {
      driver.anAuthorizeRequestFor(
        Some(someMerchantKey),
        Some(someCurrencyAmount),
        Some(someNonCvnCreditCard)) returns authorizationKey

      ewayGateway.authorize(someMerchantKey, someNonCvnCreditCard, someCurrencyAmount) must
        beASuccessfulTry(
          check = ===(authorizationKey)
        )
    }

    "successfully yield an authorization key upon valid request (with cvv)" in new Ctx {
      driver.anAuthorizeRequestFor(
        Some(someMerchantKey),
        Some(someCurrencyAmount),
        Some(someCvnCreditCard)) returns authorizationKey

      ewayGateway.authorize(someMerchantKey, someCvnCreditCard, someCurrencyAmount) must
        beASuccessfulTry(
          check = ===(authorizationKey)
        )
    }

    "successfully yield an authorization key upon valid for a known customer" in new Ctx {
      driver.anAuthorizeRequestFor(
        Some(someMerchantKey),
        Some(someCurrencyAmount),
        Some(someCvnCreditCard),
        Some(someCustomer)) returns authorizationKey

      ewayGateway.authorize(someMerchantKey, someCvnCreditCard, someCurrencyAmount, Some(someCustomer)) must
        beASuccessfulTry(
          check = ===(authorizationKey)
        )
    }

    "gracefully return a reject error upon invalid request" in new Ctx {
      driver.anAuthorizeRequestFor(
        Some(someMerchantKey),
        Some(someCurrencyAmount),
        Some(someFailingCreditCard)) rejects("03", "m3$$age")

      ewayGateway.authorize(
        someMerchantKey,
        someFailingCreditCard,
        someCurrencyAmount) must
          be_==(Failure(PaymentRejectedException("Error Code: 03, Error Message: m3$$age.")))
    }

    "gracefully return an error, upon eway side error" in new Ctx {
      val httpStatus = StatusCodes.GatewayTimeout

      driver.anAuthorizeRequestFor(
        Some(someErroneousMerchantKey),
        Some(someCurrencyAmount),
        Some(someFailingCreditCard)) errors httpStatus

      ewayGateway.authorize(
        someErroneousMerchantKey,
        someFailingCreditCard,
        someCurrencyAmount) must
          be_==(Failure(PaymentErrorException(s"eWay server returned ${httpStatus.intValue} status.")))
    }

    "not accept currency which is not Australian Dollar ('AUD')" in new Ctx {
      val currency = "USD"

      ewayGateway.authorize(
        someErroneousMerchantKey,
        someFailingCreditCard,
        CurrencyAmount(currency, 33.33)) must be_==(Failure(InvalidCurrencyException(currency)))
    }
  }


  "capture" should {
    val someCaptureTransactionId = randomTransactionId()
    val refAuthorizationKey = transactionKeyFor(someCurrencyAmount)
    val unknownAuthorizationKey = transactionKeyFor(CurrencyAmount("AUD", 33333))
    val someAmount = someCurrencyAmount.amount

    "successfully yield a transaction key, upon a valid request" in new Ctx {
      driver.aCaptureRequestFor(
        Some(someMerchantKey),
        Some(refAuthorizationKey),
        Some(someAmount)) returns someCaptureTransactionId

      ewayGateway.capture(
        someMerchantKey,
        refAuthorizationKey,
        someAmount) must be_===(Success(someCaptureTransactionId))
    }

    "gracefully return a reject error upon invalid request" in new Ctx {
      driver.aCaptureRequestFor(
        Some(someMerchantKey),
        Some(unknownAuthorizationKey),
        Some(someAmount)) rejects("03", "m3$$age")

      ewayGateway.capture(
        someMerchantKey,
        unknownAuthorizationKey,
        someAmount) must
          be_==(Failure(PaymentRejectedException("Error Code: 03, Error Message: m3$$age.")))
    }

    "gracefully return an error, upon eWay side error" in new Ctx {
      val httpStatus = StatusCodes.GatewayTimeout

      driver.aCaptureRequestFor(
        Some(someErroneousMerchantKey),
        Some(unknownAuthorizationKey),
        Some(someAmount)) errors httpStatus

      ewayGateway.capture(
        someErroneousMerchantKey,
        unknownAuthorizationKey,
        someAmount) must
          be_==(Failure(PaymentErrorException(s"eWay server returned ${httpStatus.intValue} status.")))
    }
  }


  "sale request" should {
    val someSaleTransactionId = randomTransactionId()

    "successfully yield a transaction key upon valid request (no cvv)" in new Ctx {
      driver.aSaleRequestFor(
        Some(someMerchantKey),
        Some(someCurrencyAmount),
        Some(someNonCvnCreditCard)) returns someSaleTransactionId

      ewayGateway.sale(someMerchantKey, someNonCvnCreditCard, someCurrencyAmount) must
        be_===(Success(someSaleTransactionId))
    }

    "successfully yield a transaction key upon valid request (with cvv)" in new Ctx {
      driver.aSaleRequestFor(
        Some(someMerchantKey),
        Some(someCurrencyAmount),
        Some(someCvnCreditCard)) returns someSaleTransactionId

      ewayGateway.sale(someMerchantKey, someCvnCreditCard, someCurrencyAmount) must
        be_===(Success(someSaleTransactionId))
    }

    "successfully yield a transaction key upon valid for a known customer" in new Ctx {
      driver.aSaleRequestFor(
        Some(someMerchantKey),
        Some(someCurrencyAmount),
        Some(someCvnCreditCard),
        Some(someCustomer)) returns someSaleTransactionId

      ewayGateway.sale(someMerchantKey, someCvnCreditCard, someCurrencyAmount, Some(someCustomer)) must
        be_===(Success(someSaleTransactionId))
    }

    "gracefully return a reject error upon invalid request" in new Ctx {
      driver.aSaleRequestFor(
        Some(someMerchantKey),
        Some(someCurrencyAmount),
        Some(someFailingCreditCard)) rejects("03", "m3$$age")

      ewayGateway.sale(
        someMerchantKey,
        someFailingCreditCard,
        someCurrencyAmount) must
          be_==(Failure(PaymentRejectedException("Error Code: 03, Error Message: m3$$age.")))
    }

    "gracefully return an error, upon eway side error" in new Ctx {
      val httpStatus = StatusCodes.GatewayTimeout

      driver.aSaleRequestFor(
        Some(someErroneousMerchantKey),
        Some(someCurrencyAmount),
        Some(someFailingCreditCard)) errors httpStatus

      ewayGateway.sale(
        someErroneousMerchantKey,
        someFailingCreditCard,
        someCurrencyAmount) must
          be_==(Failure(PaymentErrorException(s"eWay server returned ${httpStatus.intValue} status.")))
    }

    "not accept currency which is not Australian Dollar ('AUD')" in new Ctx {
      val currency = "USD"

      ewayGateway.sale(
        someErroneousMerchantKey,
        someFailingCreditCard,
        CurrencyAmount(currency, 33.33),
        Some(someCustomer)) must be_==(Failure(InvalidCurrencyException(currency)))
    }
  }


  "void authorization" should {
    val voidTransactionId = randomTransactionId()
    val refAuthorizationKey = transactionKeyFor(someCurrencyAmount)
    val unknownAuthorizationKey = transactionKeyFor(CurrencyAmount("AUD", 33333))

    "successfully yield a transaction key, upon a valid request" in new Ctx {
      driver.anVoidAuthorizationRequestFor(
        Some(someMerchantKey),
        Some(refAuthorizationKey)) returns voidTransactionId

      ewayGateway.voidAuthorization(
        someMerchantKey,
        refAuthorizationKey) must be_===(Success(voidTransactionId))
    }

    "gracefully return a reject error upon invalid request" in new Ctx {
      driver.anVoidAuthorizationRequestFor(
        Some(someMerchantKey),
        Some(unknownAuthorizationKey)) rejects("03", "m3$$age")

      ewayGateway.voidAuthorization(
        someMerchantKey,
        unknownAuthorizationKey) must
          be_==(Failure(PaymentRejectedException("Error Code: 03, Error Message: m3$$age.")))
    }

    "gracefully return an error, upon eWay side error" in new Ctx {
      val httpStatus = StatusCodes.GatewayTimeout

      driver.anVoidAuthorizationRequestFor(
        Some(someErroneousMerchantKey),
        Some(unknownAuthorizationKey)) errors httpStatus

      ewayGateway.voidAuthorization(
        someErroneousMerchantKey,
        unknownAuthorizationKey) must
          be_==(Failure(PaymentErrorException(s"eWay server returned ${httpStatus.intValue} status.")))
    }
  }


  step {
    prob.doStop()
  }
}
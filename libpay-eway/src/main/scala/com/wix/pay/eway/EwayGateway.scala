/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway


import akka.actor.ActorSystem
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.eway.model.Conversions._
import com.wix.pay.eway.model.EwayAuthorization
import com.wix.pay.eway.model.parsers.{JsonEwayAuthorizationParser, JsonEwayMerchantParser}
import com.wix.pay.model._
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentGateway, PaymentRejectedException}
import spray.client.pipelining.sendReceive
import spray.http._
import spray.httpx.RequestBuilding._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Try}
import scala.xml.{Elem, XML}


object Endpoints {
  val production = "https://www.eway.com.au"
}

/** A subclass of the `PaymentGateway`, for eWay gateway.
  *
  * @param baseUrl
  *                The base URL for submitting payment request (the url upto the path part; usually includes the
  *                protocol, the host and optionally the port)
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class EwayGateway(baseUrl: String = Endpoints.production, timeout: Option[Duration] = None) extends PaymentGateway {
  implicit val system = ActorSystem()
  import system.dispatcher

  val merchantParser = new JsonEwayMerchantParser()
  val authorizationParser = new JsonEwayAuthorizationParser()

  private def postRequest(requestXml: Elem, url: String): Try[String] = {
    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
    val futureResponse = pipeline(Post(url, requestXml))

    Try {
      val response = Await.result(futureResponse, timeout.getOrElse(Duration.Inf))

      response match {
        case HasXmlResponse(responseXml) =>
          val status = (responseXml \ "ewayTrxnStatus").text.toBoolean

          if (status) {
            (responseXml \ "ewayTrxnNumber").text
          } else {
            val error = (responseXml \ "ewayTrxnError").text

            throw PaymentRejectedException(s"Error Code: ${error.take(2)}, Error Message: ${error.substring(3)}." )
          }

        case _ => throw PaymentErrorException(s"eWay server returned ${response.status.intValue} status.")
      }
    } match {
      case Failure(e: PaymentException) => Failure(e)
      case Failure(e) => Failure(PaymentErrorException(cause = e))
      case otherwise => otherwise
    }
  }

  override def authorize(merchantKey: String,
                         creditCard: CreditCard,
                         currencyAmount: CurrencyAmount,
                         customer: Option[Customer],
                         deal: Option[Deal]): Try[String] = {
    val merchant = merchantParser.parse(merchantKey)
    val ewayAmount = toEwayAmount(currencyAmount.amount)
    val requestXml =
      <ewaygateway>
        <ewayCustomerID>{merchant.customerId}</ewayCustomerID>
        <ewayTotalAmount>{ewayAmount}</ewayTotalAmount>
        <ewayCustomerFirstName>{customer.fold(""){ _.name.fold("")(_.first)}}</ewayCustomerFirstName>
        <ewayCustomerLastName>{customer.fold(""){ _.name.fold("")(_.last)}}</ewayCustomerLastName>
        <ewayCustomerEmail>{customer.fold("")(_.email.getOrElse(""))}</ewayCustomerEmail>
        <ewayCustomerAddress></ewayCustomerAddress>
        <ewayCustomerPostcode></ewayCustomerPostcode>
        <ewayCustomerInvoiceDescription></ewayCustomerInvoiceDescription>
        <ewayCustomerInvoiceRef></ewayCustomerInvoiceRef>
        <ewayCardHoldersName>{creditCard.holderName.getOrElse("")}</ewayCardHoldersName>
        <ewayCardNumber>{creditCard.number}</ewayCardNumber>
        <ewayCardExpiryMonth>{creditCard.expiration.month}</ewayCardExpiryMonth>
        <ewayCardExpiryYear>{creditCard.expiration.year % 100}</ewayCardExpiryYear>
        <ewayTrxnNumber></ewayTrxnNumber>
        <ewayOption1></ewayOption1>
        <ewayOption2></ewayOption2>
        <ewayOption3></ewayOption3>
        <ewayCVN>{creditCard.csc.getOrElse("")}</ewayCVN>
      </ewaygateway>
    val url = s"$baseUrl/${creditCard.csc.fold("gateway/xmlauth.asp")(_ => "gateway_cvn/xmlauth.asp")}"

    if (currencyAmount.currency != "AUD") {
      Failure(new InvalidCurrencyException(currencyAmount.currency))
    } else {
      postRequest(requestXml, url) map { authorizationId =>
        val authorization = EwayAuthorization(authorizationId, ewayAmount)

        authorizationParser.stringify(authorization)
      }
    }
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = {
    val merchant = merchantParser.parse(merchantKey)
    val authorization = authorizationParser.parse(authorizationKey)
    val requestXml =
      <ewaygateway>
        <ewayCustomerID>{merchant.customerId}</ewayCustomerID>
        <ewayAuthTrxnNumber>{authorization.transactionNum}</ewayAuthTrxnNumber>
        <ewayTotalAmount>{toEwayAmount(amount)}</ewayTotalAmount>
        <ewayCardExpiryMonth></ewayCardExpiryMonth>
        <ewayCardExpiryYear></ewayCardExpiryYear>
        <ewayOption1></ewayOption1>
        <ewayOption2></ewayOption2>
        <ewayOption3></ewayOption3>
      </ewaygateway>

    postRequest(requestXml, s"$baseUrl/gateway/xmlauthcomplete.asp")
  }

  override def sale(merchantKey: String,
                    creditCard: CreditCard,
                    currencyAmount: CurrencyAmount,
                    customer: Option[Customer],
                    deal: Option[Deal]): Try[String] = {
    val merchant = merchantParser.parse(merchantKey)
    val requestXml =
      <ewaygateway>
        <ewayCustomerID>{merchant.customerId}</ewayCustomerID>
        <ewayTotalAmount>{toEwayAmount(currencyAmount.amount)}</ewayTotalAmount>
        <ewayCustomerFirstName>{customer.fold(""){ _.name.fold("")(_.first)}}</ewayCustomerFirstName>
        <ewayCustomerLastName>{customer.fold(""){ _.name.fold("")(_.last)}}</ewayCustomerLastName>
        <ewayCustomerEmail>{customer.fold("")(_.email.getOrElse(""))}</ewayCustomerEmail>
        <ewayCustomerAddress></ewayCustomerAddress>
        <ewayCustomerPostcode></ewayCustomerPostcode>
        <ewayCustomerInvoiceDescription></ewayCustomerInvoiceDescription>
        <ewayCustomerInvoiceRef></ewayCustomerInvoiceRef>
        <ewayCardHoldersName>{creditCard.holderName.getOrElse("")}</ewayCardHoldersName>
        <ewayCardNumber>{creditCard.number}</ewayCardNumber>
        <ewayCardExpiryMonth>{creditCard.expiration.month}</ewayCardExpiryMonth>
        <ewayCardExpiryYear>{creditCard.expiration.year % 100}</ewayCardExpiryYear>
        <ewayTrxnNumber></ewayTrxnNumber>
        <ewayOption1></ewayOption1>
        <ewayOption2></ewayOption2>
        <ewayOption3></ewayOption3>
        <ewayCVN>{creditCard.csc.getOrElse("")}</ewayCVN>
      </ewaygateway>
    val url = s"$baseUrl/${creditCard.csc.fold("gateway/xmlpayment.asp")(_ => "gateway_cvn/xmlpayment.asp")}"

    if (currencyAmount.currency != "AUD") {
      Failure(new InvalidCurrencyException(currencyAmount.currency))
    } else {
      postRequest(requestXml, url)
    }
  }

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    val merchant = merchantParser.parse(merchantKey)
    val authorization = authorizationParser.parse(authorizationKey)

    val requestXml =
      <ewaygateway>
        <ewayCustomerID>{merchant.customerId}</ewayCustomerID>
        <ewayAuthTrxnNumber>{authorization.transactionNum}</ewayAuthTrxnNumber>
        <ewayTotalAmount>{authorization.amount}</ewayTotalAmount>
        <ewayOption1></ewayOption1>
        <ewayOption2></ewayOption2>
        <ewayOption3></ewayOption3>
      </ewaygateway>

    postRequest(requestXml, s"$baseUrl/gateway/xmlauthvoid.asp")
  }
}


/** An Extractor Object usable for HTTP responses to extract the XML in the body.
  *
  * An Extractor Object is an object that has a method(s) called {{{unapply}}} as one of its members. The purpose
  * of that {{{unapply}}} method is to match a value and take it apart.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object HasXmlResponse {
  def unapply(response: HttpResponse): Option[Elem] = {
    Try(XML.loadString(response.entity.asString)).toOption
  }
}

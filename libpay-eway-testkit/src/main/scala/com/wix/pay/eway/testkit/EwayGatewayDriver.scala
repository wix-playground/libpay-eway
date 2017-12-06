/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2015, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.eway.testkit


import scala.xml.{Elem, Node, Utility, XML}
import akka.http.scaladsl.model.StatusCodes.ServerError
import akka.http.scaladsl.model._
import com.wix.e2e.http.api.StubWebServer
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.e2e.http.server.WebServerFactory._
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.eway.model.Conversions._
import com.wix.pay.eway.model.EwayMerchant
import com.wix.pay.eway.model.parsers.{JsonEwayAuthorizationParser, JsonEwayMerchantParser}
import com.wix.pay.model.{CurrencyAmount, Customer}


/** This class is a driver for eWay gateway tests, introducing a higher lever language for stubbing requests for eWay
  * gateway Http Prob.
  */
class EwayGatewayDriver(port: Int) {
  private val server: StubWebServer = aStubWebServer.onPort(port).build

  val merchantParser = new JsonEwayMerchantParser()
  val transactionParser = new JsonEwayAuthorizationParser()

  def start(): Unit = server.start
  def stop(): Unit = server.stop
  def reset(): Unit = server.replaceWith()

  def anAuthorizeRequestFor(merchantKey: Option[String] = None,
                            currencyAmount: Option[CurrencyAmount] = None,
                            creditCard: Option[CreditCard] = None,
                            customer: Option[Customer] = None): AuthorizeCtx = {
    new AuthorizeCtx(merchantKey, currencyAmount, creditCard, customer)
  }

  def aCaptureRequestFor(merchantKey: Option[String] = None,
                         refAuthorizationKey: Option[String] = None,
                         amount: Option[Double] = None): CaptureCtx = {
    new CaptureCtx(merchantKey, amount.map(CurrencyAmount("AUD", _)), refAuthorizationKey)
  }

  def aSaleRequestFor(merchantKey: Option[String] = None,
                      currencyAmount: Option[CurrencyAmount] = None,
                      creditCard: Option[CreditCard] = None,
                      customer: Option[Customer] = None): SaleCtx = {
    new SaleCtx(merchantKey, currencyAmount, creditCard, customer)
  }

  def anVoidAuthorizationRequestFor(merchantKey: Option[String] = None,
                                    refAuthorizationKey: Option[String] = None): VoidAuthorizationCtx = {
    new VoidAuthorizationCtx(merchantKey, refAuthorizationKey)
  }

  abstract class Ctx(path: String,
                     protected val merchant: Option[EwayMerchant]) {
    protected val response: (Option[(String, String)], Option[String]) => Node = (error, transactionId) => {
      val errorCode = error.fold("00")(_._1)
      val errorMessage = error.fold("Transaction Approved")(_._2)
      val status = error.fold("True")(_ => "False")

      <ewayResponse>
        <ewayTrxnError>{errorCode},{errorMessage}</ewayTrxnError>
        <ewayTrxnStatus>{status}</ewayTrxnStatus>
        <ewayTrxnNumber>{transactionId.fold("")(id => id)}</ewayTrxnNumber>
        <ewayTrxnOption1>optional 1</ewayTrxnOption1>
        <ewayTrxnOption2>optional 2</ewayTrxnOption2>
        <ewayTrxnOption3>optional 3</ewayTrxnOption3>
        <ewayReturnAmount></ewayReturnAmount>
        <ewayAuthCode>123456</ewayAuthCode>
        <ewayTrxnReference>12345678</ewayTrxnReference>
      </ewayResponse>
    }

    private val validResponse: String => Node = transactionKey =>
      response(None, Option(transactionKey))
    private val invalidResponse: (String, String) => Node = (errorCode, errorMessage) =>
      response(Option(errorCode, errorMessage), None)

    def returns(transactionKey: String): Unit = {
      server.replaceWith {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path(`path`),
        _,
        entity,
        _) if isStubbed(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = Utility.serialize(validResponse(transactionKey)).toString())
      }
    }

    def rejects(errorCode: String, errorMessage: String): Unit = {
      server.replaceWith {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path(`path`),
        _,
        entity,
        _) if isStubbed(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = Utility.serialize(invalidResponse(errorCode, errorMessage)).toString())
      }
    }

    def errors(httpStatus: ServerError): Unit = {
      server.replaceWith {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path(`path`),
        _,
        entity,
        _) if isStubbed(entity) =>
          HttpResponse(
            status = httpStatus)
      }
    }

    protected val mandatoryTags: Seq[String]
    protected val isStubbedEntity: Elem => Boolean

    private def isStubbed(entity: HttpEntity): Boolean = {
      val requestXml = XML.loadString(entity.extractAsString)
      val ewayCustomerID = (requestXml \ "ewayCustomerID").text

      merchant.fold(true)(_.customerId == ewayCustomerID) && isStubbedEntity(requestXml) &&
        (mandatoryTags forall (tag => (requestXml \ tag).nonEmpty))
    }
  }


  class AuthorizeCtx(merchantKey: Option[String],
                     currencyAmount: Option[CurrencyAmount],
                     creditCard: Option[CreditCard],
                     customer: Option[Customer]) extends Ctx(
      creditCard.fold("/gateway/xmlauth.asp") { cc =>
        cc.csc.fold("/gateway/xmlauth.asp")(_ => "/gateway_cvn/xmlauth.asp") },
      merchantKey map merchantParser.parse) {

    override protected val response: (Option[(String, String)], Option[String]) => Node = (error, authorizationKey) => {
      val errorCode = error.fold("00")(_._1)
      val errorMessage = error.fold("Transaction Approved")(_._2)
      val status = error.fold("True")(_ => "False")
      val trxn = authorizationKey map new JsonEwayAuthorizationParser().parse

      <ewayResponse>
        <ewayTrxnError>{errorCode},{errorMessage}</ewayTrxnError>
        <ewayTrxnStatus>{status}</ewayTrxnStatus>
        <ewayTrxnNumber>{trxn.fold("")(_.transactionNum)}</ewayTrxnNumber>
        <ewayTrxnOption1>optional 1</ewayTrxnOption1>
        <ewayTrxnOption2>optional 2</ewayTrxnOption2>
        <ewayTrxnOption3>optional 3</ewayTrxnOption3>
        <ewayReturnAmount>{trxn.fold("")(_.amount.toString)}</ewayReturnAmount>
        <ewayAuthCode>123456</ewayAuthCode>
        <ewayTrxnReference>12345678</ewayTrxnReference>
        <ewayCVN>{creditCard.map(_.csc.getOrElse(""))}</ewayCVN>
      </ewayResponse>
    }

    override protected val isStubbedEntity: Elem => Boolean = requestXml => {
      val cardHolderName = (requestXml \ "ewayCardHoldersName").text
      val cardNumber = (requestXml \ "ewayCardNumber").text
      val expiryMonth = (requestXml \ "ewayCardExpiryMonth").text.toInt
      val expiryYear = (requestXml \ "ewayCardExpiryYear").text.toInt
      val csc = (requestXml \ "ewayCVN").text
      val customerFirstName = (requestXml \ "ewayCustomerFirstName").text
      val customerLastName = (requestXml \ "ewayCustomerLastName").text
      val customerEmail = (requestXml \ "ewayCustomerEmail").text
      val totalAmount = (requestXml \ "ewayTotalAmount").text.toInt

      currencyAmount.fold(true) { currAmount =>
        toEwayAmount(currAmount.amount) == totalAmount && currAmount.currency == "AUD"
      } && creditCard.fold(true) { card =>
        card.holderName.fold(true)(_ == cardHolderName) &&
        card.number == cardNumber &&
        card.expiration.month == expiryMonth &&
        card.expiration.year % 100 == expiryYear &&
        card.csc.getOrElse("") == csc
      } &&
      customer.fold(true) { cust =>
        cust.name.fold(true) { name =>
          name.first == customerFirstName &&
          name.last == customerLastName } &&
        cust.email.fold(true)(_ == customerEmail)
      }
    }

    override protected val mandatoryTags =
      Seq(
        "ewayCustomerID",
        "ewayTotalAmount",
        "ewayCustomerFirstName",
        "ewayCustomerLastName",
        "ewayCustomerEmail",
        "ewayCustomerAddress",
        "ewayCustomerPostcode",
        "ewayCustomerInvoiceDescription",
        "ewayCustomerInvoiceRef",
        "ewayCardHoldersName",
        "ewayCardNumber",
        "ewayCardExpiryMonth",
        "ewayCardExpiryYear",
        "ewayTrxnNumber",
        "ewayOption1",
        "ewayOption2",
        "ewayOption3",
        "ewayCVN")
  }


  class CaptureCtx(merchantKey: Option[String],
                   currencyAmount: Option[CurrencyAmount],
                   refAuthorizationKey: Option[String]) extends Ctx(
      "/gateway/xmlauthcomplete.asp",
      merchantKey map merchantParser.parse) {

    override protected val isStubbedEntity: Elem => Boolean = requestXml => {
      val authorization = refAuthorizationKey map transactionParser.parse
      val authorizeTransactionNum = (requestXml \ "ewayAuthTrxnNumber").text
      val totalAmount = (requestXml \ "ewayTotalAmount").text.toInt

      currencyAmount.fold(true) { currAmount =>
        toEwayAmount(currAmount.amount) == totalAmount && currAmount.currency == "AUD"
      } && authorization.fold(true)(_.transactionNum == authorizeTransactionNum)
    }

    override protected val mandatoryTags =
      Seq(
        "ewayCustomerID",
        "ewayTotalAmount",
        "ewayAuthTrxnNumber",
        "ewayCardExpiryMonth",
        "ewayCardExpiryYear",
        "ewayOption1",
        "ewayOption2",
        "ewayOption3")
  }

  class SaleCtx(merchantKey: Option[String],
                currencyAmount: Option[CurrencyAmount],
                creditCard: Option[CreditCard],
                customer: Option[Customer]) extends Ctx(
      creditCard.fold("/gateway/xmlpayment.asp"){ cc =>
        cc.csc.fold("/gateway/xmlpayment.asp")(_ => "/gateway_cvn/xmlpayment.asp") },
      merchantKey map merchantParser.parse) {
    override protected val response: (Option[(String, String)], Option[String]) => Node = (error, transactionId) => {
      val errorCode = error.fold("00")(_._1)
      val errorMessage = error.fold("Transaction Approved")(_._2)
      val status = error.fold("True")(_ => "False")

      <ewayResponse>
        <ewayTrxnError>{errorCode},{errorMessage}</ewayTrxnError>
        <ewayTrxnStatus>{status}</ewayTrxnStatus>
        <ewayTrxnNumber>{transactionId.fold("")(id => id)}</ewayTrxnNumber>
        <ewayTrxnOption1>optional 1</ewayTrxnOption1>
        <ewayTrxnOption2>optional 2</ewayTrxnOption2>
        <ewayTrxnOption3>optional 3</ewayTrxnOption3>
        <ewayReturnAmount></ewayReturnAmount>
        <ewayAuthCode>123456</ewayAuthCode>
        <ewayTrxnReference>12345678</ewayTrxnReference>
        <ewayCVN>{creditCard.map(_.csc.getOrElse(""))}</ewayCVN>
      </ewayResponse>
    }

    override protected val isStubbedEntity: Elem => Boolean = requestXml => {
      val cardHolderName = (requestXml \ "ewayCardHoldersName").text
      val cardNumber = (requestXml \ "ewayCardNumber").text
      val expiryMonth = (requestXml \ "ewayCardExpiryMonth").text.toInt
      val expiryYear = (requestXml \ "ewayCardExpiryYear").text.toInt
      val csc = (requestXml \ "ewayCVN").text
      val customerFirstName = (requestXml \ "ewayCustomerFirstName").text
      val customerLastName = (requestXml \ "ewayCustomerLastName").text
      val customerEmail = (requestXml \ "ewayCustomerEmail").text
      val totalAmount = (requestXml \ "ewayTotalAmount").text.toInt

      currencyAmount.fold(true) { currAmount =>
        toEwayAmount(currAmount.amount) == totalAmount && currAmount.currency == "AUD"
      } && creditCard.fold(true) { card =>
        card.holderName.fold(true)(_ == cardHolderName) &&
        card.number == cardNumber &&
        card.expiration.month == expiryMonth &&
        card.expiration.year % 100 == expiryYear &&
        card.csc.getOrElse("") == csc
      } &&
      customer.fold(true) { cust =>
        cust.name.fold(true) { name =>
          name.first == customerFirstName &&
          name.last == customerLastName } &&
        cust.email.fold(true)(_ == customerEmail)
      }
    }

    override protected val mandatoryTags =
      Seq(
        "ewayCustomerID",
        "ewayTotalAmount",
        "ewayCustomerFirstName",
        "ewayCustomerLastName",
        "ewayCustomerEmail",
        "ewayCustomerAddress",
        "ewayCustomerPostcode",
        "ewayCustomerInvoiceDescription",
        "ewayCustomerInvoiceRef",
        "ewayCardHoldersName",
        "ewayCardNumber",
        "ewayCardExpiryMonth",
        "ewayCardExpiryYear",
        "ewayTrxnNumber",
        "ewayOption1",
        "ewayOption2",
        "ewayOption3",
        "ewayCVN")
  }


  class VoidAuthorizationCtx(merchantKey: Option[String],
                             refAuthorizationKey: Option[String]) extends Ctx(
      "/gateway/xmlauthvoid.asp",
      merchantKey map merchantParser.parse) {

    override protected val isStubbedEntity: Elem => Boolean = requestXml => {
      val refAuthorization = refAuthorizationKey map transactionParser.parse
      val authorizeTransactionNum = (requestXml \ "ewayAuthTrxnNumber").text

      refAuthorization.fold(true)(_.transactionNum == authorizeTransactionNum)
    }

    override protected val mandatoryTags =
      Seq(
        "ewayCustomerID",
        "ewayTotalAmount",
        "ewayAuthTrxnNumber",
        "ewayOption1",
        "ewayOption2",
        "ewayOption3")
  }
}

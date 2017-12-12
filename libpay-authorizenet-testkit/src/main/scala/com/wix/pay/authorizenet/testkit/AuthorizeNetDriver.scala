/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2014, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.authorizenet.testkit


import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model._
import net.authorize.TransactionType
import com.wix.e2e.http.api.StubWebServer
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.e2e.http.server.WebServerFactory._
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.CurrencyAmount


/** This class is a driver for Authorize.Net gateway tests, introducing a higher lever language for stubbing requests
  * for Authorize.Net gateway Http Prob.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class AuthorizeNetDriver(server: StubWebServer) {

  def this(port: Int) = this(aStubWebServer.onPort(port).build)

  /** Starts Authorize.Net gateway HTTP Driver.
    * Should be called before the IT tests of Authorize.Net gateway starts
    */
  def start(): Unit = server.start()

  /** Stops Authorize.Net gateway HTTP Driver.
    * Should be called after all IT tests of Authorize.Net gateway had completed.
    */
  def stop(): Unit = server.stop()

  def reset(): Unit = server.replaceWith()


  /** Encapsulates the details of an Authorize request.
    * Needs to be followed be [[AuthorizeCtx]]'s ''returns'' ''errors'' methods for actual stubbing.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param currencyAmount
    *                       The amount and currency of the Authorize operation.
    * @param creditCard
    *                   The credit card used for the Authorize operation.
    * @return
    *         a context for later stubbing
    */
  def anAuthorizeRequestFor(loginId: Option[String] = None,
                            transactionKey: Option[String] = None,
                            currencyAmount: Option[CurrencyAmount],
                            creditCard: Option[CreditCard]): AuthorizeCtx = {
    new AuthorizeCtx(loginId, transactionKey, currencyAmount, creditCard)
  }

  /** Encapsulates the details of a Capture request.
    * Needs to be followed be [[CaptureCtx]]'s ''returns'' ''errors'' methods for actual stubbing.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param refTransactionKey
    *                          The transaction Key of the Authorize operation, now to be captured.
    * @param amount
    *               The amount to capture.
    * @return
    *         a context for later stubbing
    */
  def aCaptureRequestFor(loginId: Option[String] = None,
                         transactionKey: Option[String] = None,
                         refTransactionKey: Option[String] = None,
                         amount: Option[Double] = None): CaptureCtx = {
    new CaptureCtx(loginId, transactionKey, refTransactionKey, amount)
  }

  /** Encapsulates the details of a Sale request.
    * Needs to be followed be [[SaleCtx]]'s ''returns'' ''errors'' methods for actual stubbing.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param currencyAmount
    *                       The amount and currency of the Sale operation.
    * @param creditCard
    *                   The credit card used for the Sale operation.
    * @return
    *         a context for later stubbing
    */
  def aSaleRequestFor(loginId: Option[String] = None,
                      transactionKey: Option[String] = None,
                      currencyAmount: Option[CurrencyAmount],
                      creditCard: Option[CreditCard]): SaleCtx = {
    new SaleCtx(loginId, transactionKey, currencyAmount, creditCard)
  }

  /** Encapsulates the details of a Refund request.
    * Needs to be followed be [[RefundCtx]]'s ''returns'' ''errors'' methods for actual stubbing.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param refTransactionKey
    *                          The transaction Key of the settled transaction.
    * @param amount
    *               The amount to refund (less or equals the original transaction).
    * @return
    *         a context for later stubbing
    */
  def aRefundRequestFor(loginId: Option[String] = None,
                        transactionKey: Option[String] = None,
                        refTransactionKey: Option[String] = None,
                        creditCard: Option[CreditCard] = None,
                        amount: Option[Double] = None): RefundCtx = {
    new RefundCtx(loginId, transactionKey, refTransactionKey, creditCard, amount)
  }

  /** Encapsulates the details of a Void request.
    * Needs to be followed be [[VoidCtx]]'s ''returns'' ''errors'' methods for actual stubbing.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param refTransactionKey
    *                          The transaction Key of the authorized transaction.
    * @return
    *         a context for later stubbing
    */
  def aVoidRequestFor(loginId: Option[String] = None,
                      transactionKey: Option[String] = None,
                      refTransactionKey: Option[String] = None): VoidCtx = {
    new VoidCtx(loginId, transactionKey: Option[String], refTransactionKey)
  }

  /** Encapsulates the details of a Void-Authorization request.
    * Needs to be followed be [[VoidCtx]]'s ''returns'' ''errors'' methods for actual stubbing.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param authorizationKey
    *                          The Authorization Key.
    * @return
    *         a context for later stubbing
    */
  def aVoidAuthorizationRequestFor(loginId: Option[String] = None,
                                   transactionKey: Option[String] = None,
                                   authorizationKey: Option[String] = None): VoidAuthorizationCtx = {
    new VoidAuthorizationCtx(loginId, transactionKey: Option[String], authorizationKey)
  }



  /** Abstract context, encapsulating the merchant credentials, and defining the stubbing operations.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    */
  abstract class Ctx(loginId: Option[String],
                     transactionKey: Option[String] = None) {
    val responseCodeOK = 1
    val responseReasonCodeOK = 1
    val responseReasonTextOK = "This transaction has been approved."


    /** The response string returned from Authorize.Net gateway. */
    def response(responseCode: Int,
                 responseSubCode: String = "",
                 responseReasonCode: Int,
                 responseReasonText: String,
                 authorizationCode: String = "",
                 avsCode: String = "Y",
                 transactionId: String,
                 invoiceNum: String = "",
                 desc: String = "",
                 amount: Option[Double] = None,
                 method: String = "CC",
                 transactionType: TransactionType,
                 customerId: String = "",
                 firstName: String = "",
                 lastName: String = "",
                 company: String = "",
                 address: String = "",
                 city: String = "",
                 state: String = "",
                 zipCode: String = "",
                 country: String = "",
                 phone: String = "",
                 fax: String = "",
                 email: String = "",
                 shipToFirstName: String = "",
                 shipToLastName: String = "",
                 shipToCompany: String = "",
                 shipToAddress: String = "",
                 shipToCity: String = "",
                 shipToState: String = "",
                 shipToZipCode: String = "",
                 shipToCountry: String = "",
                 tax: String = "",
                 duty: String = "",
                 freight: String = "",
                 taxExempt: String = "",
                 purchaseOrderNum: String = "",
                 md5hash: String = "",
                 cardCodeResponse: String = "P",
                 cardHolderAuthenticationVerificationResponse: String = "2",
                 accountNum: String = "",
                 cardType: String = "",
                 splitTenderId: String = "",
                 prepaidRequestAmount: String = "",
                 prepaidBalanceOnCard: String = ""): String = {
      val maskedAccountNum = accountNum.takeRight(4) match {
        case masked if masked.length < 3 => ""
        case masked => s"XXXX$masked"
      }
      val formattedAmount = amount.fold("")(a => f"$a%.2f")
      val transType = transactionType.toString.toLowerCase

      s"$responseCode|$responseSubCode|$responseReasonCode|$responseReasonText|$authorizationCode|$avsCode|$transactionId|$invoiceNum|$desc|$formattedAmount|$method|$transType|$customerId|$firstName|$lastName|$company|$address|$city|$state|$zipCode|$country|$phone|$fax|$email|$shipToFirstName|$shipToLastName|$shipToCompany|$shipToAddress|$shipToCity|$shipToState|$shipToZipCode|$shipToCountry|$tax|$duty|$freight|$taxExempt|$purchaseOrderNum|$md5hash|$cardCodeResponse|$cardHolderAuthenticationVerificationResponse|||||||||||$maskedAccountNum|$cardType|$splitTenderId|$prepaidRequestAmount|$prepaidBalanceOnCard||||||||||||||"
    }

    /** Verifies that the specified HTTP Entity matches the stubbed request. */
    val isStubbedRequestEntity: HttpEntity => Boolean = entity => {
      val valuesMap = entity.extractAsString.split('&')
        .map {_.split('=')}
        .map {keyValue => keyValue(0) -> (if (keyValue.length > 1) keyValue(1) else "")}
        .toMap

      loginId.fold(true)(_ => valuesMap.get("x_login") == loginId) &&
        transactionKey.fold(true)(_ => valuesMap.get("x_tran_key") == transactionKey) &&
        isRequestEntity(valuesMap, entity)
    }


    /** Verifies that the specified HTTP Entity matches the stubbed request.
      * Must be overridden by the concrete context classes, for specific verifications.
      */
    def isRequestEntity(valuesMap: Map[String, String], entity: HttpEntity): Boolean

    /** The transaction key returned for the request.
      * Must be overridden by the concrete context classes, for specific response.
      */
    def returns(transactionKey: String)

    /** The error encountered while processing the request.
      * Must be overridden by the concrete context classes, for specific error.
      */
    def errors(responseCode: Int,
               responseReasonCode: Int,
               responseReasonText: String)
  }


  /** Context for the Authorize operation.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param currencyAmount
    *                       The amount and currency of the Sale operation.
    * @param creditCard
    *                   The credit card used for the Sale operation.
    */
  class AuthorizeCtx(loginId: Option[String],
                     transactionKey: Option[String] = None,
                     currencyAmount: Option[CurrencyAmount],
                     creditCard: Option[CreditCard]) extends Ctx(loginId, transactionKey) {

    override def returns(authorizationId: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCodeOK,
                  responseReasonCode = responseReasonCodeOK,
                  responseReasonText = responseReasonTextOK,
                  authorizationCode = loginId.getOrElse(""),
                  transactionId = authorizationId,
                  amount = currencyAmount.map(_.amount),
                  transactionType = TransactionType.AUTH_ONLY,
                  accountNum = creditCard.fold("")(_.number))))
      }
    }

    override def errors(responseCode: Int,
                        responseReasonCode: Int,
                        responseReasonText: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCode,
                  responseReasonCode = responseReasonCode,
                  responseReasonText = responseReasonText,
                  authorizationCode = loginId.getOrElse(""),
                  transactionId = "",
                  transactionType = TransactionType.AUTH_ONLY)))
      }
    }

    override def isRequestEntity(valuesMap: Map[String, String], entity: HttpEntity): Boolean = {
      valuesMap.get("x_duplicate_window").fold(false)(_ == "0") &&
        valuesMap.get("x_type").fold(false)(_ == "AUTH_ONLY") &&
        currencyAmount.fold(true)(ca =>
          valuesMap.get("x_currency_code") == Option(ca.currency) &&
            valuesMap.get("x_amount") == Option(f"${ca.amount}%.2f")) &&
        creditCard.fold(true)(cc =>
          valuesMap.get("x_method") == Option("CC") &&
            valuesMap.get("x_card_num") == Option(cc.number) &&
            valuesMap.get("x_exp_date") == Option(f"${cc.expiration.month}%02d${cc.expiration.year}") &&
            valuesMap.get("x_card_code") == cc.csc)
    }
  }


  /** Context for the Capture operation.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param refTransactionKey
    *                          The transaction Key of the Authorize operation, now to be captured.
    * @param amount
    *               The amount to capture.
    */
  class CaptureCtx(loginId: Option[String],
                   transactionKey: Option[String] = None,
                   refTransactionKey: Option[String],
                   amount: Option[Double]) extends Ctx(loginId, transactionKey) {

    override def returns(transactionId: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCodeOK,
                  responseReasonCode = responseReasonCodeOK,
                  responseReasonText = responseReasonTextOK,
                  authorizationCode = refTransactionKey.getOrElse(""),
                  transactionId = transactionId,
                  amount = amount,
                  transactionType = TransactionType.PRIOR_AUTH_CAPTURE)))
      }
    }

    override def errors(responseCode: Int,
                        responseReasonCode: Int,
                        responseReasonText: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCode,
                  responseReasonCode = responseReasonCode,
                  responseReasonText = responseReasonText,
                  authorizationCode = loginId.getOrElse(""),
                  transactionId = "",
                  transactionType = TransactionType.PRIOR_AUTH_CAPTURE)))
      }
    }

    override def isRequestEntity(valuesMap: Map[String, String], entity: HttpEntity): Boolean = {
      valuesMap.get("x_type").fold(false)(_ == "PRIOR_AUTH_CAPTURE") &&
        refTransactionKey.fold(true)(_ => valuesMap.get("x_trans_id") == refTransactionKey) &&
        amount.fold(true)(a => valuesMap.get("x_amount") == Option(f"$a%.2f"))
    }
  }


  /** Context for the Sale operation.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param currencyAmount
    *                       The amount and currency of the Sale operation.
    * @param creditCard
    *                   The credit card used for the Sale operation.
    */
  class SaleCtx(loginId: Option[String],
                transactionKey: Option[String] = None,
                currencyAmount: Option[CurrencyAmount],
                creditCard: Option[CreditCard]) extends Ctx(loginId, transactionKey) {

    override def returns(transactionId: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCodeOK,
                  responseReasonCode = responseReasonCodeOK,
                  responseReasonText = responseReasonTextOK,
                  authorizationCode = loginId.getOrElse(""),
                  transactionId = transactionId,
                  amount = currencyAmount.map(_.amount),
                  transactionType = TransactionType.AUTH_CAPTURE,
                  accountNum = creditCard.fold("")(_.number))))
      }
    }

    override def errors(responseCode: Int,
                        responseReasonCode: Int,
                        responseReasonText: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCode,
                  responseReasonCode = responseReasonCode,
                  responseReasonText = responseReasonText,
                  authorizationCode = loginId.getOrElse(""),
                  transactionId = "",
                  transactionType = TransactionType.AUTH_CAPTURE)))
      }
    }

    override def isRequestEntity(valuesMap: Map[String, String], entity: HttpEntity): Boolean = {
      valuesMap.get("x_duplicate_window").fold(false)(_ == "0") &&
        valuesMap.get("x_type").fold(false)(_ == "AUTH_CAPTURE") &&
        currencyAmount.fold(true)(ca =>
          valuesMap.get("x_currency_code") == Option(ca.currency) &&
            valuesMap.get("x_amount") == Option(f"${ca.amount}%.2f")) &&
        creditCard.fold(true)(cc =>
          valuesMap.get("x_method") == Option("CC") &&
            valuesMap.get("x_card_num") == Option(cc.number) &&
            valuesMap.get("x_exp_date") == Option(f"${cc.expiration.month}%02d${cc.expiration.year}") &&
            valuesMap.get("x_card_code") == cc.csc)
    }
  }


  /** Context for the Refund operation.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param refTransactionKey
    *                          The transaction Key of the settled transaction.
    * @param amount
    *               The amount to refund (less or equals the original transaction).
    */
  class RefundCtx(loginId: Option[String] = None,
                  transactionKey: Option[String] = None,
                  refTransactionKey: Option[String] = None,
                  creditCard: Option[CreditCard] = None,
                  amount: Option[Double] = None) extends Ctx(loginId, transactionKey) {

    override def returns(transactionId: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCodeOK,
                  responseReasonCode = responseReasonCodeOK,
                  responseReasonText = responseReasonTextOK,
                  transactionId = transactionId,
                  amount = amount,
                  transactionType = TransactionType.CREDIT,
                  accountNum = creditCard.fold("")(_.number))))
      }
    }

    override def errors(responseCode: Int,
                        responseReasonCode: Int,
                        responseReasonText: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCode,
                  responseReasonCode = responseReasonCode,
                  responseReasonText = responseReasonText,
                  authorizationCode = loginId.getOrElse(""),
                  transactionId = "",
                  transactionType = TransactionType.CREDIT)))
      }
    }

    override def isRequestEntity(valuesMap: Map[String, String], entity: HttpEntity): Boolean = {
      valuesMap.get("x_type").fold(false)(_ == "CREDIT") &&
        refTransactionKey.fold(true)(_ => valuesMap.get("x_trans_id") == refTransactionKey) &&
        amount.fold(true)(a => valuesMap.get("x_amount") == Option(f"$a%.2f")) &&
        creditCard.fold(true)(cc =>
          valuesMap.get("x_method") == Option("CC") &&
            valuesMap.get("x_card_num") == Option(cc.number) &&
            valuesMap.get("x_exp_date") == Option(f"${cc.expiration.month}%02d${cc.expiration.year}") &&
            valuesMap.get("x_card_code") == cc.csc)
    }
  }


  /** Context for the Void operation.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param refTransactionKey
    *                          The transaction Key of the settled transaction.
    */
  class VoidCtx(loginId: Option[String] = None,
                transactionKey: Option[String] = None,
                refTransactionKey: Option[String] = None) extends Ctx(loginId, transactionKey) {

    override def returns(transactionId: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCodeOK,
                  responseReasonCode = responseReasonCodeOK,
                  responseReasonText = responseReasonTextOK,
                  transactionId = transactionId,
                  transactionType = TransactionType.VOID)))
      }
    }

    override def errors(responseCode: Int,
                        responseReasonCode: Int,
                        responseReasonText: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCode,
                  responseReasonCode = responseReasonCode,
                  responseReasonText = responseReasonText,
                  authorizationCode = loginId.getOrElse(""),
                  transactionId = "",
                  transactionType = TransactionType.VOID)))
      }
    }

    override def isRequestEntity(valuesMap: Map[String, String], entity: HttpEntity): Boolean = {
      valuesMap.get("x_type").fold(false)(_ == "VOID") &&
        refTransactionKey.fold(true)(_ => valuesMap.get("x_trans_id") == refTransactionKey)
    }
  }


  /** Context for the Void-Auhtorization operation.
    *
    * @param loginId
    *                Authorize.Net login API ID.
    * @param transactionKey
    *                       Authorize.Net transaction key.
    * @param authorizationKey
    *                          The authorization Key.
    */
  class VoidAuthorizationCtx(loginId: Option[String] = None,
                transactionKey: Option[String] = None,
                authorizationKey: Option[String] = None) extends Ctx(loginId, transactionKey) {

    override def returns(transactionId: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCodeOK,
                  responseReasonCode = responseReasonCodeOK,
                  responseReasonText = responseReasonTextOK,
                  transactionId = transactionId,
                  transactionType = TransactionType.VOID)))
      }
    }

    override def errors(responseCode: Int,
                        responseReasonCode: Int,
                        responseReasonText: String) {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path("/gateway/transact.dll"),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                response(
                  responseCode = responseCode,
                  responseReasonCode = responseReasonCode,
                  responseReasonText = responseReasonText,
                  authorizationCode = loginId.getOrElse(""),
                  transactionId = "",
                  transactionType = TransactionType.VOID)))
      }
    }

    override def isRequestEntity(valuesMap: Map[String, String], entity: HttpEntity): Boolean = {
      valuesMap.get("x_type").fold(false)(_ == "VOID") &&
        authorizationKey.fold(true)(_ => valuesMap.get("x_trans_id") == authorizationKey)
    }
  }
}

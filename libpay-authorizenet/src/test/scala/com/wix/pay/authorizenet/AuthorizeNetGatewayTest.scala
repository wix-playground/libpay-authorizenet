/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2014, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.authorizenet


import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.{CurrencyAmount, Payment}
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import net.authorize.aim.{Result, Transaction}
import net.authorize.{Environment, ResponseCode, ResponseField, Merchant => AnetMetchant}
import org.specs2.matcher._
import org.specs2.mock.Mockito
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

import scala.collection.JavaConversions._


/** The Unit Test class for the [[AuthorizeNetGateway]] class.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class AuthorizeNetGatewayTest extends SpecWithJUnit with Mockito {
  trait Ctx extends Scope {
    val authorizationParser = new JsonAuthorizenetAuthorizationParser
    val helper = mock[AuthorizeNetGetewayHelper]
    val environment = Environment.SANDBOX
    val authorizeNetGateway = new AuthorizeNetGateway(environment, helper, authorizationParser)

    val someMerchantKey = "merchant key"
    val someAnetMerchant = AnetMetchant.createMerchant(null, null, null)
    val someCurrencyAmount = CurrencyAmount("USD", 33.3)
    val somePayment = Payment(someCurrencyAmount, 1)
    val someCreditCard = CreditCard(
      "4012888818888",
      YearMonth(2016, 12),
      Some(CreditCardOptionalFields(
        csc = Some("123"))))
    val someTransaction = someAnetMerchant.createAIMTransaction(null, null)

    val acceptedResponseCode = ResponseCode.APPROVED.getCode
    val rejectResponseCode = ResponseCode.DECLINED.getCode
    val reviewResponseCode = ResponseCode.REVIEW.getCode
    val errorResponseCode = ResponseCode.ERROR.getCode

    def someCreatedAnetMerchant: AnetMetchant = {
      helper.createMerchant(environment, someMerchantKey) returns someAnetMerchant

      someAnetMerchant
    }

    def someAuthorizeTransaction: Transaction = {
      helper.createAuthorizeOnlyTransaction(someAnetMerchant, someCurrencyAmount, someCreditCard,None, None) returns
        someTransaction

      someTransaction
    }

    def someCaptureTransaction(transactionKey: String, amount: Double): Transaction = {
      helper.createCaptureTransaction(someAnetMerchant, transactionKey, amount) returns someTransaction

      someTransaction
    }

    def someSaleTransaction: Transaction = {
      helper.createAuthorizeCaptureTransaction(someAnetMerchant, someCurrencyAmount, someCreditCard, None, None) returns
        someTransaction

      someTransaction
    }
    
    def someRefundTransaction(transactionId: String): Transaction = {
      helper.createCreditTransaction(
        someAnetMerchant,
        transactionId,
        someCreditCard,
        someCurrencyAmount.amount) returns someTransaction

      someTransaction
    }

    def someVoidTransaction(transactionId: String): Transaction = {
      helper.createVoidTransaction(
        someAnetMerchant,
        transactionId) returns someTransaction

      someTransaction
    }

    val validateHelperFlow: (Unit => Transaction) => MatchResult[Result[Transaction]] = f => {
      got {
        one(helper).createMerchant(environment, someMerchantKey)
        f.apply(Unit)
        one(helper).postTransaction(someAnetMerchant, someTransaction)
      }
    }

    val anApprovedTransactionResult: String => Result[Transaction] = transactionId => Result.createResult(
      someTransaction,
      Map(
        ResponseField.RESPONSE_CODE -> acceptedResponseCode.toString,
        ResponseField.TRANSACTION_ID -> transactionId))

    val aFailedTransactionResult: (String, String) => Result[Transaction] = (rejectReasonCode, rejectReasonText) =>
      Result.createResult(
        someTransaction,
        Map(
          ResponseField.RESPONSE_CODE -> rejectResponseCode.toString,
          ResponseField.RESPONSE_REASON_CODE -> rejectReasonCode,
          ResponseField.RESPONSE_REASON_TEXT -> rejectReasonText))
    val aReviewTransactionResult: (String, String) => Result[Transaction] = (reviewReasonCode, reviewReasonText) =>
      Result.createResult(
        someTransaction,
        Map(
          ResponseField.RESPONSE_CODE -> reviewResponseCode.toString,
          ResponseField.RESPONSE_REASON_CODE -> reviewReasonCode,
          ResponseField.RESPONSE_REASON_TEXT -> reviewReasonText))
    val anErrorTransactionResult: (String, String) => Result[Transaction] = (errorReasonCode, errorReasonText) =>
      Result.createResult(
        someTransaction,
        Map(
          ResponseField.RESPONSE_CODE -> errorResponseCode.toString,
          ResponseField.RESPONSE_REASON_CODE -> errorReasonCode,
          ResponseField.RESPONSE_REASON_TEXT -> errorReasonText))
  }


  "authorize method" should {
    "create an Autorize.Net merchant and authorize only transaction, post it and approve it if all OK" in new Ctx {
      val someTransactionId = "transaction ID"
      val someAuthorizationKey = authorizationParser.stringify(AuthorizenetAuthorization(someTransactionId))

      helper.postTransaction(someCreatedAnetMerchant, someAuthorizeTransaction) returns
        anApprovedTransactionResult(someTransactionId)

      authorizeNetGateway.authorize(someMerchantKey, someCreditCard, somePayment) must
        beASuccessfulTry(
          check = ===(someAuthorizationKey)
        )

      validateHelperFlow(_ => one(helper)
        .createAuthorizeOnlyTransaction(someAnetMerchant, someCurrencyAmount, someCreditCard, None,None))
    }

    "throw a rejected exception if transaction is declined" in new Ctx {
      val someRejectReasonCode = "2"
      val someRejectReasonText = "rejected because"

      helper.postTransaction(someCreatedAnetMerchant, someAuthorizeTransaction) returns
        aFailedTransactionResult(someRejectReasonCode, someRejectReasonText)

      authorizeNetGateway.authorize(someMerchantKey, someCreditCard, somePayment) must
        beAFailedTry.like {
          case e: PaymentRejectedException => e.message must beEqualTo(s"response code: RRC_${someRejectReasonCode}_$rejectResponseCode, response text: $someRejectReasonText")
        }

      validateHelperFlow(_ => one(helper)
        .createAuthorizeOnlyTransaction(someAnetMerchant, someCurrencyAmount, someCreditCard, None, None))
    }

    "throw a rejected exception if transaction is review" in new Ctx {
      val someReviewReasonCode = "253"
      val someReviewReasonText = "review because"

      helper.postTransaction(someCreatedAnetMerchant, someAuthorizeTransaction) returns
        aReviewTransactionResult(someReviewReasonCode, someReviewReasonText)

      authorizeNetGateway.authorize(someMerchantKey, someCreditCard, somePayment) must
        beAFailedTry.like {
          case e: PaymentRejectedException => e.message must beEqualTo(s"response code: RRC_${reviewResponseCode}_$someReviewReasonCode, response text: $someReviewReasonText")
        }

      validateHelperFlow(_ => one(helper)
        .createAuthorizeOnlyTransaction(someAnetMerchant, someCurrencyAmount, someCreditCard, None, None))
    }

    "throw an error exception if transaction is error" in new Ctx {
      val someErrorReasonCode = "6"
      val someErrorReasonText = "error because"

      helper.postTransaction(someCreatedAnetMerchant, someAuthorizeTransaction) returns
        anErrorTransactionResult(someErrorReasonCode, someErrorReasonText)

      authorizeNetGateway.authorize(someMerchantKey, someCreditCard, somePayment) must
        beAFailedTry.like {
          case e: PaymentErrorException => e.message must beEqualTo(s"response code: RRC_${errorResponseCode}_$someErrorReasonCode, response text: $someErrorReasonText")
        }

      validateHelperFlow(_ => one(helper)
        .createAuthorizeOnlyTransaction(someAnetMerchant, someCurrencyAmount, someCreditCard, None, None))
    }
  }
  
  "capture method" should {
    "create an Autorize.Net merchant and capture transaction, post it and approve it if all OK" in new Ctx {
      val someTransactionId = "333"
      val someAuthorizationKey = authorizationParser.stringify(AuthorizenetAuthorization(someTransactionId))

      helper.postTransaction(
        someCreatedAnetMerchant,
        someCaptureTransaction(someTransactionId, someCurrencyAmount.amount)) returns
          anApprovedTransactionResult(someTransactionId)

      authorizeNetGateway.capture(someMerchantKey, someAuthorizationKey, someCurrencyAmount.amount) must
        beASuccessfulTry(
          check = ===(someTransactionId)
        )

      validateHelperFlow(_ => one(helper)
        .createCaptureTransaction(someAnetMerchant, someTransactionId, someCurrencyAmount.amount))
    }

    "throw a rejected exception if transaction is declined" in new Ctx {
      val someTransactionId = "333"
      val someAuthorizationKey = authorizationParser.stringify(AuthorizenetAuthorization(someTransactionId))
      val someRejectReasonCode = "2"
      val someRejectReasonText = "rejected because"

      helper.postTransaction(
        someCreatedAnetMerchant,
        someCaptureTransaction(someTransactionId, someCurrencyAmount.amount)) returns
          aFailedTransactionResult(someRejectReasonCode, someRejectReasonText)

      authorizeNetGateway.capture(someMerchantKey, someAuthorizationKey, someCurrencyAmount.amount) must
        beAFailedTry.like {
          case e: PaymentRejectedException => e.message must beEqualTo(s"response code: RRC_${someRejectReasonCode}_$rejectResponseCode, response text: $someRejectReasonText")
        }

      validateHelperFlow(_ => one(helper)
        .createCaptureTransaction(someAnetMerchant, someTransactionId, someCurrencyAmount.amount))
    }

    "throw a rejected exception if transaction is review" in new Ctx {
      val someTransactionId = "333"
      val someAuthorizationKey = authorizationParser.stringify(AuthorizenetAuthorization(someTransactionId))
      val someReviewReasonCode = "253"
      val someReviewReasonText = "review because"

      helper.postTransaction(
        someCreatedAnetMerchant,
        someCaptureTransaction(someTransactionId, someCurrencyAmount.amount)) returns
          aReviewTransactionResult(someReviewReasonCode, someReviewReasonText)

      authorizeNetGateway.capture(someMerchantKey, someAuthorizationKey, someCurrencyAmount.amount) must
        beAFailedTry.like {
          case e: PaymentRejectedException => e.message must beEqualTo(s"response code: RRC_${reviewResponseCode}_$someReviewReasonCode, response text: $someReviewReasonText")
        }

      validateHelperFlow(_ => one(helper)
        .createCaptureTransaction(someAnetMerchant, someTransactionId, someCurrencyAmount.amount))
    }

    "throw an error exception if transaction is error" in new Ctx {
      val someTransactionId = "333"
      val someAuthorizationKey = authorizationParser.stringify(AuthorizenetAuthorization(someTransactionId))
      val someErrorReasonCode = "6"
      val someErrorReasonText = "error because"

      helper.postTransaction(
        someCreatedAnetMerchant,
        someCaptureTransaction(someTransactionId, someCurrencyAmount.amount)) returns
          anErrorTransactionResult(someErrorReasonCode, someErrorReasonText)

      authorizeNetGateway.capture(someMerchantKey, someAuthorizationKey, someCurrencyAmount.amount) must
        beAFailedTry.like {
          case e: PaymentErrorException => e.message must beEqualTo(s"response code: RRC_${errorResponseCode}_$someErrorReasonCode, response text: $someErrorReasonText")
        }

      validateHelperFlow(_ => one(helper)
        .createCaptureTransaction(someAnetMerchant, someTransactionId, someCurrencyAmount.amount))
    }
  }

  "sale method" should {
    "create an Autorize.Net merchant and authorize-capture transaction, post it and approve it if all OK" in new Ctx {
      val someTransactionId = "transaction ID"

      helper.postTransaction(someCreatedAnetMerchant, someSaleTransaction) returns
        anApprovedTransactionResult(someTransactionId)

      authorizeNetGateway.sale(someMerchantKey, someCreditCard, somePayment) must
        beASuccessfulTry(check = ===(someTransactionId))

      validateHelperFlow(_ => one(helper)
        .createAuthorizeCaptureTransaction(someAnetMerchant, someCurrencyAmount, someCreditCard, None, None))
    }

    "throw a rejected exception if transaction is declined" in new Ctx {
      val someRejectReasonCode = "2"
      val someRejectReasonText = "rejected because"

      helper.postTransaction(someCreatedAnetMerchant, someSaleTransaction) returns
        aFailedTransactionResult(someRejectReasonCode, someRejectReasonText)

      authorizeNetGateway.sale(someMerchantKey, someCreditCard, somePayment) must
        beAFailedTry.like {
          case e: PaymentRejectedException => e.message must beEqualTo(s"response code: RRC_${someRejectReasonCode}_$rejectResponseCode, response text: $someRejectReasonText")
        }

      validateHelperFlow(_ => one(helper)
        .createAuthorizeCaptureTransaction(someAnetMerchant, someCurrencyAmount, someCreditCard, None, None))
    }

    "throw a rejected exception if transaction is review" in new Ctx {
      val someReviewReasonCode = "253"
      val someReviewReasonText = "review because"

      helper.postTransaction(someCreatedAnetMerchant, someSaleTransaction) returns
        aReviewTransactionResult(someReviewReasonCode, someReviewReasonText)

      authorizeNetGateway.sale(someMerchantKey, someCreditCard, somePayment) must
        beAFailedTry.like {
          case e: PaymentRejectedException => e.message must beEqualTo(s"response code: RRC_${reviewResponseCode}_$someReviewReasonCode, response text: $someReviewReasonText")
        }

      validateHelperFlow(_ => one(helper)
        .createAuthorizeCaptureTransaction(someAnetMerchant, someCurrencyAmount, someCreditCard, None, None))
    }

    "throw an error exception if transaction is error" in new Ctx {
      val someErrorReasonCode = "6"
      val someErrorReasonText = "error because"

      helper.postTransaction(someCreatedAnetMerchant, someSaleTransaction) returns
        anErrorTransactionResult(someErrorReasonCode, someErrorReasonText)

      authorizeNetGateway.sale(someMerchantKey, someCreditCard, somePayment) must
        beAFailedTry.like {
          case e: PaymentErrorException => e.message must beEqualTo(s"response code: RRC_${errorResponseCode}_$someErrorReasonCode, response text: $someErrorReasonText")
        }

      validateHelperFlow(_ => one(helper)
        .createAuthorizeCaptureTransaction(someAnetMerchant, someCurrencyAmount, someCreditCard, None, None))
    }
  }
}

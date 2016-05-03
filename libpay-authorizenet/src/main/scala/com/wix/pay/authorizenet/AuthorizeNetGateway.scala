/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2014, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.authorizenet


import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentGateway, PaymentRejectedException}
import net.authorize.Environment
import net.authorize.aim.{Result, Transaction}

import scala.util.{Failure, Success, Try}


/** A subclass of the [[PaymentGateway]], for Authorize.Net gateway.
  *
  * @param environment
  *                    The environment the gateway operates in (e.g., production, test, etc.)
  * @param helper
  *               Helper object being delegated to, for doing actual work.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class AuthorizeNetGateway(environment: Environment,
                          helper: AuthorizeNetGetewayHelper = new DefaultAuthorizeNetGetewayHelper(),
                          authorizationParser: AuthorizeNetAuthorizationParser = new JsonAuthorizenetAuthorizationParser)
    extends PaymentGateway {

  override def authorize(merchantKey: String,
                         creditCard: CreditCard,
                         currencyAmount: CurrencyAmount,
                         customer: Option[Customer] = None,
                         deal: Option[Deal] = None): Try[String] = {
    val merchant = helper.createMerchant(environment, merchantKey)
    val transaction = helper.createAuthorizeOnlyTransaction(merchant, currencyAmount, creditCard,deal,customer)
    val transactionResult = helper.postTransaction(merchant, transaction)

    val transactionId = handleTransactionResult(transactionResult)

    Try {
      transactionId match {
        case Success(id) => authorizationParser.stringify(AuthorizenetAuthorization(id))
        case Failure(e) => throw e
      }
    }
  }

  override def capture(merchantKey: String,
                       authorizationKey: String,
                       amount: Double): Try[String] = {
    val merchant = helper.createMerchant(environment, merchantKey)
    Try {
      authorizationParser.parse(authorizationKey)
    } match {
      case Success(authorization: AuthorizenetAuthorization) =>
        val transaction = helper.createCaptureTransaction(merchant, authorization.transactionId, amount)
        val transactionResult = helper.postTransaction(merchant, transaction)
        handleTransactionResult(transactionResult)

      case Failure(e) => Failure(new PaymentErrorException("Invalid authorizationKey format", e))
    }
  }

  override def sale(merchantKey: String,
                    creditCard: CreditCard,
                    currencyAmount: CurrencyAmount,
                    customer: Option[Customer] = None,
                    deal: Option[Deal] = None): Try[String] = {
    val merchant = helper.createMerchant(environment, merchantKey)
    val transaction = helper.createAuthorizeCaptureTransaction(merchant, currencyAmount, creditCard,deal,customer)
    val transactionResult = helper.postTransaction(merchant, transaction)

    handleTransactionResult(transactionResult)
  }

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    val merchant = helper.createMerchant(environment, merchantKey)
    val authorization = authorizationParser.parse(authorizationKey)
    val transaction = helper.createVoidTransaction(merchant, authorization.transactionId)
    val transactionResult = helper.postTransaction(merchant, transaction)

    handleTransactionResult(transactionResult)
  }


  private def handleTransactionResult(transactionResult: Result[Transaction]): Try[String] = {
    Try {
      transactionResult match {
        case IsApproved() =>
          transactionResult.getTarget.getTransactionId

        case IsDeclined() | IsReview() =>
          throw new PaymentRejectedException(
            s"response code: ${transactionResult.getReasonResponseCode}, response text: ${transactionResult.getResponseText}")

        case IsError() =>
          throw new PaymentErrorException(
            s"response code: ${transactionResult.getReasonResponseCode}, response text: ${transactionResult.getResponseText}")

        case _ =>
          throw new PaymentException(
            s"response code: ${transactionResult.getReasonResponseCode}, response text: ${transactionResult.getResponseText}")
      }
    }
  }
}

/** An Extractor Object usable for Transaction's Result, matched if the result is an ''Error''.
  *
  * An Extractor Object is an object that has a method(s) called {{{unapply}}} as one of its members. The purpose
  * of that {{{unapply}}} method is to match a value and take it apart.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object IsError {
  def unapply(result: Result[Transaction]): Boolean = {
    result.isError
  }
}


/** An Extractor Object usable for Transaction's Result, matched if the result is ''Declined''.
  *
  * An Extractor Object is an object that has a method(s) called {{{unapply}}} as one of its members. The purpose
  * of that {{{unapply}}} method is to match a value and take it apart.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object IsDeclined {
  def unapply(result: Result[Transaction]): Boolean = {
    result.isDeclined
  }
}


/** An Extractor Object usable for Transaction's Result, matched if the result is held for ''Review''.
  *
  * An Extractor Object is an object that has a method(s) called {{{unapply}}} as one of its members. The purpose
  * of that {{{unapply}}} method is to match a value and take it apart.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object IsReview {
  def unapply(result: Result[Transaction]): Boolean = {
    result.isReview
  }
}


/** An Extractor Object usable for Transaction's Result, matched if the result is ''Approved''.
  *
  * An Extractor Object is an object that has a method(s) called {{{unapply}}} as one of its members. The purpose
  * of that {{{unapply}}} method is to match a value and take it apart.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
object IsApproved {
  def unapply(result: Result[Transaction]): Boolean = {
    result.isApproved
  }
}

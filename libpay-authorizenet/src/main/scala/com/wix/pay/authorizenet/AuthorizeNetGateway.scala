/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2014, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.authorizenet


import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import com.wix.pay.{PaymentErrorException, PaymentGateway}
import net.authorize.Environment

import scala.util.{Failure, Success, Try}


/** A subclass of the `PaymentGateway`, for Authorize.Net gateway.
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

    TransactionResultTranslator.translateTransactionResult(transactionResult) match {
      case Success(transactionId) => Success(authorizationParser.stringify(AuthorizenetAuthorization(transactionId)))
      case Failure(e) => Failure(e)
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
        TransactionResultTranslator.translateTransactionResult(transactionResult)

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

    TransactionResultTranslator.translateTransactionResult(transactionResult)
  }

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    val merchant = helper.createMerchant(environment, merchantKey)
    val authorization = authorizationParser.parse(authorizationKey)
    val transaction = helper.createVoidTransaction(merchant, authorization.transactionId)
    val transactionResult = helper.postTransaction(merchant, transaction)

    TransactionResultTranslator.translateTransactionResult(transactionResult)
  }
}


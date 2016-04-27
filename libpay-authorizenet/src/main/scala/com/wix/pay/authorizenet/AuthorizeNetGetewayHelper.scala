/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2014, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.authorizenet


import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import net.authorize.aim.{Result, Transaction}
import net.authorize.{Environment, Merchant}


/** This trait defines the operations of a concrete helper class, that would receive delegated calls from the gateway,
  * for doing actual work.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
trait AuthorizeNetGetewayHelper {

  def createMerchant(environment: Environment, merchant: String): Merchant

  def createAuthorizeOnlyTransaction(merchant: Merchant,
                                     currencyAmount: CurrencyAmount,
                                     creditCard: CreditCard,
                                     deal : Option[Deal] = None,
                                     customer : Option[Customer] = None): Transaction

  def createCaptureTransaction(merchant: Merchant,
                               transactionId: String,
                               amount: Double): Transaction

  def createAuthorizeCaptureTransaction(merchant: Merchant,
                                        currencyAmount: CurrencyAmount,
                                        creditCard: CreditCard,
                                        deal : Option[Deal] = None,
                                        customer : Option[Customer] = None): Transaction

  def createCreditTransaction(merchant: Merchant,
                              transactionId: String,
                              creditCard: CreditCard,
                              amount: Double): Transaction

  def createVoidTransaction(merchant: Merchant,
                            transactionId: String): Transaction

  def postTransaction(merchant: Merchant, transaction: Transaction): Result[Transaction]
}

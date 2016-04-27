/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2014, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.authorizenet


import java.util

import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model._
import net.authorize.aim.{Result, Transaction}
import net.authorize.data.creditcard.{CreditCard => AutorizeNetCreditCard}
import net.authorize.data.{Customer => AnetCustomer, Order, OrderItem => AuthorizeNetOrderItem, ShippingAddress => AutorizeNetShippingAddress, ShippingCharges}
import net.authorize.{Environment, Merchant, TransactionType}

import scala.language.implicitConversions


/** A default implementation of the [[AuthorizeNetGetewayHelper]], using a JSON representation parsing of Authorize.Net
  * merchants.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class DefaultAuthorizeNetGetewayHelper() extends AuthorizeNetGetewayHelper {

  val merchantParser = new JsonAuthorizeNetMerchantParser()

  override def createMerchant(environment: Environment, merchantKey: String): Merchant = {
    merchantParser.parse(environment, merchantKey)
  }


  override def createAuthorizeOnlyTransaction(merchant: Merchant,
                                              currencyAmount: CurrencyAmount,
                                              creditCard: CreditCard,
                                              deal : Option[Deal] = None,
                                              customer : Option[Customer] = None): Transaction = {
    createAimTransaction(TransactionType.AUTH_ONLY,merchant,currencyAmount,creditCard,deal,customer)


  }
  override def createAuthorizeCaptureTransaction(merchant: Merchant,
                                                 currencyAmount: CurrencyAmount,
                                                 creditCard: CreditCard,
                                                 deal : Option[Deal] = None,
                                                 customer : Option[Customer] = None): Transaction = {
    createAimTransaction(TransactionType.AUTH_CAPTURE,merchant,currencyAmount,creditCard,deal,customer)
  }

  private def createAimTransaction(transactionType:  TransactionType,
                                    merchant: Merchant,
                                   currencyAmount: CurrencyAmount,
                                   creditCard: CreditCard,
                                   deal : Option[Deal] = None,
                                   customer : Option[Customer] = None): Transaction = {
    val aimTransaction = merchant.createAIMTransaction(
      transactionType,
      BigDecimal(currencyAmount.amount).bigDecimal)

    aimTransaction.setCurrencyCode(currencyAmount.currency)
    aimTransaction.setDuplicateWindow(0)

    Option(creditCard) foreach (aimTransaction.setCreditCard(_))
    aimTransaction.setCustomer(authNetCustomerFor(creditCard, customer))
    deal.flatMap(_.shippingAddress).foreach(shippingAddress=>
      aimTransaction.setShippingAddress(shippingAddressFor(shippingAddress)))
    deal.foreach(deal=>aimTransaction.setOrder(getOrderWithItems(deal)))
    aimTransaction

  }

  override def createCaptureTransaction(merchant: Merchant,
                                        transactionId: String,
                                        amount: Double): Transaction = {
    val aimTransaction = merchant.createAIMTransaction(
      TransactionType.PRIOR_AUTH_CAPTURE,
      BigDecimal(amount).bigDecimal)

    aimTransaction.setTransactionId(transactionId)

    aimTransaction
  }




  private def authNetCustomerFor(creditCard: CreditCard, customer: Option[Customer]) = {
    val authNetCustomer = AnetCustomer.createCustomer()
    val billingAddress = creditCard.billingAddressDetailed

    customer.flatMap(_.firstName) foreach authNetCustomer.setFirstName
    customer.flatMap(_.lastName) foreach authNetCustomer.setLastName
    customer.flatMap(_.company) foreach authNetCustomer.setCompany
    customer.flatMap(_.phone) foreach authNetCustomer.setPhone
    customer.flatMap(_.fax) foreach authNetCustomer.setFax
    customer.flatMap(_.email) foreach authNetCustomer.setEmail
    customer.flatMap(_.ipAddress) foreach authNetCustomer.setCustomerIP
    billingAddress.flatMap(_.street)  foreach authNetCustomer.setAddress
    billingAddress.flatMap(_.city) foreach authNetCustomer.setCity
    billingAddress.flatMap(_.state) foreach authNetCustomer.setState
    billingAddress.flatMap(_.postalCode) foreach authNetCustomer.setZipPostalCode
    billingAddress.flatMap(_.countryCode.map(_.getDisplayCountry)) foreach authNetCustomer.setCountry

    authNetCustomer
  }

  private def shippingAddressFor(shippingAddress: ShippingAddress) : AutorizeNetShippingAddress= {
    val anetShippingAddress = AutorizeNetShippingAddress.createShippingAddress()

    shippingAddress.firstName foreach anetShippingAddress.setFirstName
    shippingAddress.lastName foreach anetShippingAddress.setLastName
    shippingAddress.company foreach anetShippingAddress.setCompany
    shippingAddress.street foreach anetShippingAddress.setAddress
    shippingAddress.city foreach anetShippingAddress.setCity
    shippingAddress.state foreach anetShippingAddress.setState
    shippingAddress.postalCode foreach anetShippingAddress.setZipPostalCode
    shippingAddress.countryCode.map(_.getDisplayCountry) foreach anetShippingAddress.setCountry

    anetShippingAddress
  }
  def shippingChargesFor(charges: IncludedCharges): ShippingCharges = {
    val anetShippingCharges = ShippingCharges.createShippingCharges()
    charges.shipping.map(BigDecimal(_).bigDecimal) foreach  anetShippingCharges.setFreightAmount
    charges.shipping.map(s => "Shipping") foreach  anetShippingCharges.setFreightItemName
    charges.shipping.map(s => "Shipping") foreach  anetShippingCharges.setFreightDescription
    charges.tax.map(BigDecimal(_).bigDecimal) foreach  anetShippingCharges.setTaxAmount
    charges.tax.map(t=> "Tax") foreach  anetShippingCharges.setTaxItemName
    charges.tax.map(t=> "Tax") foreach  anetShippingCharges.setTaxDescription
    anetShippingCharges
  }

  private def getOrderWithItems(deal: Deal)= {
    val anetOrder = Order.createOrder()

    deal.invoiceId.foreach(anetOrder.setInvoiceNumber)

    anetOrder.setOrderItems(convertToANetOrderItems(deal.orderItems))
    deal.includedCharges.foreach(charges =>
      anetOrder.setShippingCharges(shippingChargesFor(charges)) )
    anetOrder
  }


  private def convertToANetOrderItems(items: Seq[OrderItem]): util.ArrayList[AuthorizeNetOrderItem] = {
    val aNetItems: util.ArrayList[AuthorizeNetOrderItem] = new util.ArrayList[AuthorizeNetOrderItem]()

    items.foreach(item => {
      val orderItem = AuthorizeNetOrderItem.createOrderItem()

      item.id.map(_.slice(0,31)) foreach orderItem.setItemId
      item.name.map(_.slice(0,31)) foreach orderItem.setItemName
      item.quantity foreach (quantity => orderItem.setItemQuantity(quantity.toString))
      item.pricePerItem foreach(price => orderItem.setItemPrice(price.toString))
      item.description.map(_.slice(0,255)) foreach orderItem.setItemDescription

      aNetItems.add(orderItem)
    })

    aNetItems
  }

  override def createCreditTransaction(merchant: Merchant,
                                       transactionId: String,
                                       creditCard: CreditCard,
                                       amount: Double): Transaction = {
    val aimTransaction = merchant.createAIMTransaction(
      TransactionType.CREDIT,
      BigDecimal(amount).bigDecimal)

    aimTransaction.setTransactionId(transactionId)
    Option(creditCard) foreach (aimTransaction.setCreditCard(_))

    aimTransaction
  }

  override def createVoidTransaction(merchant: Merchant,
                                     transactionId: String): Transaction = {
    val aimTransaction = merchant.createAIMTransaction(
      TransactionType.VOID, null)

    aimTransaction.setTransactionId(transactionId)

    aimTransaction
  }

  override def postTransaction(merchant: Merchant, transaction: Transaction): Result[Transaction] = {
    merchant.postTransaction(transaction).asInstanceOf[Result[Transaction]]
  }



  implicit def toAnetCreditCard(creditCard: CreditCard): AutorizeNetCreditCard = {
    val anetCreditCard = AutorizeNetCreditCard.createCreditCard()

    Option(creditCard.number) foreach anetCreditCard.setCreditCardNumber
    Option(creditCard.expiration) foreach {expiration =>
      anetCreditCard.setExpirationDate(s"${expiration.year}-${expiration.month}")
    }
    creditCard.csc foreach anetCreditCard.setCardCode

    anetCreditCard
  }
}

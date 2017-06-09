package com.wix.pay.authorizenet

import java.util.UUID

import net.authorize.aim.{Result, Transaction}
import net.authorize.data.creditcard.CreditCard
import net.authorize.data.{Order, Customer => AnetCustomer, OrderItem => AuthorizeNetOrderItem, ShippingAddress => AutorizeNetShippingAddress}
import net.authorize.{Environment, Merchant, TransactionType}

import scala.collection.JavaConversions._

object ClientApp extends App {
  val merchant = Merchant.createMerchant(Environment.SANDBOX, "82ysS586CeKV", "7MgP96q9E54R47cD")

  val amount = 1.0

  val aimTransaction = merchant.createAIMTransaction(TransactionType.AUTH_CAPTURE, BigDecimal(amount).bigDecimal)
  aimTransaction.setCurrencyCode("USD")
  aimTransaction.setDuplicateWindow(0)

  val orderItem = AuthorizeNetOrderItem.createOrderItem()
  orderItem.setItemId("someItemId")
  orderItem.setItemName("Some nice item")
  orderItem.setItemDescription("Some item description")
  orderItem.setItemQuantity("1")
  orderItem.setItemPrice(BigDecimal(amount / 2).bigDecimal)

  val order = Order.createOrder()
  order.setOrderItems(Seq(orderItem))

  aimTransaction.setOrder(order)

  val creditCard = CreditCard.createCreditCard()
  creditCard.setCreditCardNumber("5424000000000015")
  creditCard.setExpirationYear("2020")
  creditCard.setExpirationMonth("12")
  creditCard.setCardCode("999")
  aimTransaction.setCreditCard(creditCard)

  val response = merchant.postTransaction(aimTransaction).asInstanceOf[Result[Transaction]]
  println(s"[${response.getResponseCode}][${response.getReasonResponseCode}] ${response.getResponseText}")
}

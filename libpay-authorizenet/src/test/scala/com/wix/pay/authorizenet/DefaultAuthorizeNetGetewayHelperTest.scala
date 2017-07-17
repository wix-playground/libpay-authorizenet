/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2014, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.authorizenet

import java.util.{Locale, UUID}

import com.wix.pay.creditcard.{AddressDetailed, CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model._
import net.authorize.aim.Transaction
import net.authorize.{Environment, Merchant, TransactionType}
import org.specs2.matcher.MustMatchers._
import org.specs2.matcher.{AlwaysMatcher, Matcher}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

import scala.collection.JavaConversions._
import scala.util.Try

/** The Unit Test class for the [[DefaultAuthorizeNetGetewayHelper]] class.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class DefaultAuthorizeNetGetewayHelperTest extends SpecWithJUnit {

  trait Ctx extends Scope {
    val helper = new DefaultAuthorizeNetGetewayHelper()

    val someCcNumber = "4012888818888"
    val someCcExpiration = YearMonth(2020, 12)
    val someCcCsc = "123"
    val someMerchant = helper.createMerchant(Environment.SANDBOX, aMerchantKey("kuki", "buki"))
    val someCurrencyAmount = CurrencyAmount("USD", 33.3)
    val someCreditCard = CreditCard(
      someCcNumber,
      someCcExpiration,
      Some(CreditCardOptionalFields(
        csc = Some(someCcCsc))))
    val someTransactionId = "333"

    def aMerchantKey(login: String, transactionKey: String): String = {
      new JsonAuthorizeNetMerchantParser().stringify(AuthorizenetMerchant(login = login, transactionKey = transactionKey))
    }
  }

  val localeForUSA: Locale = new Locale("", "US")
  def reverseToLocale(that: Option[String]): Option[Locale] = {
    that match {
      case Some(country) => if (country == localeForUSA.getDisplayCountry) Some(localeForUSA) else None
      case _ => None
    }
  }

  def beMerchant(environment: Matcher[Environment] = AlwaysMatcher(),
                 login: Matcher[String] = AlwaysMatcher(),
                 transactionKey: Matcher[String] = AlwaysMatcher()): Matcher[Merchant] = {
    environment ^^ {(_: Merchant).getEnvironment aka "environment"} and
      login ^^ {(_: Merchant).getLogin aka "login"} and
      transactionKey ^^ {(_: Merchant).getTransactionKey aka "transaction key"}
  }

  def beTransaction(transactionType: Matcher[TransactionType] = AlwaysMatcher(),
                    creditCard: Matcher[CreditCard] = AlwaysMatcher(),
                    currencyAmount: Matcher[CurrencyAmount] = AlwaysMatcher(),
                    customer: Matcher[Option[Customer]] = AlwaysMatcher(),
                    deal: Matcher[Deal] = AlwaysMatcher()
                   ): Matcher[Transaction] = {
    transactionType ^^ {
      (_: Transaction).getTransactionType aka "transaction type"
    } and
      currencyAmount ^^ { (transaction: Transaction) =>
        CurrencyAmount(transaction.getCurrencyCode, transaction.getRequestMap.get("x_amount").toDouble)
      } and
      creditCard ^^ { (transaction: Transaction) =>
        val cc = transaction.getCreditCard
        CreditCard(
          number = cc.getCreditCardNumber,
          expiration = YearMonth(cc.getExpirationYear.toInt, cc.getExpirationMonth.toInt),
          additionalFields = Some(CreditCardOptionalFields.withFields(
            csc = Option(cc.getCardCode),
            holderId = Option(cc.getCardholderAuthenticationIndicator)
          ).withBillingAddressDetailed(Option(transaction.getCustomer).map(anetCustomer =>
            AddressDetailed(
              street = Option(anetCustomer.getAddress),
              city = Option(anetCustomer.getCity),
              state = Option(anetCustomer.getState),
              postalCode = Option(anetCustomer.getZipPostalCode),
              countryCode = reverseToLocale(Option(anetCustomer.getCountry)))))

          )) aka "credit card"
      } and
      deal ^^ { (transaction: Transaction) =>
        Deal(id = "DealId",
          invoiceId = Option(transaction.getOrder.getInvoiceNumber),
          includedCharges = Option(transaction.getOrder.getShippingCharges).map(s =>
            IncludedCharges(
              tax = Option(s.getTaxAmount).map(_.toString.toDouble),
              shipping = Option(s.getFreightAmount).map(_.toString.toDouble)
            )),
          shippingAddress = Option(transaction.getShippingAddress).map(shipAddress =>
            ShippingAddress(
              firstName = Option(shipAddress.getFirstName),
              lastName = Option(shipAddress.getLastName),
              company = Option(shipAddress.getCompany),
              address = Some(AddressDetailed(
                street = Option(shipAddress.getAddress),
                city = Option(shipAddress.getCity),
                state = Option(shipAddress.getState),
                postalCode = Option(shipAddress.getZipPostalCode),
                countryCode = reverseToLocale(Option(shipAddress.getCountry))
              )))),
          orderItems = Option(transaction.getOrder) match {
            case None => Seq.empty[OrderItem]
            case _ => transaction.getOrder.getOrderItems.toList.toSeq.map(x =>
              OrderItem(
                id = Option(x.getItemId),
                name = Option(x.getItemName),
                quantity = Option(x.getItemQuantity.toString.toDouble),
                pricePerItem = Option(x.getItemPrice.toString.toDouble),
                description = Option(x.getItemDescription)
              ))
          }
        )
      } and
      customer ^^ { (transaction: Transaction) =>
        val customerOption = if (transaction.getCustomer == null) {
          None
        } else {
          Some(Customer(
            Some(Name(transaction.getCustomer.getFirstName, transaction.getCustomer.getLastName)),
            phone = Option(transaction.getCustomer.getPhone),
            email = Option(transaction.getCustomer.getEmail),
            ipAddress = Option(transaction.getCustomer.getCustomerIP),
            fax = Option(transaction.getCustomer.getFax),
            company = Option(transaction.getCustomer.getCompany)))
        }
        customerOption
      }
  }

  def beCurrencyAmount(currecny: Matcher[String] = AlwaysMatcher(),
                       amount: Matcher[Double] = AlwaysMatcher()): Matcher[CurrencyAmount] = {
    currecny ^^ {(_: CurrencyAmount).currency aka "currency"} and
      amount ^^ {(_: CurrencyAmount).amount aka "amount"}
  }

  def beCreditCard(number: Matcher[String] = AlwaysMatcher(),
                   expiration: Matcher[YearMonth] = AlwaysMatcher(),
                   csc: Matcher[Option[String]] = AlwaysMatcher(),
                   billingAddress: Matcher[Option[AddressDetailed]] = AlwaysMatcher()
                  ): Matcher[CreditCard] = {
    number ^^ {(_: CreditCard).number aka "credit card number"} and
      expiration ^^ {(_: CreditCard).expiration aka "expiration"} and
      csc ^^ {(_: CreditCard).csc aka "csc (cvv)"} and
      billingAddress ^^ {(_: CreditCard).billingAddressDetailed aka "billing address detailed"}
  }

  def beDeal(invoiceId: Matcher[Option[String]] = AlwaysMatcher(),
             shippingAddress: Matcher[Option[ShippingAddress]] = AlwaysMatcher(),
             taxInOrderShippingCharges: Matcher[Option[IncludedCharges]] = AlwaysMatcher(),
             orderItems: Matcher[Seq[OrderItem]] = AlwaysMatcher()
            ): Matcher[Deal] = {

    invoiceId ^^ {(_: Deal).invoiceId aka "invoiceId"} and
      shippingAddress ^^ {(_: Deal).shippingAddress aka "shipping address"} and
      taxInOrderShippingCharges ^^ {(_: Deal).includedCharges aka "tax in order shipping charges"} and
      orderItems ^^ {(_: Deal).orderItems aka "orderItems"}
  }

  def beShippingAddress(number: Matcher[String] = AlwaysMatcher(),
                        expiration: Matcher[YearMonth] = AlwaysMatcher(),
                        csc: Matcher[Option[String]] = AlwaysMatcher()): Matcher[CreditCard] = {
    number ^^ {(_: CreditCard).number aka "credit card number"} and
      expiration ^^ {(_: CreditCard).expiration aka "expiration"} and
      csc ^^ {(_: CreditCard).csc aka "csc (cvv)"}
  }

  def beUUID: Matcher[String] = beSuccessfulTry ^^ { str: String => Try(UUID.fromString(str)) }

  "createMerchant" should {
    "create a production merchant with the given credentials" in new Ctx {
      val someLogin = "kuki"
      val someTransactionKey = "buki"
      val merchantKey = aMerchantKey(someLogin, someTransactionKey)

      helper.createMerchant(Environment.SANDBOX, merchantKey) must beMerchant(
        environment = ===(Environment.SANDBOX),
        login = ===(someLogin),
        transactionKey = ===(someTransactionKey))
    }
  }

  "create authorize only transaction" should {
    "create an Authorize Only transaction with the given monetary details and credit card details" in new Ctx {

      helper.createAuthorizeOnlyTransaction(someMerchant, someCurrencyAmount, someCreditCard) must beTransaction(
        transactionType = ===(TransactionType.AUTH_ONLY),
        creditCard = beCreditCard(
          number = ===(someCcNumber),
          expiration = ===(someCcExpiration),
          csc = beSome(someCcCsc)),
        currencyAmount = ===(someCurrencyAmount))
    }
  }

  "create capture only transaction" should {
    "create a Capture Only transaction, with the given amount and transaction ID" in new Ctx {

      helper.createCaptureTransaction(someMerchant, someTransactionId, someCurrencyAmount.amount) must beTransaction(
        transactionType = ===(TransactionType.PRIOR_AUTH_CAPTURE),
        currencyAmount = beCurrencyAmount(amount = ===(someCurrencyAmount.amount)))
    }
  }

  "create authorize-capture transaction" should {
    "create a Authorize-Capture transaction, with the given monetary details and credit card details" in new Ctx {

      helper.createAuthorizeCaptureTransaction(someMerchant, someCurrencyAmount, someCreditCard) must beTransaction(
        transactionType = ===(TransactionType.AUTH_CAPTURE),
        creditCard = beCreditCard(
          number = ===(someCcNumber),
          expiration = ===(someCcExpiration),
          csc = beSome(someCcCsc)),
        currencyAmount = ===(someCurrencyAmount))
    }
  }

  "create credit transaction" should {
    "create a Credit transaction, with the given monetary details, and credit card details" in new Ctx {

      helper.createCreditTransaction(someMerchant, someTransactionId, someCreditCard, someCurrencyAmount.amount) must
        beTransaction(
          transactionType = ===(TransactionType.CREDIT),
          creditCard = beCreditCard(
            number = ===(someCcNumber),
            expiration = ===(someCcExpiration),
            csc = beSome(someCcCsc)),
          currencyAmount = beCurrencyAmount(amount = ===(someCurrencyAmount.amount)))
    }
  }

  "create credit transaction with additional info" should {
    "create a Credit transaction, with shipping address" in new Ctx {

      val deal = Deal("dealId", shippingAddress = Some(ShippingAddress(
        firstName = Some("shippingFirstName"),
        lastName = Some("shippingLastName"),
        company = Some("shippingCompany"),
        address = Some(AddressDetailed(
          street = Some("shippingStreet"),
          city = Some("shippingCity"),
          state = Some("shippingState"),
          postalCode = Some("shippingPostalCode"),
          countryCode = Some(localeForUSA)
        ))
      )))

      helper.createAuthorizeCaptureTransaction(someMerchant, someCurrencyAmount, someCreditCard, Some(deal), None) must
        beTransaction(
          deal = beDeal(shippingAddress = ===(deal.shippingAddress)))
    }

    "create a Credit transaction, with tax" in new Ctx {

      val deal = Deal("dealId", includedCharges = Some(IncludedCharges(tax = Some(1.0), shipping = Some(2.0))))
      helper.createAuthorizeCaptureTransaction(someMerchant, someCurrencyAmount, someCreditCard, Some(deal), None) must
        beTransaction(
          deal = beDeal(taxInOrderShippingCharges = ===(deal.includedCharges)))
    }

    "create a Credit transaction, with customer details" in new Ctx {
      val customer = Some(Customer(name = Some(Name("FirstName", "LastName")),
        phone = Some("phone"),
        email = Some("email"),
        ipAddress = Some("ipAddress"),
        fax = Some("fax"),
        company = Some("company")))

      helper.createAuthorizeCaptureTransaction(someMerchant, someCurrencyAmount, someCreditCard, None, customer) must
        beTransaction(
          customer = ===(customer))
    }

    "create a Credit transaction, with billing address" in new Ctx {
      val billingAddress = Some(AddressDetailed(
        street = Some("billingStreet"),
        city = Some("billingCity"),
        state = Some("billingState"),
        postalCode = Some("billingPostalCode"),
        countryCode = Some(localeForUSA))
      )
      val ccWithBillingAddress = someCreditCard.copy(
        additionalFields = Some(CreditCardOptionalFields(
          csc = Some(someCcCsc)).withBillingAddressDetailed(billingAddress)))//))
      helper.createAuthorizeCaptureTransaction(someMerchant, someCurrencyAmount, ccWithBillingAddress, None, None) must
        beTransaction(
          creditCard = beCreditCard(billingAddress = ===(billingAddress)))
    }

    "create a Credit transaction, with order items" in new Ctx {
      val deal = Deal("dealId",
        invoiceId = Some("invoiceId"),
        orderItems = Seq(OrderItem(
          id = Some("orderItemId"),
          name = Some("OrderItemName"),
          quantity = Some(1.0),
          pricePerItem = Some(2.0),
          description = Some("ItemDescription")
        ))
      )

      helper.createAuthorizeCaptureTransaction(someMerchant, someCurrencyAmount, someCreditCard, Some(deal), None) must
        beTransaction(
          deal = beDeal(
            invoiceId = ===(deal.invoiceId),
            orderItems = beEqualTo(deal.orderItems)))
    }

    "creating a Credit transaction with an order item without id, name or quantity must generate those values" in new Ctx {
      val deal = Deal("dealId",
        invoiceId = Some("invoiceId"),
        orderItems = Seq(OrderItem(
          id = None,
          name = None,
          quantity = None,
          pricePerItem = Some(2.0),
          description = Some("ItemDescription")
        ))
      )

      val orderItems: Seq[net.authorize.data.OrderItem] =
        helper.createAuthorizeCaptureTransaction(someMerchant, someCurrencyAmount, someCreditCard, Some(deal), None).getOrder.getOrderItems
      
      orderItems must haveSize(1)
      val orderItem = orderItems.head
      orderItem.getItemId must beUUID
      orderItem.getItemName must beUUID
      orderItem.getItemQuantity mustEqual BigDecimal(1.0).bigDecimal
    }
  }

  "create void transaction" should {
    "create a Void transaction, with the given transaction ID" in new Ctx {

      helper.createVoidTransaction(someMerchant, someTransactionId) must
        beTransaction(transactionType = ===(TransactionType.VOID))
    }
  }
}

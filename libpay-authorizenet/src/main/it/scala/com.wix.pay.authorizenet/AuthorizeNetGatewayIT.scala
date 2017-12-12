package com.wix.pay.authorizenet

import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import com.wix.pay.authorizenet.testkit.AuthorizeNetDriver
import com.wix.pay.authorizenet.testkit.builders.AuthNetDomainTestBuilders._
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.{CurrencyAmount, Payment}
import net.authorize.{Environment => AuthorizenetEnvironment}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.{AfterAll, BeforeAll, Scope}

class AuthorizeNetGatewayIT extends SpecWithJUnit with BeforeAll with AfterAll {
  sequential
  "AuthorizeNetGateway" should {
    "return approved response" in new ctx {
      givenAuthorizeApprovesTransaction

      authNetGateway.sale(someMerchantKey, creditCard, somePayment
      ) must beASuccessfulTry(
        check = ===(transactionId)
      )
    }

    "return decline response" in new ctx {
      givenAuthorizeDeclinesTransaction

      authNetGateway.sale(someMerchantKey, invalidCreditCard, somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }

    "return GatewayFailure response" in new ctx {
      givenAuthorizeFailsTransaction

      authNetGateway.sale(someMerchantKey, creditCard, somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentErrorException]
      )
    }
  }

  val authNetPort = 10932
  val authNetDriver = new AuthorizeNetDriver(authNetPort)
  val merchantParser = new JsonAuthorizeNetMerchantParser

  trait ctx extends Scope {
    val baseUrl: String = s"http://localhost:$authNetPort"
    val environment: AuthorizenetEnvironment = AuthorizenetEnvironment.createEnvironment(baseUrl, baseUrl)
    val authNetGateway = new AuthorizeNetGateway(environment)

    authNetDriver.reset()

    val someLoginId = "someLogin"
    val someTransactionKey = "someTransactionKey"
    val someMerchant = AuthorizenetMerchant(someLoginId, someTransactionKey)
    val someMerchantKey = merchantParser.stringify(someMerchant)

    val someAmount = 33.3
    val someCurrencyAmount = CurrencyAmount("USD", someAmount)
    val somePayment = Payment(currencyAmount = CurrencyAmount("USD", someAmount))

    val creditCard = CreditCard(
      "4012888818888",
      YearMonth(2020, 12),
      Some(CreditCardOptionalFields(csc = Some("123"))))

    val invalidCreditCard = creditCard.copy(number = "")

    def givenAuthorizeApprovesTransaction = {
      authNetDriver.aSaleRequestFor(
        Some(someLoginId),
        Some(someTransactionKey),
        Some(someCurrencyAmount),
        Some(creditCard)) returns transactionId
    }

    def givenAuthorizeDeclinesTransaction = {
      authNetDriver.aSaleRequestFor(
        Some(someLoginId),
        Some(someTransactionKey),
        Some(someCurrencyAmount),
        Some(invalidCreditCard)) errors(
        declineErrorMessage.responseCode,
        declineErrorMessage.responseReasonCode,
        declineErrorMessage.responseReasonText)
    }

    def givenAuthorizeFailsTransaction = {
      authNetDriver.aSaleRequestFor(
        Some(someLoginId),
        Some(someTransactionKey),
        Some(someCurrencyAmount),
        Some(creditCard)) errors(
        failureErrorMessage.responseCode,
        failureErrorMessage.responseReasonCode,
        failureErrorMessage.responseReasonText)
    }
  }

  def beforeAll() =
    authNetDriver.start()

  def afterAll() =
    authNetDriver.stop()
}

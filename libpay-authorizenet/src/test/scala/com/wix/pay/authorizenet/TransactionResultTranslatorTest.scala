/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2014, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.authorizenet


import java.math.{BigDecimal => JBigDecimal}

import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import net.authorize._
import net.authorize.aim.{Result, Transaction}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

import scala.collection.JavaConversions._
import scala.util.Try

class TransactionResultTranslatorTest extends SpecWithJUnit {
  trait Ctx extends Scope {}

  def translateResponseReasonCode(responseReasonCode: ResponseReasonCode): Try[String] = {
    val transactionResult = Result.createResult[Transaction](null, Map(
      ResponseField.RESPONSE_CODE -> responseReasonCode.getResponseCode.getCode.toString,
      ResponseField.RESPONSE_REASON_CODE -> responseReasonCode.getResponseReasonCode.toString,
      ResponseField.RESPONSE_REASON_TEXT -> responseReasonCode.getReasonText
    ))

    TransactionResultTranslator.translateTransactionResult(transactionResult)
  }

  def translateResponseReasonCode(responseReasonCode: ResponseReasonCode, transactionId: String): Try[String] = {
    val merchant = Merchant.createMerchant(Environment.CUSTOM, "some login", "some transactionKey")
    val transaction = Transaction.createTransaction(merchant, TransactionType.AUTH_CAPTURE, JBigDecimal.ONE)
    val transactionResult = Result.createResult[Transaction](transaction, Map(
      ResponseField.RESPONSE_CODE -> responseReasonCode.getResponseCode.getCode.toString,
      ResponseField.RESPONSE_REASON_CODE -> responseReasonCode.getResponseReasonCode.toString,
      ResponseField.RESPONSE_REASON_TEXT -> responseReasonCode.getReasonText,
      ResponseField.TRANSACTION_ID -> transactionId
    ))

    TransactionResultTranslator.translateTransactionResult(transactionResult)
  }

  "translateTransactionResult" should {
    "treat approved transactions as successful" in new Ctx {
      val someTransactionId = "some transaction ID"
      translateResponseReasonCode(ResponseReasonCode.RRC_1_1, someTransactionId) must beASuccessfulTry(
        check = beEqualTo(someTransactionId)
      )
    }

    "treat declined transactions as rejected" in new Ctx {
      val transactionResponseCode = ResponseReasonCode.RRC_2_44
      translateResponseReasonCode(transactionResponseCode) must beAFailedTry.like {
        case e: PaymentRejectedException =>
          e.message must (contain(transactionResponseCode.getResponseReasonCode.toString) and
            contain(transactionResponseCode.getReasonText))
      }
    }

    "treat transactions in review as rejected" in new Ctx {
      val transactionResponseCode = ResponseReasonCode.RRC_4_193
      translateResponseReasonCode(transactionResponseCode) must beAFailedTry.like {
        case e: PaymentRejectedException =>
          e.message must (contain(transactionResponseCode.getResponseReasonCode.toString) and
            contain(transactionResponseCode.getReasonText))
      }
    }

    "treat specific erroneous transactions as rejected" in new Ctx {
      val transactionResponseCode = ResponseReasonCode.RRC_3_78
      translateResponseReasonCode(transactionResponseCode) must beAFailedTry.like {
        case e: PaymentRejectedException =>
          e.message must (contain(transactionResponseCode.getResponseReasonCode.toString) and
            contain(transactionResponseCode.getReasonText))
      }
    }

    "treat all other transactions as errors" in new Ctx {
      val transactionResponseCode = ResponseReasonCode.RRC_3_173
      translateResponseReasonCode(transactionResponseCode) must beAFailedTry.like {
        case e: PaymentErrorException =>
          e.message must (contain(transactionResponseCode.getResponseReasonCode.toString) and
            contain(transactionResponseCode.getReasonText))
      }
    }
  }
}

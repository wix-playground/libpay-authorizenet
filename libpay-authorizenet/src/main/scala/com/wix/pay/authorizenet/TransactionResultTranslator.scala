package com.wix.pay.authorizenet

import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import net.authorize.ResponseReasonCode
import net.authorize.aim.{Result, Transaction}

import scala.util.Try

object TransactionResultTranslator {
  def translateTransactionResult(transactionResult: Result[Transaction]): Try[String] = {
    Try {
      transactionResult match {
        case IsApproved() =>
          transactionResult.getTarget.getTransactionId

        case IsDeclined() | IsReview() | IsErrorDeclined() =>
          throw PaymentRejectedException(
            s"response code: ${transactionResult.getReasonResponseCode}, response text: ${transactionResult.getResponseText}")

        case _ =>
          throw PaymentErrorException(
            s"response code: ${transactionResult.getReasonResponseCode}, response text: ${transactionResult.getResponseText}")
      }
    }
  }

  private val declinedReasonResponseCodes = Set(
    ResponseReasonCode.RRC_3_78
  )

  /** Extractor object matching erroneous transactions that should be treated as declined transactions. */
  private object IsErrorDeclined {
    def unapply(result: Result[Transaction]): Boolean = {
      result.isError && declinedReasonResponseCodes.contains(result.getReasonResponseCode)
    }
  }

  /** Extractor object matching declined transactions. */
  private object IsDeclined {
    def unapply(result: Result[Transaction]): Boolean = {
      result.isDeclined
    }
  }


  /** Extractor object matching transactions that are pending review. */
  private object IsReview {
    def unapply(result: Result[Transaction]): Boolean = {
      result.isReview
    }
  }


  /** Extractor matching approved transactions. */
  private object IsApproved {
    def unapply(result: Result[Transaction]): Boolean = {
      result.isApproved
    }
  }
}

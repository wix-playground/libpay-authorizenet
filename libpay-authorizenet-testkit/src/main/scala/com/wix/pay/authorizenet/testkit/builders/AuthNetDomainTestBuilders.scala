package com.wix.pay.authorizenet.testkit.builders

object AuthNetDomainTestBuilders {
  val transactionId = "2226056663"

  val declineErrorMessage = AuthNetErrorMessage(responseCode = 2, responseReasonCode = 65, responseReasonText = "Some Response Reason Text")
  val failureErrorMessage = AuthNetErrorMessage(responseCode = 3, responseReasonCode = 33, responseReasonText = "Some Response Reason Text")
}

case class AuthNetErrorMessage(responseCode: Int, responseReasonCode: Int, responseReasonText: String) {
  val message: String = s"response code: RRC_${responseCode}_$responseReasonCode, response text: $responseReasonText"
}
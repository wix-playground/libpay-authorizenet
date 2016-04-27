package com.wix.pay.authorizenet

trait AuthorizeNetAuthorizationParser {
  def parse(authorizationKey: String): AuthorizenetAuthorization
  def stringify(authorization: AuthorizenetAuthorization): String
}

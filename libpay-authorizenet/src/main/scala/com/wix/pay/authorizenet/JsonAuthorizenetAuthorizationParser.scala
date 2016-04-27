package com.wix.pay.authorizenet

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonAuthorizenetAuthorizationParser() extends AuthorizeNetAuthorizationParser {
  private implicit val formats = DefaultFormats

  override def parse(authorizationKey: String): AuthorizenetAuthorization = {
    Serialization.read[AuthorizenetAuthorization](authorizationKey)
  }

  override def stringify(authorization: AuthorizenetAuthorization): String = {
    Serialization.write(authorization)
  }
}

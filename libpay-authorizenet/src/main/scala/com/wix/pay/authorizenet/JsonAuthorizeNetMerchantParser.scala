/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2014, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.authorizenet


import net.authorize.{Environment, Merchant}
import org.json4s.DefaultFormats
import org.json4s.native.Serialization


/** A concrete subclass of the [[AuthorizeNetMerchantParser]], using JSON for parsing to and from Authorize.Net
  * merchants.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class JsonAuthorizeNetMerchantParser() extends AuthorizeNetMerchantParser {
  private implicit val formats = DefaultFormats

  /** Parses a JSON string representation of a merchant, for the specified environment.
    *
    * @param environment
    *                    The environment used for the parsed merchant.
    * @param merchantKey
    *                 A JSON string representation of the merchant (holding the credentials, {{{loginId}}} and
    *                 {{{transactionKey}}}).
    * @return
    *         The parsed merchant.
    */
  override def parse(environment: Environment, merchantKey: String): Merchant = {
    val authorizenetMerchant = Serialization.read[AuthorizenetMerchant](merchantKey)

    Merchant.createMerchant(
      environment,
      authorizenetMerchant.login,
      authorizenetMerchant.transactionKey)
  }

  /** Stringifies the specified merchant to a JSON string.
    *
    * @param merchant
    *                 The merchant to be stringified.
    * @return
    *         A JSON string representation of the specified merchant.
    */
  override def stringify(merchant: Merchant): String = {
    stringify(AuthorizenetMerchant(login = merchant.getLogin, transactionKey = merchant.getTransactionKey))
  }

  def stringify(merchant: AuthorizenetMerchant): String = {
    Serialization.write(merchant)
  }
}

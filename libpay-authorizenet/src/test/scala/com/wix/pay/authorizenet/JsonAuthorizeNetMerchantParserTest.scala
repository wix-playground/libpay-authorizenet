/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2014, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.authorizenet


import net.authorize.{Environment, Merchant}
import org.specs2.matcher.Matcher
import org.specs2.matcher.MustMatchers._
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope



/** The Unit Test class for the [[JsonAuthorizeNetMerchantParser]] class.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
class JsonAuthorizeNetMerchantParserTest extends SpecWithJUnit {

  trait Ctx extends Scope {
    val parser = new JsonAuthorizeNetMerchantParser
  }

  def beMerchant(merchant: Merchant): Matcher[Merchant] = {
    be_===(merchant.getDeviceType) ^^ { (_: Merchant).getDeviceType aka "device type" } and
      be_===(merchant.getEnvironment) ^^ { (_: Merchant).getEnvironment aka "environment" } and
      be_===(merchant.getLogin) ^^ { (_: Merchant).getLogin aka "login" } and
      be_===(merchant.getMarketType) ^^ { (_: Merchant).getMarketType aka "market type" } and
      be_===(merchant.getMD5Value) ^^ { (_: Merchant).getMD5Value aka "MD5 value" } and
      be_===(merchant.getTransactionKey) ^^ { (_: Merchant).getTransactionKey aka "transaction key" } and
      be_===(merchant.getUserRef) ^^ { (_: Merchant).getUserRef aka "user ref" }
  }


  "stringify and then parse" should {
    "yield a merchant similar to the original one" in new Ctx {
      val merchant = Merchant.createMerchant(Environment.PRODUCTION, "kuki", "buki")
      val strMerchant = parser.stringify(merchant)

      parser.parse(Environment.PRODUCTION, strMerchant) must beMerchant(merchant)
    }
  }
}

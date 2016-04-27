/*      __ __ _____  __                                              *\
**     / // // /_/ |/ /          Wix                                 **
**    / // // / /|   /           (c) 2006-2014, Wix LTD.             **
**   / // // / //   |            http://www.wix.com/                 **
**   \__/|__/_//_/| |                                                **
\*                |/                                                 */
package com.wix.pay.authorizenet


import net.authorize.{Environment, Merchant}


/** This trait defines the operations for parsing Authorize.Net merchant.
  *
  * @author <a href="mailto:ohadr@wix.com">Raz, Ohad</a>
  */
trait AuthorizeNetMerchantParser {

  def parse(environment: Environment, merchantKey: String): Merchant

  def stringify(merchant: Merchant): String
}

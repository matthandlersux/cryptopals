package rounds

import crypto.{Oracle, Crypto}
import helpers.{Transformers, Helpers}
import data.Data

import play.api.libs.json.JsObject

object Round2 {

  import Transformers.bytesToString

  private val key = "YELLOW SUBMARINE"

	def problem9: String = {
		bytesToString(Helpers.padBlock(key.getBytes, 20))
	}

  def problem10: String = {
    val bytes1 = Transformers.decode64(Data.Round1.problem7)
    val decrypted = Crypto.decryptECB(bytes1, key)
    assert(bytesToString(bytes1) == Crypto.encryptECB(decrypted.getBytes, key))

    val bytes2 = Transformers.decode64(Data.Round2.problem10)
    Crypto.decryptCBC(bytes2, key, (Seq.fill(16)(0) map (_.toByte)).toArray)
  }

  def problem11: Double = {
    val bytes = Transformers.decode64(Data.Round2.problem10)
    val iv = (Seq.fill(16)(0) map (_.toByte)).toArray
    val data = Crypto.decryptCBC(bytes, key, iv)
    assert(bytesToString(bytes) == Crypto.encryptCBC((data map (_.toByte)).toArray, key, iv))

    val string = "ab" * (16 * 100)
    val totalRuns = 1000
    val ecbRuns = (0 to totalRuns) map { _ =>
      val encrypted = Oracle.ecbCBCOracle(string) map (_.toByte)
      val grouped = (encrypted grouped 16).toSeq
      grouped.toSet.size.toDouble/grouped.size
    } filter (_ < 0.2)

    ecbRuns.size.toDouble/totalRuns
  }

  def problem12: String = {
    Crypto.decryptECBBlackBox(16, Oracle.stringPrependOracle)
  }

  def problem13: Option[JsObject] = {
    val key = Helpers.randomKey(16) map (_.toChar) mkString ""
    val emailWithAdmin = "asdf@e.comadmin"
    val encryptedAdminBlock = Oracle.encodedParamsOracle(key, emailWithAdmin) drop 16 take 16
    val emailWithBoundary = "asdf@email.cm"
    val encryptedHeadBlock = Oracle.encodedParamsOracle(key, emailWithBoundary) take 32

    val encoded = Crypto.decryptECB((encryptedHeadBlock + encryptedAdminBlock + encryptedHeadBlock.take(16)).map(_.toByte).toArray, key)

    Transformers.parseQueryString(encoded)
  }

  def problem14: String = {

  }
}
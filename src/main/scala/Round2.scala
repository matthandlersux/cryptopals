package rounds

import crypto.{Oracle, Crypto}
import helpers.Helpers
import data.Data

object Round2 {

	def problem9: String = {
		val string = "YELLOW SUBMARINE"
		Helpers.padBlock(string.getBytes, 20) map (_.toChar) mkString ""
	}

  def problem10: String = {
    val bytes1 = Helpers.decode64(Data.Round1.problem7)
    val key = "YELLOW SUBMARINE"

    val decrypted = Crypto.decryptECB(bytes1, key)
    assert(Helpers.bytesToString(bytes1) == Crypto.encryptECB(decrypted.getBytes, key))

    val bytes2 = Helpers.decode64(Data.Round2.problem10)
    Crypto.decryptCBC(bytes2, key, (Seq.fill(16)(0) map (_.toByte)).toArray)
  }

  def problem11: Double = {
    val key = "YELLOW SUBMARINE"
    val bytes = Helpers.decode64(Data.Round2.problem10)
    val iv = (Seq.fill(16)(0) map (_.toByte)).toArray
    val data = Crypto.decryptCBC(bytes, key, iv)
    assert((bytes map (_.toChar) mkString) == Crypto.encryptCBC((data map (_.toByte)).toArray, key, iv))

    val string = Seq.fill(16 * 50)("ab") mkString ""
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
}
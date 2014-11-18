package rounds

import crypto.Crypto
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

    val decrypted = Crypto.decryptAESECB(bytes1, key)
    assert(Helpers.bytesToString(bytes1) == Crypto.encryptAESECB(decrypted.getBytes, key))

    val bytes2 = Helpers.decode64(Data.Round2.problem10)
    Crypto.decryptCBC(bytes2, key, (Seq.fill(16)(0) map (_.toByte)).toArray)
  }

}
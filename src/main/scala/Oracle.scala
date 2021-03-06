package crypto

import helpers.{Transformers, Helpers}
import data.Data

import scala.util.Random

object Oracle {

  def encodedParamsOracle(key: String, email: String): String = {
    val encodedEmail = email replaceAll ("&", "%26") replaceAll ("=", "%3D")
    val bytes = (s"email=$encodedEmail&uid=10&role=user" map (_.toByte)).toArray
    Crypto.encryptECB(bytes, key)
  }

  def stringPrependOracle: String => String = {
    val bytes = Transformers.decode64(Data.Round2.problem12)
    val key = Helpers.randomKey(16)
    (string: String) => Crypto.encryptECB((string map (_.toByte)).toArray ++ bytes, key map (_.toChar) mkString "")
  }

  def stringEncaseOracle: String => String = {
    val prepend = Helpers.randomKey(Random.nextInt(17))
    val bytes = Transformers.decode64(Data.Round2.problem12)
    val key = Helpers.randomKey(16)

    (string: String) => {
      val stringBytes = string.map(_.toByte).toArray
      Crypto.encryptECB(prepend ++ stringBytes ++ bytes, key map (_.toChar) mkString "")
    }
  }

  def ecbCBCOracle(string: String): String = {
    val keySize = 16
    val randomKey = Helpers.randomKey(keySize) map (_.toChar) mkString ""
    val bytes = (string map (_.toByte)).toArray
    val input = randomPad(5, 5) ++ bytes ++ randomPad(5, 5)
    val paddedInput = Helpers.padToMultiple(input, keySize, Byte.box(0))

    if (Random.nextBoolean)
      Crypto.encryptCBC(paddedInput, randomKey, Helpers.randomKey(keySize))
    else
      Crypto.encryptECB(paddedInput, randomKey)
  }

  private def randomPad(base: Int, variable: Int): Array[Byte] =
    Helpers.randomKey(base) ++ Helpers.randomKey(Random.nextInt(variable + 1))

}
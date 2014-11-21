package crypto

import helpers.{Transformers, Helpers}
import data.Data

import scala.util.Random

object Oracle {

  def encodedParamsOracle: String => String = {
    val key = Helpers.randomKey(16)
    (email: String) => {
      val encodedEmail = (email replaceAll ("&", "%26") replaceAll ("=", "%3D") map (_.toByte)).toArray
      val bytes = (s"email=$encodedEmail&uid=10&role=user" map (_.toByte)).toArray
      Crypto.encryptECB(bytes, key map (_.toChar) mkString "")
    }
  }

  def stringPrependOracle: String => String = {
    val bytes = Transformers.decode64(Data.Round2.problem12)
    val key = Helpers.randomKey(16)
    (string: String) => Crypto.encryptECB((string map (_.toByte)).toArray ++ bytes, key map (_.toChar) mkString "")
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
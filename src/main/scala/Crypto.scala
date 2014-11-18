package crypto

import helpers.Helpers

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import scala.util.Random

object Crypto {

  def findRepeatingBlockScore(string: String, size: Int): Double =
    (string grouped size).toSet.size.toDouble/size

  def findRepeatingBlockScore[T](array: Array[T], size: Int): Double =
    (array grouped size).toSet.size.toDouble/size

  def randomlyEncrypt(string: String): String = {
    val keySize = 16
    val randomKey = Helpers.randomKey(keySize) map (_.toChar) mkString ""
    val bytes = (string map (_.toByte)).toArray
    val input = randomPad(5, 5) ++ bytes ++ randomPad(5, 5)
    val paddingNeeded = input.size % keySize match {
      case 0 => 0
      case i => input.size - i
    }
    val paddedInput = Helpers.padBlock(input, paddingNeeded, Byte.box(0))

    if (Random.nextBoolean)
      encryptCBC(paddedInput, randomKey, Helpers.randomKey(keySize))
    else
      encryptAESECB(paddedInput, randomKey)
  }

  private def randomPad(base: Int, variable: Int): Array[Byte] =
    Helpers.randomKey(base) ++ Helpers.randomKey(Random.nextInt(variable + 1))

  private case class CBCAcc(iv: Array[Byte], output: Array[Byte] = Array())

  def decryptCBC(bytes: Array[Byte], key: String, iv: Array[Byte]): String = {
    val cbc = (bytes grouped key.size).foldLeft(CBCAcc(iv)) { case (CBCAcc(vector, output), cipherText) =>
      val decrypted = decryptAESECB(cipherText, key) map (_.toByte)
      CBCAcc(cipherText, output ++ Xor.xorBytes(decrypted.toArray, vector))
    }

    cbc.output map (_.toChar) mkString ""
  }

  def encryptCBC(bytes: Array[Byte], key: String, iv: Array[Byte]): String = {
    val cbc = (bytes grouped key.size).foldLeft(CBCAcc(iv)) { case (CBCAcc(vector, output), plainText) =>
      val xored = Xor.xorBytes(plainText, vector)
      val cipherText = (encryptAESECB(xored, key) map (_.toByte)).toArray
      CBCAcc(cipherText, output ++ cipherText)
    }

    cbc.output map (_.toChar) mkString ""
  }

  def encryptAESECB(bytes: Array[Byte], key: String): String =
    aesECB(bytes, key, Cipher.ENCRYPT_MODE)

  def decryptAESECB(bytes: Array[Byte], key: String): String =
    aesECB(bytes, key, Cipher.DECRYPT_MODE)

  private def aesECB(bytes: Array[Byte], key: String, mode: Int): String = {
    val algorithm = "AES"
    val cipherName = algorithm + "/ECB/NoPadding"
    val keyBytes = (key map (_.toByte)).toArray
    val secretKey = new SecretKeySpec(keyBytes, algorithm)

    val cipher = Cipher.getInstance(cipherName)
    cipher.init(mode, secretKey)
    Helpers.bytesToString(cipher.doFinal(bytes))
  }

  def hashWithRotatingKey(bytes: Array[Byte], key: String): String =
    bytes zip Helpers.rotatingKey(key) map xorTuple map byteToHex mkString ""

  private def byteToHex(byte: Int): String = {
    val hex = byte.toHexString
    if (hex.size == 1) "0" + hex else hex
  }

  def solveRotatingKey(bytes: Array[Byte], key: String): String =
    Helpers.intsToString(bytes zip Stream.continually(key.toCharArray).flatten map xorTuple)

  private def xorTuple: PartialFunction[(Byte, Char), Int] = {
    case (a, b) => a ^ b
  }

  def solveSingleByteXor(bytes: String): Char =
    solveSingleByteXor(bytes.toCharArray map (_.toByte))

  def solveSingleByteXor(bytes: Seq[Byte]): Char =
    solveSingleByteXor(bytes.toArray)

  def solveSingleByteXor(bytes: Array[Byte]): Char = {
    val found = Helpers.alphabet map { char =>
      val stringified = Helpers.intsToString(Xor.xorWith(bytes, char))
      (char.toChar, Helpers.scoreString(stringified))
    } maxBy (_._2)

    found._1
  }

}
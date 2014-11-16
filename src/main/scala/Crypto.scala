package crypto

import helpers.Helpers

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

object Crypto {

  def decryptAESECB(bytes: Array[Byte], key: String): String = {
    val algorithm = "AES"
    val cipherName = algorithm + "/ECB/PKCS5Padding"
    val secretKey = new SecretKeySpec(key.getBytes("UTF8"), algorithm)

    val decryptor = Cipher.getInstance(cipherName)
    decryptor.init(Cipher.DECRYPT_MODE, secretKey)
    Helpers.bytesToString(decryptor.doFinal(bytes))
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
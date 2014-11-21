package crypto

import helpers.{Transformers, Helpers}

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import scala.util.Random

object Crypto {

  def findRepeatingBlockScore(string: String, size: Int): Double =
    (string grouped size).toSet.size.toDouble/size

  def findRepeatingBlockScore[T](array: Array[T], size: Int): Double =
    (array grouped size).toSet.size.toDouble/size

  private case class DecryptECB(padding: String, acc: String) {
    def shift(char: Char): DecryptECB =
      copy((padding drop 1) + char, acc + char)
  }

  def decryptECBBlackBox(blockSize: Int, blackBox: String => String): String = {
    val decrypted = blackBox("").zipWithIndex.foldLeft(DecryptECB("A" * 15, "")) {
      case (acc @ DecryptECB(padding, _), (_, i)) =>
        val char = (0 to 256) map (_.toChar) find { char =>
          val knownHead = padding + char
          val shiftingHead = padding take (blockSize - 1 - (i % blockSize))

          val encrypted = blackBox(knownHead + shiftingHead)
          val head = encrypted take blockSize
          val compare = encrypted drop (i/blockSize * blockSize + blockSize) take blockSize

          head == compare
        }

        char map acc.shift getOrElse {
          throw new Error(s"Could not find matching byte for character $i in appended string")
        }
    }

    decrypted.acc
  }

  private case class CBCAcc(iv: Array[Byte], output: Array[Byte] = Array())

  def decryptCBC(bytes: Array[Byte], key: String, iv: Array[Byte]): String = {
    val cbc = (bytes grouped key.size).foldLeft(CBCAcc(iv)) {
      case (CBCAcc(vector, output), cipherText) =>
        val decrypted = decryptECB(cipherText, key) map (_.toByte)
        CBCAcc(cipherText, output ++ Xor.xorBytes(decrypted.toArray, vector))
    }

    cbc.output map (_.toChar) mkString ""
  }

  def encryptCBC(bytes: Array[Byte], key: String, iv: Array[Byte]): String = {
    val cbc = (bytes grouped key.size).foldLeft(CBCAcc(iv)) {
      case (CBCAcc(vector, output), plainText) =>
        val xored = Xor.xorBytes(plainText, vector)
        val cipherText = (encryptECB(xored, key) map (_.toByte)).toArray
        CBCAcc(cipherText, output ++ cipherText)
    }

    cbc.output map (_.toChar) mkString ""
  }

  def encryptECB(bytes: Array[Byte], key: String): String =
    aesECB(bytes, key, Cipher.ENCRYPT_MODE)

  def decryptECB(bytes: Array[Byte], key: String): String =
    aesECB(bytes, key, Cipher.DECRYPT_MODE)

  private def aesECB(bytes: Array[Byte], key: String, mode: Int): String = {
    val algorithm = "AES"
    val cipherName = algorithm + "/ECB/NoPadding"
    val keyBytes = (key map (_.toByte)).toArray
    val secretKey = new SecretKeySpec(keyBytes, algorithm)
    val padded = Helpers.padToMultiple(bytes, key.size, Byte.box(0))

    val cipher = Cipher.getInstance(cipherName)
    cipher.init(mode, secretKey)
    Transformers.bytesToString(cipher.doFinal(padded))
  }

  def hashWithRotatingKey(bytes: Array[Byte], key: String): String =
    bytes zip Helpers.rotatingKey(key) map xorTuple map byteToHex mkString ""

  private def byteToHex(byte: Int): String = {
    val hex = byte.toHexString
    if (hex.size == 1) "0" + hex else hex
  }

  def solveRotatingKey(bytes: Array[Byte], key: String): String =
    Transformers.intsToString(bytes zip Stream.continually(key.toCharArray).flatten map xorTuple)

  private def xorTuple: PartialFunction[(Byte, Char), Int] = {
    case (a, b) => a ^ b
  }

  def solveSingleByteXor(bytes: String): Char =
    solveSingleByteXor(bytes.toCharArray map (_.toByte))

  def solveSingleByteXor(bytes: Seq[Byte]): Char =
    solveSingleByteXor(bytes.toArray)

  def solveSingleByteXor(bytes: Array[Byte]): Char = {
    val found = Helpers.alphabet map { char =>
      val stringified = Transformers.intsToString(Xor.xorWith(bytes, char))
      (char.toChar, Helpers.scoreString(stringified))
    } maxBy (_._2)

    found._1
  }

}
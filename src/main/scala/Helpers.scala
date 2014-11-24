package helpers

import scala.util.Random
import scala.math.BigInt

object Math {

  // function modular_pow(base, exponent, modulus)
  //     Assert :: (modulus - 1) * (modulus - 1) does not overflow base
  //     result := 1
  //     base := base mod modulus
  //     while exponent > 0
  //         if (exponent mod 2 == 1):
  //            result := (result * base) mod modulus
  //         exponent := exponent >> 1
  //         base := (base * base) mod modulus
  //     return result

  def modeExp(base: BigInt, exponent: Int, modulus: Int): Int = {

  }

  private def reduceUntil

}

object Helpers {

  val alphabet = Range(0, 256)

  private val freq = "etetaoin shrdlcumwfgypbvkjxqz".toCharArray
  private val freqMap = freq.reverse.zipWithIndex.toMap

  def randomKey(size: Int): Array[Byte] =
    Array.fill(size)(Random.nextInt(256)) map (_.toByte)

  def padToMultiple(bytes: Array[Byte], factor: Int, padChar: Byte = Byte.box(0)): Array[Byte] =
    bytes.size % factor match {
      case 0 => bytes
      case i => padBlock(bytes, bytes.size + factor - i, padChar)
    }

  def padBlock(bytes: Array[Byte], length: Int, padChar: Byte = Byte.box(4)): Array[Byte] =
    ((0 until length) map (bytes lift _ getOrElse padChar)).toArray

  def transposeGrouped[A](list: Seq[A], size: Int): Seq[Seq[A]] = {
    val grouped = (list grouped size).toSeq
    Range(0, size) map { i =>
      (grouped flatMap (_ lift i)).toSeq
    }
  }

  def editDistance(string1: String, string2: String): Int =
    editDistance(string1.toCharArray map (_.toByte), string2.toCharArray map (_.toByte))

  def editDistance(string1: Array[Byte], string2: Array[Byte]): Int = {
    if (string1.size != string2.size) throw new Error("String sizes don't match")

    val bitSetList = string1 zip string2 map {
      case (a, b) => numberOfBitsSet(a ^ b)
    }

    bitSetList.sum
  }

  def scoreString(string: String): Int =
    string.toCharArray.foldLeft(0) { (sum, char) =>
      sum + freqMap.getOrElse(char, 0)
    }

  private def numberOfBitsSet(i: Int): Int = {
    val byte = i.toByte
    (0 to 7).map(i => (byte >>> i) & 1).sum
  }

  def rotatingKey(string: String): Stream[Char] =
    Stream.continually(string.toCharArray).flatten

}
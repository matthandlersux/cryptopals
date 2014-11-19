package helpers

import sun.misc.{BASE64Encoder, BASE64Decoder}
import scala.util.Random

object Transformers {

  def bytesToString(bytes: Array[Byte]): String =
    bytes map (_.toChar) mkString ""

  def intsToString(ints: Array[Int]): String =
    ints map (_.toChar) mkString ""

  def hexToBytes(string: String): Array[Byte] =
    (string grouped 2 map parseHex map (_.toByte)).toArray

  def encode64(ints: Seq[Int]): String =
    new BASE64Encoder().encodeBuffer((ints map (_.toByte)).toArray)

  def encode64(string: String): String =
    new BASE64Encoder().encodeBuffer(string.getBytes)

  def decode64(string: String): Array[Byte] =
    new BASE64Decoder().decodeBuffer(string)

  def parseHex(string: String): Int =
    Integer.parseInt(string, 16)

}
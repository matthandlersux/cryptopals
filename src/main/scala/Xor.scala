package crypto

import helpers.{Transformers, Helpers}
import scala.util.Try

object Xor {

  def xorBytes(a: Array[Byte], b: Array[Byte]): Array[Byte] =
    a zip Stream.continually(b).flatten map {
      case (a, b) => a ^ b
    } map (_.toByte)

  def xorHexStrings(a: String, b: String): String =
    hexStringToIntArray(a) zip hexStringToIntArray(b) map {
      case (a, b) => a ^ b
    } map (_.toHexString) mkString ""

  private def hexStringToIntArray(string: String): Iterator[Int] =
    string grouped 2 map Transformers.parseHex

  def xorWith(hash: String, char: Int): Array[Int] =
    hash.grouped(2).map(hex => Try(Integer.parseInt(hex, 16)) getOrElse 0).map(_ ^ char).toArray

  def xorWith(bytes: Array[Byte], char: Int): Array[Int] =
    bytes.map(_ ^ char.toByte).toArray

}

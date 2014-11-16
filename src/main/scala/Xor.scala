package crypto

import helpers.Helpers
import scala.util.Try

object Xor {

  def xorStrings(a: String, b: String): String =
    toIntArray(a) zip toIntArray(b) map {
      case (a, b) => a ^ b
    } map (_.toHexString) mkString ""

  private def toIntArray(string: String): Iterator[Int] =
    string grouped 2 map Helpers.parseHex

  def xorWith(hash: String, char: Int): Array[Int] =
    hash.grouped(2).map(hex => Try(Integer.parseInt(hex, 16)) getOrElse 0).map(_ ^ char).toArray

  def xorWith(bytes: Array[Byte], char: Int): Array[Int] =
    bytes.map(_ ^ char.toByte).toArray

}

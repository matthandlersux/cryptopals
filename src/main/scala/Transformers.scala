package helpers

import sun.misc.{BASE64Encoder, BASE64Decoder}
import scala.util.Random
import play.api.libs.json.{JsObject, Json}
import java.net.{URLEncoder, URLDecoder}

object Transformers {

  private val base64Chars =
    ((65 to 90) ++ (97 to 122) ++ (48 to 57) :+ 43 :+ 47 :+ 61) map (_.toChar)

  private val base64DecodingMap =
    base64Chars.zipWithIndex.toMap

  private val base64EncodingMap =
    base64Chars.zipWithIndex.map(_.swap).toMap

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

  def manualEncode64(string: Array[Byte]): String =
    string grouped 3 flatMap {
      case Array(a, b, c) => convert3BytesTo4(a, b, c)
      case Array(a, b) => convert3BytesTo4(a, b, Byte.box(0)).take(3) :+ 64
      case Array(a) => convert3BytesTo4(a, Byte.box(0), Byte.box(0)).take(2) ++ Array(64, 64)
    } map base64EncodingMap mkString ""

  private def convert3BytesTo4(a: Byte, b: Byte, c: Byte): Array[Int] =
    Array(a >>> 2, ((a & 3) << 4) + (b >>> 4), ((b & 15) << 2) + (c >>> 6), c & 63)

  def manualDecode64(string: String): Array[Byte] =
    (string.toCharArray map base64DecodingMap map (_.toByte) grouped 4 flatMap {
      case Array(a, b, c, d) if c == 64 && d == 64 => convert4BytesTo3(a, b, 0, 0) take 1
      case Array(a, b, c, d) if d == 64 => convert4BytesTo3(a, b, c, 0) take 2
      case Array(a, b, c, d) => convert4BytesTo3(a, b, c, d)
    }).toArray

  private def convert4BytesTo3(a: Byte, b: Byte, c: Byte, d: Byte): Array[Byte] =
    Array((a << 2) + (b >>> 4), ((b & 15) << 4) + (c >>> 2), ((c & 3) << 6) + d) map (_.toByte)

  def decode64(string: String): Array[Byte] =
    new BASE64Decoder().decodeBuffer(string)

  def parseHex(string: String): Int =
    Integer.parseInt(string, 16)

  def parseQueryString(string: String): Option[JsObject] = {
    val Pattern = """([^=]+)=(.*)""".r
    val pairs = string split '&' collect {
      case Pattern(key, value) => (key, URLDecoder.decode(value))
    }

    Json.toJson(pairs.toMap).asOpt[JsObject]
  }

  def jsonToQueryString(json: JsObject): String =
    json.fields map { case (key, jsValue) =>
      key + '=' + URLEncoder.encode(jsValue.toString)
    } mkString "&"

}
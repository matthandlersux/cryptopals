package crypto

object Crypto {

  def solveRotatingCypher(bytes: Array[Byte], cipher: String): String =
    bytes zip Stream.continually(cipher.toCharArray).flatten map {
      case (a, b) => a ^ b
    } map (_.toChar) mkString ""

  def solveSingleByteXor(bytes: String): Char =
    solveSingleByteXor(bytes.toCharArray map (_.toByte))

  def solveSingleByteXor(bytes: Seq[Byte]): Char =
    solveSingleByteXor(bytes.toArray)

  def solveSingleByteXor(bytes: Array[Byte]): Char = {
    val found = Range(0, 256) map { char =>
      val stringified = Xor.xorWith(bytes, char) map (_.toChar) mkString ""
      (char.toChar, scoreString(stringified))
    } maxBy (_._2)

    found._1
  }

}
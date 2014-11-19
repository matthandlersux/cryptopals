package rounds

import crypto.{Crypto, Xor}
import data.Data
import helpers.Helpers

object Round1 {

  def problem1: String =
    encodeHexStringToBase64(
      "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    )

  private def encodeHexStringToBase64(string: String): String =
    Helpers.encode64((string grouped 2 map Helpers.parseHex).toSeq)

  def problem2: String = {
    val string1 = "1c0111001f010100061a024b53535009181c"
    val string2 = "686974207468652062756c6c277320657965"
    Xor.xorStrings(string1, string2)
  }

  def problem3: (Char, String) = {
    val hash = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    val key = Crypto.solveSingleByteXor(Helpers.hexToBytes(hash))

    (key, Xor.xorWith(hash, key) map (_.toChar) mkString "")
  }

  def problem4: (Char, String, Int) = {
    (for {
      line <- Data.Round1.problem4.split("\n")
      char <- Helpers.alphabet
    } yield {
      val intList = Xor.xorWith(line, char)
      val stringified = intList map (_.toChar) mkString ""
      (char.toChar, stringified, Helpers.scoreString(stringified))
    }) maxBy (_._3)
  }

  def problem5: String = {
    val key = "ICE"
    val text = """Burning 'em, if you ain't quick and nimble
      |I go crazy when I hear a cymbal""".stripMargin

    Crypto.hashWithRotatingKey(text.toCharArray map (_.toByte), key)
  }

  def problem6: String = {
    val keySizes = Range(2, 41)

    val bytes = Helpers.decode64(Data.Round1.problem6)
    val keySize = getKeySize(bytes, keySizes)

    val transposed = Helpers.transposeGrouped(bytes, keySize)
    val key = transposed map Crypto.solveSingleByteXor

    Crypto.solveRotatingKey(bytes, key mkString "")
  }

  private def getKeySize(string: Array[Byte], range: Range): Int =
    (range map { keySize =>
      val grouped = (string grouped keySize take 4).toSeq
      val distances = for {
        a <- grouped
        b <- grouped if b != a
      } yield Helpers.editDistance(a, b).toDouble

      (keySize, distances.sum/(keySize * distances.size))
    } minBy (_._2))._1

  def problem7: String = {
    val bytes = Helpers.decode64(Data.Round1.problem7)
    bytes grouped 16 map (Crypto.decryptECB(_, "YELLOW SUBMARINE")) mkString ""
  }

  def problem8: Int = {
    val hexes = Data.Round1.problem8.split("\n")
    val found = hexes.zipWithIndex map { case (hex, i) =>
      (i, Crypto.findRepeatingBlockScore(hex, 16))
    } minBy (_._2)

    found._1
  }

}
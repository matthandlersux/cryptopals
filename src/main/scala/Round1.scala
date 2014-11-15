package rounds

import crypto.{Crypto, Xor}
import data.Data

object Round1 {

  def problem1: Unit = {

  }

  def problem2: String = {
    val string1 = "1c0111001f010100061a024b53535009181c"
    val string2 = "686974207468652062756c6c277320657965"
    // val string3 = "746865206b696420646f6e277420706c6179"

    Xor.xorStrings(string1, string2)
  }

  def problem3: Unit = {
    val hash = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    Crypto.solveSingleByteXor(hash)
  }

  def problem4: Unit = {
    (for {
      line <- Data.problem4.split("\n")
      char <- alphabet.toCharArray.map(_.toInt)
    } yield {
      val intList = Xor.xorWith(line, char)
      val stringified = intList map (_.toChar) mkString ""
      (char.toChar, stringified, scoreString(stringified))
    }) maxBy (_._3)
  }

  def problem5: Unit = {
    private val key = "ICE"
    private val keyStream = Stream.continually(key.toCharArray).flatten

    def run: Unit = {
      val hashed = Data.problem5a zip cipherStream map {
        case (char, cipherChar) => char.toInt ^ cipherChar.toInt
      }

      println(hashed map (_.toHexString) mkString "")
      assert(hashed == "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")
    }
  }

  def problem6: String = {
    val keySizes = Range(2, 41)

    val bytes = Helpers.decode64(Data.problem6)
    val keySize = getKeySize(bytes)

    val transposed = Helpers.transposeGrouped(bytes, keySize)
    val cipher = transposed map solveSingleByteXor

    Crypto.solveRotatingCypher(bytes, cipher mkString "")
  }

  private def getKeySize(string: Array[Byte]): Int =
    (keySizes map { keySize =>
      val grouped = (string grouped keySize take 4).toSeq
      val distances = for {
        a <- grouped
        b <- grouped if b != a
      } yield editDistance(a, b).toDouble

      (keySize, distances.sum/(keySize * distances.size))
    } minBy (_._2))._1

  def problem7: Unit = {
    val key = "YELLOW SUBMARINE"
    val bytes = decode64(Data.problem7)
  }

  def problem8: Unit = {

  }

}
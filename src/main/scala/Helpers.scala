package helpers

import sun.misc.{BASE64Encoder, BASE64Decoder}

object Helpers {

	private val freq = "etetaoin shrdlcumwfgypbvkjxqz".toCharArray
	private val freqMap = freq.reverse.zipWithIndex.toMap

	def encode64(string: String): String = (new BASE64Encoder().encodeBuffer(string.getBytes))

	def decode64(string: String): Array[Byte] = (new BASE64Decoder().decodeBuffer(string))

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

	def parseHex(string: String): Int =
		Integer.parseInt(string, 16)

	def rotatingKey(string: String): Stream[Char] =
		Stream.continually(string.toCharArray).flatten

}
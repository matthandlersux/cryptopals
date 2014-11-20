package test.unit

import helpers.Transformers

import org.scalatest._
import Matchers._
import scala.util.Random

class TransformersSpec extends FreeSpec {

  "base 64" - {

    "it can decode its own encoding" in {
      val input = Array.fill(20)(Random.nextInt(128)) map (_.toByte)
      Transformers.manualDecode64(Transformers.manualEncode64(input)) shouldEqual input
    }

    ".manualDecode64" - {

      def decode(string: String): String =
        Transformers.manualDecode64(string) map (_.toChar) mkString ""

      "when string has no = at the end" - {

        "decodes a string" in {
          decode("TWFuIGlzIGRpc3Rp") shouldEqual "Man is disti"
        }

      }

      "when string has 1 = at the end" - {

        "decodes a string" in {
          decode("TWFuIGlzIGR=") shouldEqual "Man is d"
        }

      }

      "when string has 2 = at the end" - {

        "decodes a string" in {
          decode("TWFuIGlzIG==") shouldEqual "Man is "
        }

      }

    }

    ".manualEncode64" - {

      def encode(string: String): String =
        Transformers.manualEncode64((string map (_.toByte)).toArray)


      "when string has 1 missing byte" - {

        "encodes a string" in {
          val string = "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."
          val fixture = "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4="

          encode(string) shouldEqual fixture
        }

        "adds = at the end" in {
          val string = "abcde"
          encode(string) should endWith ("=")
          encode(string) shouldNot endWith ("==")
        }

      }

      "when string has 2 missing bytes" - {

        "adds == at the end" in {
          encode("abcd") should endWith ("==")
        }

      }

      "when string size is divisible by 3" - {

        "encodes a string" in {
          encode("abcdef") shouldEqual "YWJjZGVm"
        }

        "does not add = at the end" in {
          encode("abcdef") shouldNot contain ("=")
        }

      }

    }

  }


}

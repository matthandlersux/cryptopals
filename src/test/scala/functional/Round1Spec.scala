package test.functional

import rounds.Round1

import scala.io.Source
import org.scalatest._
import Matchers._

class RoundOneSpec extends FreeSpec {

  "problem 1" - {

    "encodes a string in base64" in {
      Round1.problem1 should startWith ("SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
    }

  }

  "problem 2" - {

    "xors strings together" in {
      Round1.problem2 shouldEqual "746865206b696420646f6e277420706c6179"
    }

  }

  "problem 3" - {

    "finds the single character xor key" in {
      Round1.problem3._1 shouldEqual 'X'
    }

    "decodes the encrypted text" in {
      Round1.problem3._2 shouldEqual "Cooking MC's like a pound of bacon"
    }

  }

  "problem 4" - {

    "finds a line with english text" in {
      Round1.problem4._2 should startWith ("Now that the party is jumping")
    }

  }

  "problem 5" - {

    "encodes the text" in {
      Round1.problem5 shouldEqual "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
    }

  }

  "problem 6" - {

    "decodes the paragraph" in {
      val fixture = Source.fromFile("src/test/scala/fixtures/problem6.output.txt").mkString
      Round1.problem6 shouldEqual fixture
    }

  }

  "problem 7" - {

    "decodes AES ECB encrypted text" in {
      val fixture = Source.fromFile("src/test/scala/fixtures/problem6.output.txt").mkString
      Round1.problem7 shouldEqual fixture
    }

  }

}
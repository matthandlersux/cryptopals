package test.functional

import rounds.Round1

import org.scalatest._
import Matchers._

class RoundOneSpec extends FreeSpec {

  "Problem 1" - {

    "encodes a string in base64" in {
      Round1.problem1 should startWith ("SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
    }

  }

  "Problem 2" - {

    "xors strings together" in {
      Round1.problem2 shouldEqual "746865206b696420646f6e277420706c6179"
    }

  }

  "Problem 3" - {

    "finds the single character xor key" in {
      Round1.problem3._1 shouldEqual 'X'
    }

    "decodes the encrypted text" in {
      Round1.problem3._2 shouldEqual "Cooking MC's like a pound of bacon"
    }

  }

}
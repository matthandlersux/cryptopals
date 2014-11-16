package test.functional

import rounds.Round2

import org.scalatest._
import Matchers._

class Round2Spec extends FreeSpec {

  "problem 1" - {

    "pads a string with characters" in {
      Round2.problem1 shouldEqual "YELLOW SUBMARINE\u0004\u0004\u0004\u0004"
    }

  }

}
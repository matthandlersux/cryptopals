package test.functional

import rounds.Round2

import org.scalatest._
import Matchers._
import scala.io.Source

class Round2Spec extends FreeSpec {

  "problem 9" - {

    "pads a string with characters" in {
      Round2.problem9 shouldEqual "YELLOW SUBMARINE\u0004\u0004\u0004\u0004"
    }

  }

  "problem 10" - {

    "encrypts ecb" in {
      val fixture = Source.fromFile("src/test/scala/fixtures/problem6.output.txt").mkString
      Round2.problem10 should startWith (fixture)
    }

  }

  "problem 11" - {

    "blah" in {
      Round2.problem11
    }

  }
}
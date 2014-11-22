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

    "finds that half of the runs were ECB encrypted" in {
      val score = Round2.problem11
      score should be < 0.6
      score should be > 0.4
    }

  }

  "problem 12" - {

    "can read an unknown string via ECB encryption" in {
      val fixture = Source.fromFile("src/test/scala/fixtures/problem12.output.txt").mkString
      Round2.problem12 should startWith (fixture)
    }

  }

  "problem 13" - {

    "injects a role=admin key into parsed json block" in {
      val json = Round2.problem13.get

      (json \ "role").as[String] shouldEqual "admin"
    }

  }
}
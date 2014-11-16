package data

import scala.io.Source

object Data {

  private def readFile(name: String): String =
    Source.fromFile("data/" + name) mkString ""

  lazy val problem4 = readFile("problem4.txt")
  lazy val problem6 = readFile("problem6.txt")
  lazy val problem7 = readFile("problem7.txt")
  lazy val problem8 = readFile("problem8.txt")

}
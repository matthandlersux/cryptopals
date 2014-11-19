package data

import scala.io.Source

object Data {

  abstract class FileReader(folder: String) {
    def readFile(name: String): String =
      Source.fromFile("data/" + folder + "/" + name) mkString ""
  }

  object Round1 extends FileReader("round1") {
    lazy val problem4 = readFile("problem4.txt")
    lazy val problem6 = readFile("problem6.txt")
    lazy val problem7 = readFile("problem7.txt")
    lazy val problem8 = readFile("problem8.txt")
  }

  object Round2 extends FileReader("round2") {
    lazy val problem10 = readFile("problem10.txt")
    lazy val problem12 = readFile("problem12.txt")
  }

}
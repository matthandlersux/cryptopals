package rounds

import helpers.Helpers

object Round2 {

	def problem1: String = {
		val string = "YELLOW SUBMARINE"
		Helpers.padBlock(string.getBytes, 20) map (_.toChar) mkString ""
	}


}
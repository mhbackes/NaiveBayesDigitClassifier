package classifier.bayes.naive.digit

/**
 * Created by mhbackes on 19/05/15.
 */
class Digit(private val _id: Int,
            private val _pixels: Array[Double]) {

  private def parseData(data: String) {
    val dataNumbers: Array[String] = data.split(" ,")
    for (i <- 0 until Digit.MAX_PIXEL) {
      _pixels(i) = dataNumbers(i).toDouble
    }
  }

  def id: Int = _id

  def pixels: Array[Double] = _pixels

  def pixel(i: Int): Double = _pixels(i)
}

object Digit {

  val MAX_PIXEL: Int = 256
  val MAX_DIGIT: Int = 10

  def apply(data: String): Digit = {
    val dataNumbers: Array[String] = data.split(",")
    val pixels: Array[Double] = new Array[Double](256)
    for (i <- 0 until MAX_PIXEL) {
      pixels(i) = dataNumbers(i).toDouble
    }
    val digit = dataNumbers(MAX_PIXEL).toInt
    new Digit(digit, pixels)
  }

}
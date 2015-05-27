package classifier.bayes.naive.digit

import scala.collection.mutable.ArrayBuffer

/**
 * Created by mhbackes on 19/05/15.
 */
class DigitSet(private val _digits: Array[Array[Digit]],
               private val _size: Int) {

  def digits = _digits

  def size = _size

  def mean(digitId: Int, pixel: Int): Double = {
    val sum = _digits(digitId).foldRight(0.0)((d, s) => s + d.pixel(pixel))
    sum / _digits(digitId).size
  }

  def variance(digitId: Int, pixel: Int): Double = {
    val mean = this.mean(digitId, pixel)
    val sumDiff = _digits(digitId).foldRight(0.0)((d, sd) => {
      val diff = d.pixel(pixel) - mean
      sd + diff * diff
    })
    sumDiff / _digits(digitId).size
  }

  def density(digitId: Int): Double = {
    _digits(digitId).size.toDouble / size.toDouble
  }

  def meanVariance(digitId: Int, pixel: Int): (Double, Double) = {
    val mean: Double = this.mean(digitId, pixel)
    val sumDiff: Double = _digits(digitId).foldRight(0.0)((d, sd) => {
      val diff: Double = d.pixel(pixel) - mean
      sd + diff * diff
    })
    val variance: Double = sumDiff / _digits(digitId).size
    (mean, variance)
  }

}

object DigitSet {

  def apply(data: Array[String]): DigitSet = {
    val digits = Array.fill(Digit.MAX_DIGIT)(ArrayBuffer[Digit]())
    val digitsImmutable = new Array[Array[Digit]](Digit.MAX_DIGIT)
    data.foreach { i => parseDigit(digits, i) }
    for (i <- 0 until Digit.MAX_DIGIT) {
      digitsImmutable(i) = digits(i).toArray
    }
    new DigitSet(digitsImmutable, data.size)
  }

  private def parseDigit(digits: Array[ArrayBuffer[Digit]], data: String) {
    val digit: Digit = Digit(data)
    digits(digit.id) += digit
  }

  def apply(digitSets: Iterable[DigitSet]): DigitSet = {
    val digits = Array.fill(Digit.MAX_DIGIT)(ArrayBuffer[Digit]())
    val digitsImmutable = new Array[Array[Digit]](Digit.MAX_DIGIT)
    var size: Int = 0
    digitSets.foreach { ds =>
      ds.digits.foreach { dss =>
        dss.foreach { d =>
          parseDigit(digits, d)
          size = size + 1
        }
      }
    }

    for (i <- 0 until Digit.MAX_DIGIT) {
      digitsImmutable(i) = digits(i).toArray
    }
    new DigitSet(digitsImmutable, size)
  }

  def parseDigit(digits: Array[ArrayBuffer[Digit]], digit: Digit) {
    digits(digit.id) += digit
  }

}

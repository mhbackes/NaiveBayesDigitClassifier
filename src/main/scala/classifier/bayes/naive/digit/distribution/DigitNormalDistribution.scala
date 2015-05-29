package classifier.bayes.naive.digit.distribution

import classifier.bayes.naive.digit.{DigitSet, Digit}

import scala.math.log

/**
 * Created by mhbackes on 19/05/15.
 */
class DigitNormalDistribution(private var _pixels: Array[PixelNormalDistribution],
                              private var _prior: Double) {

  def logPosterior(digit: Digit): Double = {
    logPrior + logLikelihood(digit)
  }

  def prior = _prior

  def logPrior = log(_prior)

  def logLikelihood(digit: Digit): Double = {
    (_pixels, digit.pixels).zipped.foldRight(0.0) { case ((pnd, p), sum) => pnd.logLikelihood(p) + sum }
  }

}

object DigitNormalDistribution {

  def apply(digitSet: DigitSet, digitId: Int): DigitNormalDistribution = {
    val pixels = new Array[PixelNormalDistribution](Digit.MAX_PIXEL)
    for (i <- 0 until Digit.MAX_PIXEL) {
      pixels(i) = PixelNormalDistribution(digitSet, digitId, i)
    }
    new DigitNormalDistribution(pixels, digitSet.density(digitId))
  }

}

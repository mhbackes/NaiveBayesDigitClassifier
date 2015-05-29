package classifier.bayes.naive.digit.distribution

import classifier.bayes.naive.digit.DigitSet
import scala.math.{exp, sqrt, Pi, log}

/**
 * Created by mhbackes on 19/05/15.
 */
class PixelNormalDistribution(private val _mean: Double,
                              private val _variance: Double) {
  def mean = _mean

  def variance = _variance

  def likelihood(x: Double): Double = {
    val diff = x - _mean
    exp(-(diff * diff) / (2 * _variance)) / sqrt(2 * Pi * _variance)
  }

  def logLikelihood(x: Double): Double = log(likelihood(x))

}

object PixelNormalDistribution {
  def apply(digitSet: DigitSet, digitId: Int, pixel: Int): PixelNormalDistribution = {
    val (mean, variance) = digitSet.meanVariance(digitId, pixel)
    new PixelNormalDistribution(mean, variance)
  }
}

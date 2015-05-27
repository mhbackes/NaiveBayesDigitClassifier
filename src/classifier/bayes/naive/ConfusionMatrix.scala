package classifier.bayes.naive

import classifier.bayes.naive.digit.{DigitSet, Digit}
import classifier.bayes.naive.digit.parser.DigitSetParser

/**
 * Created by mhbackes on 20/05/15.
 */
class ConfusionMatrix(private val _bayes: NaiveBayesDigitClassifier,
                      private val _cells: Array[Int]) {

  def cell(i: Int, j: Int) = _cells(ConfusionMatrix.index(i, j))

  def total = _cells.foldRight(0)((a, b) => a + b)

  def accuracy: Double = {
    var truePredictions = 0.0
    for(i <- 0 until Digit.MAX_DIGIT){
      truePredictions += cell(i, i)
    }
    truePredictions / total
  }

  def precision(i: Int): Double = {
    var rowSum: Double = 0.0
    for (j <- 0 until Digit.MAX_DIGIT) {
      rowSum += cell(j, i)
    }
    cell(i, i) / rowSum
  }

  def recall(i: Int): Double = {
    var lineSum: Double = 0.0
    for (j <- 0 until Digit.MAX_DIGIT) {
      lineSum += cell(i, j)
    }
    cell(i, i) / lineSum
  }

  def fScore(i: Int) = (2 * recall(i) * precision(i)) / (recall(i) + precision(i))

  def metrics(i: Int): (Double, Double, Double) = {
    val precision_ = precision(i)
    val recall_ = recall(i)
    val fScore_ = ConfusionMatrix.fScore(recall_, precision_)
    (precision_, recall_, fScore_)
  }

  private def incCell(i: Int, j: Int) = _cells(ConfusionMatrix.index(i, j)) += 1

  private def test(digit: Digit) {
    val predictedId = _bayes.classify(digit)
    incCell(digit.id, predictedId)
  }

  private def test(digitSet: DigitSet) {
    digitSet.digits.foreach(ds => ds.foreach(d => test(d)))
  }

}

object ConfusionMatrix {
  def apply(bayes: NaiveBayesDigitClassifier, digitSet: DigitSet): ConfusionMatrix = {
    val cells = new Array[Int](Digit.MAX_DIGIT * Digit.MAX_DIGIT)
    val matrix = new ConfusionMatrix(bayes, cells)
    matrix.test(digitSet)
    matrix
  }

  private def index(i: Int, j: Int): Int = Digit.MAX_DIGIT * j + i

  private def fScore(recall: Double, precision: Double) = (2 * recall * precision) / (recall + precision)

}
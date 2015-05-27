package classifier.bayes.naive

import classifier.bayes.naive.digit.distribution.DigitNormalDistribution
import classifier.bayes.naive.digit.parser.DigitSetParser
import classifier.bayes.naive.digit.{Digit, DigitSet}

/**
 * Created by mhbackes on 19/05/15.
 */
class NaiveBayesDigitClassifier(private val _digits: Array[DigitNormalDistribution]) {

  def classify(digit: Digit): Int = {
    var (maximumAPosteriori, digitId) = (_digits(0).logPosterior(digit), 0)
    for (i <- 1 until Digit.MAX_DIGIT) {
      val posterior = _digits(i).logPosterior(digit)
      if (maximumAPosteriori < posterior) {
        maximumAPosteriori = posterior
        digitId = i
      }
    }
    digitId
  }

}

object NaiveBayesDigitClassifier {

  def apply(digitSet: DigitSet): NaiveBayesDigitClassifier = {
    val digits = new Array[DigitNormalDistribution](Digit.MAX_DIGIT)
    for (i <- 0 until Digit.MAX_DIGIT) {
      digits(i) = DigitNormalDistribution(digitSet, i)
    }
    new NaiveBayesDigitClassifier(digits)
  }

  val K_FOLD = 10

  def main(args: Array[String]) {
    if (args.size != 2) usage

    val digitSetParser = new DigitSetParser(args(0), args(1))
    val digitSets = digitSetParser.parseAsSubsets(K_FOLD)

    var sumAccuracy: Double = 0.0
    val sumPrecision = new Array[Double](Digit.MAX_DIGIT)
    val sumRecall = new Array[Double](Digit.MAX_DIGIT)
    val sumFScore = new Array[Double](Digit.MAX_DIGIT)

    for(i <- 0 until K_FOLD){
      val testSet = digitSets.dequeue()
      val trainingSet = DigitSet(digitSets)
      val nbDigitClassifier = NaiveBayesDigitClassifier(trainingSet)
      val confusionMatrix = ConfusionMatrix(nbDigitClassifier, testSet)
      val accuracy = confusionMatrix.accuracy
      sumAccuracy += accuracy
      println("Iteration: " + i + " | Accuracy: " + accuracy)
      println("Digit\tPrecision\t\tRecall\t\t\tF-Score")
      for(j <- 0 until Digit.MAX_DIGIT) {
        val (precision, recall, fScore) = confusionMatrix.metrics(j)
        sumPrecision(i) += precision
        sumRecall(i) += recall
        sumFScore(i) += fScore
        printf("%d\t%.16g\t%.16g\t%.16g\n", j, precision, recall, fScore)
      }
      println("\n")
      digitSets.enqueue(testSet)
    }

    val avgAccuracy = sumAccuracy / K_FOLD
    val avgPrecision = new Array[Double](Digit.MAX_DIGIT)
    val avgRecall = new Array[Double](Digit.MAX_DIGIT)
    val avgFScore = new Array[Double](Digit.MAX_DIGIT)
    for(i <- 0 until Digit.MAX_DIGIT){
      avgPrecision(i) = sumPrecision(i) / K_FOLD
      avgRecall(i) = sumRecall(i) / K_FOLD
      avgFScore(i) = sumFScore(i) / K_FOLD
    }
    println("Final Result | Average Accuracy: " + avgAccuracy)
    println("Digit\tAverage Precision\tAvgerage Recall \tAvgerage F-Score")
    for(i <- 0 until Digit.MAX_DIGIT){
      printf("%d\t%.16g\t%.16g\t%.16g\n", i, avgPrecision(i), avgRecall(i), avgFScore(i))
    }

  }

  private def usage {
    println("usage: java NBDigitClassifier.jar dataFile indexFile")
    println("\tdataFile: the USPS digit image file in .csv format.")
    println("\tindexFile: the index file that indicates which lines of the dataFile will be read.")
    sys.exit(-1)
  }

}

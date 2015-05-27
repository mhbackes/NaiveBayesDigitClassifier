package classifier.bayes.naive.digit.parser

import classifier.bayes.naive.digit.{Digit, DigitSet}
import scala.collection.mutable.Queue
import scala.io.Source

/**
 * Created by mhbackes on 19/05/15.
 */
class DigitSetParser(private var _dataFilePath: String,
                     private var _indexFilePath: String) {

  def parseDataIndex: Array[Int] = {
    val indexStr = Source.fromFile(_indexFilePath).getLines().toArray
    indexStr.map(s => s.toInt)
  }

  def parseDataLines(dataIndex: Array[Int]): Array[String] = {
    val allDataLines = Source.fromFile(_dataFilePath).getLines().toArray
    val selectedDataLines = new Array[String](dataIndex.size)
    for (i <- 0 until dataIndex.size)
      selectedDataLines(i) = allDataLines(dataIndex(i))
    selectedDataLines
  }

  def cut(xs: Array[String], n: Int): Array[Array[String]] = {
    val (quot, rem) = (xs.size / n, xs.size % n)
    val (smaller, bigger) = xs.splitAt(xs.size - rem * (quot + 1))
    (smaller.grouped(quot) ++ bigger.grouped(quot + 1)).toArray
  }

  def parse = DigitSet(parseDataLines(parseDataIndex))

  def parseAsSubsets(subsetSize: Int): Queue[DigitSet] = {
    val dataLines = parseDataLines(parseDataIndex)
    val subsets = cut(dataLines, subsetSize)
    val digitSets = subsets.map(dl => DigitSet(dl))
    Queue(digitSets: _*)
  }

}

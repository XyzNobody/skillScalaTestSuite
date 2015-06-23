package common.randomHelpers

import scala.util.Random

abstract class Distribution(protected val random: Random) {
  def next: Int
}

class UniformDistribution(_random: Random, val lowerBound: Int, val upperBound: Int)
  extends Distribution(_random) {
  def next = random.nextInt(upperBound - lowerBound) + lowerBound
}

class LogarithmicDistribution(_random: Random, lowerBound: Int, upperBound: Int)
  extends Distribution(_random) {
  private val logLower = Math.log(lowerBound)
  private val logUpper = Math.log(upperBound)
  def next = Math.exp(random.nextDouble() * (logUpper - logLower) + logLower).toInt
}

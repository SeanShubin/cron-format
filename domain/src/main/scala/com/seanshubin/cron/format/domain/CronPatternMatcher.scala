package com.seanshubin.cron.format.domain

import com.seanshubin.cron.format.domain.CronMatchResult._

object CronPatternMatcher {
  def cronElement(cronElement: String, timeUnit: CronTimeUnitEnum): CronMatchResult = {
    val SingleNumber = """(\d+)""".r
    val Range = """(\d+)-(\d+)""".r
    val Increment = """(\d+)/(\d+)""".r
    val SeveralNumbers = """(\d+(?:,\d+)*)""".r

    val result = try {
      cronElement match {
        case "*" => CronAll
        case SingleNumber(number) => CronNumber(timeUnit.stringToIntInRange(number))
        case Range(beginRange, endRange) => CronRange(timeUnit.stringToIntInRange(beginRange), timeUnit.stringToIntInRange(endRange))
        case Increment(start, step) => CronIncrement(timeUnit.stringToIntInRange(start), step.toInt)
        case SeveralNumbers(numbers) => {
          val numbersAsSeq = SingleNumber.findAllIn(numbers).toSeq.map(timeUnit.stringToIntInRange)
          CronMultiple(numbersAsSeq: _*)
        }
        case _ => CronNoMatch("'%s' did not match a valid pattern. valid patterns are a number (12), range (10-20), increment (1/5), multiple (1,2,3), or all (*)".format(cronElement))
      }
    } catch {
      case ex: Exception => CronNoMatch(ex.getMessage)
    }
    result
  }

  def verboseCronElement(element: String): (CronTimeUnitEnum, CronMatchResult) = {
    val ExtractSingleNumber = """(\d+)""".r
    val SingleNumber = """(\w+) is (\d+)""".r
    val Range = """(\w+) is between (\d+) and (\d+)""".r
    val IncrementA = """every (\d+) (\w+) starting at (\d+)""".r
    val IncrementB = """every (\w+) starting at (\d+)""".r
    val IncrementC = """every (\d+) (\w+)""".r
    val IncrementD = """every (\w+)""".r
    val SeveralNumbers = """(\w+) is (\d+(?: \d+)*)""".r

    val result: (CronTimeUnitEnum, CronMatchResult) = element match {
      case SingleNumber(timeUnitName, numberAsString) => {
        val timeUnit = CronTimeUnitEnum.fromName(timeUnitName)
        val number = timeUnit.stringToIntInRange(numberAsString)
        (timeUnit, CronNumber(number))
      }
      case Range(timeUnitName, beginRangeAsString, endRangeAsString) => {
        val timeUnit = CronTimeUnitEnum.fromName(timeUnitName)
        val beginRange = timeUnit.stringToIntInRange(beginRangeAsString)
        val endRange = timeUnit.stringToIntInRange(endRangeAsString)
        (timeUnit, CronRange(beginRange, endRange))
      }
      case IncrementA(stepAsString, timeUnitName, startAsString) => {
        val timeUnit = CronTimeUnitEnum.fromName(timeUnitName)
        val start = timeUnit.stringToIntInRange(startAsString)
        val step = stepAsString.toInt
        (timeUnit, CronIncrement(start, step))
      }
      case IncrementB(timeUnitName, startAsString) => {
        val timeUnit = CronTimeUnitEnum.fromName(timeUnitName)
        val start = timeUnit.stringToIntInRange(startAsString)
        val step = 1
        (timeUnit, CronIncrement(start, step))
      }
      case IncrementC(stepAsString, timeUnitName) => {
        val timeUnit = CronTimeUnitEnum.fromName(timeUnitName)
        val start = timeUnit.minimumAllowed
        val step = stepAsString.toInt
        (timeUnit, CronIncrement(start, step))
      }
      case IncrementD(timeUnitName) => {
        val timeUnit = CronTimeUnitEnum.fromName(timeUnitName)
        (timeUnit, CronAll)
      }
      case SeveralNumbers(timeUnitName, numbersAsString) => {
        val timeUnit = CronTimeUnitEnum.fromName(timeUnitName)
        val numbersAsSeq = ExtractSingleNumber.findAllIn(numbersAsString).toSeq.map(timeUnit.stringToIntInRange)
        (timeUnit, CronMultiple(numbersAsSeq: _*))
      }
      case _ => throw new RuntimeException("'%s' did not match a valid pattern.".format(element))
    }
    result
  }

}

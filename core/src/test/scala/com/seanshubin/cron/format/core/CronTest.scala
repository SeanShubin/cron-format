package com.seanshubin.cron.format.core

import org.scalatest.FunSuite

class CronTest extends FunSuite {
  test("cron element second") {
    val testData = Seq(
      ("*", "every second"),
      ("5", "second is 5"),
      ("1/4", "every 4 seconds starting at 1"),
      ("5-10", "second is between 5 and 10"),
      ("11,15,27", "second is 11 15 27"))
    def secondsTest(testDatum: (String, String)) = {
      val (cronElement, expected) = testDatum
      val Some(actual) = CronTimeUnitEnum.Second.format(cronElement)
      assert(actual === expected)
    }
    testData.foreach(secondsTest)
  }

  test("cron element minute") {
    val testData = Seq(
      ("*", "every minute"),
      ("0/1", "every minute"),
      ("5/1", "every minute starting at 5"),
      ("0/2", "every 2 minutes"),
      ("5/2", "every 2 minutes starting at 5"),
      ("5", "minute is 5"),
      ("1/4", "every 4 minutes starting at 1"),
      ("5-10", "minute is between 5 and 10"),
      ("11,15,27", "minute is 11 15 27"))
    def minutesTest(testDatum: (String, String)) {
      val (cronElement, expected) = testDatum
      val Some(actual) = CronTimeUnitEnum.Minute.format(cronElement)
      assert(actual === expected)
    }
    testData.foreach(minutesTest)
  }

  test("increment step is not validated as the unit") {
    val cron = "0 0 0 * * ? 2000/4"
    val expected = "every 4 years starting at 2000"
    val actual = CronFormat.cronToVerbose(cron)
    assert(actual === expected)
  }

  test("invald cron expressions") {
    val testData = Seq(
      ("", "cron expression must not be empty"),
      ("1", "cron expression '1' is invalid, it must have 6 or 7 elements, but has 1"),
      ("1 2 3 4 5", "cron expression '1 2 3 4 5' is invalid, it must have 6 or 7 elements, but has 5"),
      ("1 2 3 4 5 6 7 8", "cron expression '1 2 3 4 5 6 7 8' is invalid, it must have 6 or 7 elements, but has 8")
    )
    def invalidExpressionTest(testDatum: (String, String)) {
      val (cronExpression, expectedMessage) = testDatum
      try {
        CronFormat.cronToVerbose(cronExpression)
        fail("Should have thrown exception with message: " + expectedMessage)
      } catch {
        case ex: Exception => assert(expectedMessage === ex.getMessage)
      }
    }
    testData.foreach(invalidExpressionTest)
  }

  test("pattern matcher") {
    val testData = Seq(
      ("45", CronNumber(45)),
      ("12-34", CronRange(12, 34)),
      ("15/5", CronIncrement(15, 5)),
      ("1,2,3", CronMultiple(1, 2, 3)),
      ("*", CronAll),
      ("123", CronNoMatch("123 is greater than the maximum allowed value of 59")),
      ("123-456", CronNoMatch("123 is greater than the maximum allowed value of 59")),
      ("123/456", CronNoMatch("123 is greater than the maximum allowed value of 59")),
      ("123,456,789", CronNoMatch("123 is greater than the maximum allowed value of 59")),
      ("wtf", CronNoMatch("'wtf' did not match a valid pattern. valid patterns are a number (12), range (10-20), increment (1/5), multiple (1,2,3), or all (*)"))
    )
    def patternMatcherTest(testDatum: (String, CronMatchResult)) {
      val (target, expected) = testDatum
      val actual = CronPatternMatcher.cronElement(target, CronTimeUnitEnum.Second)
      assert(actual === expected)
    }
    testData.foreach(patternMatcherTest)
  }

  test("end to end happy path") {
    val testData = Seq(
      ("every day at midnight", "0 0 0 * * ? *"),
      ("second is 1", "1 0 0 * * ? *"),
      ("second is 2", "2 0 0 * * ? *"),
      ("second is between 2 and 4", "2-4 0 0 * * ? *"),
      ("every 5 seconds starting at 1", "1/5 0 0 * * ? *"),
      ("every second starting at 5", "5/1 0 0 * * ? *"),
      ("second is 1 2 3", "1,2,3 0 0 * * ? *"),
      ("every second", "* 0 0 * * ? *"),
      ("minute is between 3 and 5", "0 3-5 0 * * ? *"),
      ("every hour starting at 3", "0 0 3/1 * * ? *"),
      ("every 3 hours starting at 1", "0 0 1/3 * * ? *"),
      ("every 3 hours", "0 0 0/3 * * ? *"),
      ("day is 1", "0 0 0 1 * ? *"),
      ("month is 2", "0 0 0 * 2 ? *"),
      ("year is 2000", "0 0 0 * * ? 2000"),
      ("second is 12, hour is 23, month is 11, year is 1978", "12 0 23 * 11 ? 1978"),
      ("minute is 23, day is 15", "0 23 0 15 * ? *"),
      ("second is 45, minute is 23, hour is 8, day is 15, month is 5, year is 2001", "45 23 8 15 5 ? 2001"),
      ("second is between 30 and 45, every 3 minutes starting at 2, every hour, day is 2 3 7, month is 5, year is between 2001 and 2005", "30-45 2/3 * 2,3,7 5 ? 2001-2005"),
      ("every hour", "0 0 * * * ? *")
    )
    def endToEndTest(testDatum: (String, String)) {
      val (verbose, cron) = testDatum
      val convertedCron = CronFormat.verboseToCron(verbose)
      val convertedVerbose = CronFormat.cronToVerbose(cron)
      assert(convertedVerbose === verbose, "converting cron: " + cron)
      assert(convertedCron === cron, "converting verbose: " + verbose)
    }
    testData.foreach(endToEndTest)
  }

  test("if fail to parse verbose, check if it is a cron and give appropriate message") {
    val cron = "30-45 2/3 * 2,3,7 5 ? 2001-2005"
    val verbose = "second is between 30 and 45, every 3 minutes starting at 2, every hour, day is 2 3 7, month is 5, year is between 2001 and 2005"
    val error = intercept[RuntimeException] {
      CronFormat.verboseToCron(cron)
    }
    val expectedMessage = "'%s' appears to be a cron expression, try '%s' instead".format(cron, verbose)
    val actualMessage = error.getMessage
    assert(actualMessage === expectedMessage)
  }
}

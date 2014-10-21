package com.seanshubin.cron.format.core

import org.scalatest.FunSuite

class VerboseCronTest  extends FunSuite {
  test("second") {
    def valid(input:String, expected:String) = {
      VerboseCron.second.parse(input) match {
        case Left(message) =>
          fail(s"expected valid value '$expected', but got error '$message'")
        case Right(actual) =>
          assert(actual === expected)
      }
    }
    def invalid(input:String, expected:String) = {
      VerboseCron.second.parse(input) match {
        case Left(message) =>
          assert(message === expected)
        case Right(actual) =>
          fail(s"expected error message '$expected', but got valid value '$actual'")
      }
    }
    valid("0", "0")
    valid("59", "59")
    invalid("-1", "invalid value for second: '-1' is not between 0 and 59")
    invalid("60", "invalid value for second: '60' is not between 0 and 59")
    invalid("nan", "invalid value for second: 'nan' is not a whole number")
  }
  test("minute") {
    def valid(input:String, expected:String) = {
      VerboseCron.minute.parse(input) match {
        case Left(message) =>
          fail(s"expected valid value '$expected', but got error '$message'")
        case Right(actual) =>
          assert(actual === expected)
      }
    }
    def invalid(input:String, expected:String) = {
      VerboseCron.minute.parse(input) match {
        case Left(message) =>
          assert(message === expected)
        case Right(actual) =>
          fail(s"expected error message '$expected', but got valid value '$actual'")
      }
    }
    valid("0", "0")
    valid("59", "59")
    invalid("-1", "invalid value for minute: '-1' is not between 0 and 59")
    invalid("60", "invalid value for minute: '60' is not between 0 and 59")
    invalid("nan", "invalid value for minute: 'nan' is not a whole number")
  }
  test("hour") {
    def valid(input:String, expected:String) = {
      VerboseCron.hour.parse(input) match {
        case Left(message) =>
          fail(s"expected valid value '$expected', but got error '$message'")
        case Right(actual) =>
          assert(actual === expected)
      }
    }
    def invalid(input:String, expected:String) = {
      VerboseCron.hour.parse(input) match {
        case Left(message) =>
          assert(message === expected)
        case Right(actual) =>
          fail(s"expected error message '$expected', but got valid value '$actual'")
      }
    }
    valid("0", "0")
    valid("59", "59")
    invalid("-1", "invalid value for hour: '-1' is not between 0 and 59")
    invalid("60", "invalid value for hour: '60' is not between 0 and 59")
    invalid("nan", "invalid value for hour: 'nan' is not a whole number")
  }
  test("day of month") {
    def valid(input:String, expected:String) = {
      VerboseCron.dayOfMonth.parse(input) match {
        case Left(message) =>
          fail(s"expected valid value '$expected', but got error '$message'")
        case Right(actual) =>
          assert(actual === expected)
      }
    }
    def invalid(input:String, expected:String) = {
      VerboseCron.dayOfMonth.parse(input) match {
        case Left(message) =>
          assert(message === expected)
        case Right(actual) =>
          fail(s"expected error message '$expected', but got valid value '$actual'")
      }
    }
    valid("1", "1")
    valid("31", "31")
    invalid("0", "invalid value for day: '0' is not between 1 and 31")
    invalid("32", "invalid value for day: '32' is not between 1 and 31")
    invalid("nan", "invalid value for day: 'nan' is not a whole number")
  }
  test("month of year - numeric") {
    def valid(input:String, expected:String) = {
      VerboseCron.month.parse(input) match {
        case Left(message) =>
          fail(s"expected valid value '$expected', but got error '$message'")
        case Right(actual) =>
          assert(actual === expected)
      }
    }
    def invalid(input:String, expected:String) = {
      VerboseCron.month.parse(input) match {
        case Left(message) =>
          assert(message === expected)
        case Right(actual) =>
          fail(s"expected error message '$expected', but got valid value '$actual'")
      }
    }
    valid("1", "January")
    valid("12", "December")
    invalid("0", "invalid value for month: '0' is not a whole number between 1 and 12, and is not a 3-letter month abbreviation")
    invalid("13", "invalid value for month: '13' is not a whole number between 1 and 12, and is not a 3-letter month abbreviation")
    invalid("nan", "invalid value for month: 'nan' is not a whole number between 1 and 12, and is not a 3-letter month abbreviation")
  }
  test("month of year - abbreviation") {
    def valid(input:String, expected:String) = {
      VerboseCron.month.parse(input) match {
        case Left(message) =>
          fail(s"expected valid value '$expected', but got error '$message'")
        case Right(actual) =>
          assert(actual === expected)
      }
    }
    def invalid(input:String, expected:String) = {
      VerboseCron.month.parse(input) match {
        case Left(message) =>
          assert(message === expected)
        case Right(actual) =>
          fail(s"expected error message '$expected', but got valid value '$actual'")
      }
    }
    valid("jan", "January")
    valid("feb", "February")
    valid("mar", "March")
    valid("apr", "April")
    valid("may", "May")
    valid("jun", "June")
    valid("jul", "July")
    valid("aug", "August")
    valid("sep", "September")
    valid("oct", "October")
    valid("nov", "November")
    valid("dec", "December")
    valid("JAN", "January")
    valid("FEB", "February")
    valid("MAR", "March")
    valid("APR", "April")
    valid("MAY", "May")
    valid("JUN", "June")
    valid("JUL", "July")
    valid("AUG", "August")
    valid("SEP", "September")
    valid("OCT", "October")
    valid("NOV", "November")
    valid("DEC", "December")
  }
  test("day of week - numeric") {
    def valid(input:String, expected:String) = {
      VerboseCron.dayOfWeek.parse(input) match {
        case Left(message) =>
          fail(s"expected valid value '$expected', but got error '$message'")
        case Right(actual) =>
          assert(actual === expected)
      }
    }
    def invalid(input:String, expected:String) = {
      VerboseCron.dayOfWeek.parse(input) match {
        case Left(message) =>
          assert(message === expected)
        case Right(actual) =>
          fail(s"expected error message '$expected', but got valid value '$actual'")
      }
    }
    valid("1", "Sunday")
    valid("7", "Saturday")
    invalid("0", "invalid value for day of week: '0' is not a whole number between 1 and 7, and is not a 3-letter day of week abbreviation")
    invalid("8", "invalid value for day of week: '8' is not a whole number between 1 and 7, and is not a 3-letter day of week abbreviation")
    invalid("nan", "invalid value for day of week: 'nan' is not a whole number between 1 and 7, and is not a 3-letter day of week abbreviation")
  }
  test("day of week - abbreviation") {
    def valid(input:String, expected:String) = {
      VerboseCron.dayOfWeek.parse(input) match {
        case Left(message) =>
          fail(s"expected valid value '$expected', but got error '$message'")
        case Right(actual) =>
          assert(actual === expected)
      }
    }
    def invalid(input:String, expected:String) = {
      VerboseCron.dayOfWeek.parse(input) match {
        case Left(message) =>
          assert(message === expected)
        case Right(actual) =>
          fail(s"expected error message '$expected', but got valid value '$actual'")
      }
    }
    valid("sun", "Sunday")
    valid("mon", "Monday")
    valid("tue", "Tuesday")
    valid("wed", "Wednesday")
    valid("thu", "Thursday")
    valid("fri", "Friday")
    valid("sat", "Saturday")
    valid("SUN", "Sunday")
    valid("MON", "Monday")
    valid("TUE", "Tuesday")
    valid("WED", "Wednesday")
    valid("THU", "Thursday")
    valid("FRI", "Friday")
    valid("SAT", "Saturday")
  }
  test("year") {
    def valid(input:String, expected:String) = {
      VerboseCron.year.parse(input) match {
        case Left(message) =>
          fail(s"expected valid value '$expected', but got error '$message'")
        case Right(actual) =>
          assert(actual === expected)
      }
    }
    def invalid(input:String, expected:String) = {
      VerboseCron.year.parse(input) match {
        case Left(message) =>
          assert(message === expected)
        case Right(actual) =>
          fail(s"expected error message '$expected', but got valid value '$actual'")
      }
    }
    valid("1900", "1900")
    valid("3000", "3000")
    invalid("1899", "invalid value for year: '1899' is not between 1900 and 3000")
    invalid("3001", "invalid value for year: '3001' is not between 1900 and 3000")
    invalid("nan", "invalid value for year: 'nan' is not a whole number")
  }
  test("cron mini pattern") {
    val testData = Seq(
      ("*", "every foo"),
      ("5", "foo is 5"),
      ("1/4", "every 4 foos starting at 1"),
      ("5-10", "foo is between 5 and 10"),
      ("11,15,27", "foo is 11 15 27"))
    val fakeCronPartParser = new VerboseCron.CronPartParser {
      override def parse(text: String): Either[String, String] = {
        try {
          Right(s"$text")
        } catch {
          case ex:NumberFormatException => Left(s"unable to convert '$text' to a number")
        }
      }

      override def singular: String = "foo"

      override def plural: String = "foos"
    }
    def verify(testDatum: (String, String)) = {
      val (cronElement, expected) = testDatum
      val actual = fakeCronPartParser.parseRange(cronElement)
      assert(actual === Right(expected))
    }
    testData.foreach(verify)
  }
  test("describe cron") {
    val testData = Seq(
      ("1 2 3 4 5 6 1978", "second is 1, minute is 2, hour is 3, day is 4, month is May, week is Friday, year is 1978"),
      ("1 2 3 4 may fri 1978", "second is 1, minute is 2, hour is 3, day is 4, month is May, week is Friday, year is 1978"),
      ("0 0 0 * * ? *", "second is 0, minute is 0, hour is 0, every day, every month, any week, every year"),
      ("1 0 0 * * ? *", "second is 1, minute is 0, hour is 0, every day, every month, any week, every year"),
      ("2 0 0 * * ? *", "second is 2, minute is 0, hour is 0, every day, every month, any week, every year"),
      ("2-4 0 0 * * ? *", "second is between 2 and 4, minute is 0, hour is 0, every day, every month, any week, every year"),
      ("1/5 0 0 * * ? *", "every 5 seconds starting at 1, minute is 0, hour is 0, every day, every month, any week, every year"),
      ("5/1 0 0 * * ? *", "every second starting at 5, minute is 0, hour is 0, every day, every month, any week, every year"),
      ("1,2,3 0 0 * * ? *", "second is 1 2 3, minute is 0, hour is 0, every day, every month, any week, every year"),
      ("* 0 0 * * ? *", "every second, minute is 0, hour is 0, every day, every month, any week, every year"),
      ("0 3-5 0 * * ? *","second is 0, minute is between 3 and 5, hour is 0, every day, every month, any week, every year"),
      ("0 0 3/1 * * ? *", "second is 0, minute is 0, every hour starting at 3, every day, every month, any week, every year"),
      ("0 0 1/3 * * ? *", "second is 0, minute is 0, every 3 hours starting at 1, every day, every month, any week, every year"),
      ("0 0 0/3 * * ? *", "second is 0, minute is 0, every 3 hours, every day, every month, any week, every year"),
      ("0 0 0 1 * ? *", "second is 0, minute is 0, hour is 0, day is 1, every month, any week, every year"),
      ("0 0 0 * 2 ? *", "second is 0, minute is 0, hour is 0, every day, month is February, any week, every year"),
      ("0 0 0 * * ? 2000", "second is 0, minute is 0, hour is 0, every day, every month, any week, year is 2000"),
      ("12 0 23 * 11 ? 1978", "second is 12, minute is 0, hour is 23, every day, month is November, any week, year is 1978"),
      ("0 23 0 15 * ? *", "second is 0, minute is 23, hour is 0, day is 15, every month, any week, every year"),
      ("45 23 8 15 5 ? 2001", "second is 45, minute is 23, hour is 8, day is 15, month is May, any week, year is 2001"),
      ("30-45 2/3 * 2,3,7 5 ? 2001-2005", "second is between 30 and 45, every 3 minutes starting at 2, every hour, day is 2 3 7, month is May, any week, year is between 2001 and 2005"),
      ("0 0 * * * ? *", "second is 0, minute is 0, every hour, every day, every month, any week, every year")
    )
    def verify(testTuple:(String, String)): Unit = {
      val (cron, expected) = testTuple
      val actual = VerboseCron.cronToVerboseCron(cron)
      assert(actual === Right(expected))
    }
    testData.foreach(verify)
  }

}

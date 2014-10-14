package com.seanshubin.cron.format.core

import org.scalatest.FunSuite

class VerboseCronTest  extends FunSuite {
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
    valid("0", "minute is 0")
    valid("59", "minute is 59")
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
    valid("0", "hour is 0")
    valid("59", "hour is 59")
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
    valid("1", "day is 1")
    valid("31", "day is 31")
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
    valid("1", "month is January")
    valid("12", "month is December")
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
    valid("jan", "month is January")
    valid("feb", "month is February")
    valid("mar", "month is March")
    valid("apr", "month is April")
    valid("may", "month is May")
    valid("jun", "month is June")
    valid("jul", "month is July")
    valid("aug", "month is August")
    valid("sep", "month is September")
    valid("oct", "month is October")
    valid("nov", "month is November")
    valid("dec", "month is December")
    valid("JAN", "month is January")
    valid("FEB", "month is February")
    valid("MAR", "month is March")
    valid("APR", "month is April")
    valid("MAY", "month is May")
    valid("JUN", "month is June")
    valid("JUL", "month is July")
    valid("AUG", "month is August")
    valid("SEP", "month is September")
    valid("OCT", "month is October")
    valid("NOV", "month is November")
    valid("DEC", "month is December")
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
    valid("1", "day of week is Sunday")
    valid("7", "day of week is Saturday")
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
    valid("sun", "day of week is Sunday")
    valid("mon", "day of week is Monday")
    valid("tue", "day of week is Tuesday")
    valid("wed", "day of week is Wednesday")
    valid("thu", "day of week is Thursday")
    valid("fri", "day of week is Friday")
    valid("sat", "day of week is Saturday")
    valid("SUN", "day of week is Sunday")
    valid("MON", "day of week is Monday")
    valid("TUE", "day of week is Tuesday")
    valid("WED", "day of week is Wednesday")
    valid("THU", "day of week is Thursday")
    valid("FRI", "day of week is Friday")
    valid("SAT", "day of week is Saturday")
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
    valid("1900", "year is 1900")
    valid("3000", "year is 3000")
    invalid("1899", "invalid value for year: '1899' is not between 1900 and 3000")
    invalid("3001", "invalid value for year: '3001' is not between 1900 and 3000")
    invalid("nan", "invalid value for year: 'nan' is not a whole number")
  }
}

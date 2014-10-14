package com.seanshubin.cron.format.core

import scala.collection.mutable.ArrayBuffer

object VerboseCron {

  class Minute {
    def parse(text: String): Either[String, String] = {
      try {
        val value = text.toInt
        if (value < 0 || value > 59) Left(s"invalid value for minute: '$value' is not between 0 and 59")
        else Right(s"minute is $value")
      } catch {
        case ex: NumberFormatException =>
          Left(s"invalid value for minute: '$text' is not a whole number")
      }
    }
  }

  class Hour {
    def parse(text: String): Either[String, String] = {
      try {
        val value = text.toInt
        if (value < 0 || value > 59) Left(s"invalid value for hour: '$value' is not between 0 and 59")
        else Right(s"hour is $value")
      } catch {
        case ex: NumberFormatException =>
          Left(s"invalid value for hour: '$text' is not a whole number")
      }
    }
  }

  class DayOfMonth {
    def parse(text: String): Either[String, String] = {
      try {
        val value = text.toInt
        if (value < 1 || value > 31) Left(s"invalid value for day: '$value' is not between 1 and 31")
        else Right(s"day is $value")
      } catch {
        case ex: NumberFormatException =>
          Left(s"invalid value for day: '$text' is not a whole number")
      }
    }
  }

  class Month {
    def parse(text: String): Either[String, String] = {
      val maybeMonth = MonthEnum.fromString(text)
      maybeMonth match {
        case Some(monthEnum) =>
          Right(s"month is ${monthEnum.name}")
        case None =>
          Left(s"invalid value for month: '$text' is not a whole number between 1 and 12, and is not a 3-letter month abbreviation")
      }
    }
  }

  class DayOfWeek {
    def parse(text: String): Either[String, String] = {
      val maybeDayOfWeek = DayOfWeekEnum.fromString(text)
      maybeDayOfWeek match {
        case Some(dayOfWeekEnum) =>
          Right(s"day of week is ${dayOfWeekEnum.name}")
        case None =>
          Left(s"invalid value for day of week: '$text' is not a whole number between 1 and 7, and is not a 3-letter day of week abbreviation")
      }
    }
  }

  class Year {
    def parse(text: String): Either[String, String] = {
      try {
        val value = text.toInt
        if (value < 1900 || value > 3000) Left(s"invalid value for year: '$value' is not between 1900 and 3000")
        else Right(s"year is $value")
      } catch {
        case ex: NumberFormatException =>
          Left(s"invalid value for year: '$text' is not a whole number")
      }
    }
  }

  sealed abstract case class MonthEnum(ordinal: Int, name: String, shortName: String) {
    MonthEnum.valuesBuffer += this

    def matchesText(text: String): Boolean = {
      val indeedMatchesText =
        text.equalsIgnoreCase(shortName) ||
          text.equals(ordinal.toString)
      indeedMatchesText
    }
  }

  object MonthEnum {
    private val valuesBuffer = new ArrayBuffer[MonthEnum]
    lazy val values = valuesBuffer.toSeq
    val January = new MonthEnum(1, "January", "jan") {}
    val February = new MonthEnum(2, "February", "feb") {}
    val March = new MonthEnum(3, "March", "mar") {}
    val April = new MonthEnum(4, "April", "apr") {}
    val May = new MonthEnum(5, "May", "may") {}
    val June = new MonthEnum(6, "June", "jun") {}
    val July = new MonthEnum(7, "July", "jul") {}
    val August = new MonthEnum(8, "August", "aug") {}
    val September = new MonthEnum(9, "September", "sep") {}
    val October = new MonthEnum(10, "October", "oct") {}
    val November = new MonthEnum(11, "November", "nov") {}
    val December = new MonthEnum(12, "December", "dec") {}

    def fromString(text: String): Option[MonthEnum] = {
      values.find(x => x.matchesText(text))
    }
  }

  sealed abstract case class DayOfWeekEnum(ordinal: Int, name: String, shortName: String) {
    DayOfWeekEnum.valuesBuffer += this

    def matchesText(text: String): Boolean = {
      val indeedMatchesText =
        text.equalsIgnoreCase(shortName) ||
          text.equals(ordinal.toString)
      indeedMatchesText
    }
  }

  object DayOfWeekEnum {
    private val valuesBuffer = new ArrayBuffer[DayOfWeekEnum]
    lazy val values = valuesBuffer.toSeq
    val Sunday = new DayOfWeekEnum(1, "Sunday", "sun") {}
    val Monday = new DayOfWeekEnum(2, "Monday", "mon") {}
    val Tuesday = new DayOfWeekEnum(3, "Tuesday", "tue") {}
    val Wednesday = new DayOfWeekEnum(4, "Wednesday", "wed") {}
    val Thursday = new DayOfWeekEnum(5, "Thursday", "thu") {}
    val Friday = new DayOfWeekEnum(6, "Friday", "fri") {}
    val Saturday = new DayOfWeekEnum(7, "Saturday", "sat") {}

    def fromString(text: String): Option[DayOfWeekEnum] = {
      values.find(x => x.matchesText(text))
    }
  }

  val minute: Minute = new Minute
  val hour: Hour = new Hour
  val dayOfMonth: DayOfMonth = new DayOfMonth
  val month: Month = new Month
  val dayOfWeek: DayOfWeek = new DayOfWeek
  val year: Year = new Year
}

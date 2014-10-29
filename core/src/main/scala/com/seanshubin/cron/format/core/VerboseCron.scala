package com.seanshubin.cron.format.core

import scala.collection.mutable.ArrayBuffer

object VerboseCron {
  def cronToVerboseCron(cron: String): Either[String, String] = {
    if (cron.trim.isEmpty) {
      Left("cron expression must not be empty")
    } else {
      val cronParts = cron.split("\\s+", -1).toSeq
      if (cronParts.size == 6 || cronParts.size == 7) {
        val partsAndParsers = cronParts zip cronPartParsers
        val builder = partsAndParsers.foldLeft(ParsedCronBuilder())(parseCronRange)
        builder.build()
      } else {
        Left(s"cron expression '$cron' is invalid, it must have 6 or 7 elements, but has ${cronParts.size}")
      }
    }
  }

  case class ParsedCronBuilder(parsedCronParts: List[String] = Nil, errors: List[String] = Nil) {
    def withPartAndParser(partAndParser: (String, CronPartParser)): ParsedCronBuilder = {
      val (part, parser) = partAndParser
      parser.parseRange(part) match {
        case Left(error) => copy(errors = error +: errors)
        case Right(parsed) => copy(parsedCronParts = parsed +: parsedCronParts)
      }
    }

    def build(): Either[String, String] = {
      if (errors.isEmpty) {
        Right(parsedCronParts.reverse.mkString(", "))
      } else {
        Left(errors.reverse.mkString(", "))
      }
    }
  }

  def parseCronRange(builder: ParsedCronBuilder, partAndParser: (String, CronPartParser)): ParsedCronBuilder = {
    builder.withPartAndParser(partAndParser)
  }

  trait CronRange {
    def matches(text: String): Boolean

    def parse(text: String, partParser: CronPartParser): Either[String, String]
  }

  object Every extends CronRange {
    override def matches(text: String): Boolean = text == "*"

    override def parse(text: String, partParser: CronPartParser): Either[String, String] = {
      if (text == "*") Right(s"every ${partParser.singular}")
      else Left(s"Expected '*', got '$text'")
    }
  }

  object Between extends CronRange {
    override def matches(text: String): Boolean = text.contains("-")

    override def parse(text: String, partParser: CronPartParser): Either[String, String] = {
      val parts = text.split("-", -1).toSeq
      collapseEithers(parts.map(partParser.parse)) match {
        case Right(values) =>
          if (values.size == 2) Right(s"${partParser.singular} is between ${values(1)} and ${values(0)}")
          else Left(s"Only 2 values allowed in a range, got ${values.size}")
        case Left(errors) =>
          Left(errors.mkString(", "))
      }
    }
  }

  def collapseEithers(eithers: Seq[Either[String, String]]): Either[Seq[String], Seq[String]] = {
    val initialValue: Either[Seq[String], Seq[String]] = Right(Seq())
    eithers.foldLeft(initialValue)(collapseEither)
  }

  def collapseEither(soFar: Either[Seq[String], Seq[String]], current: Either[String, String]): Either[Seq[String], Seq[String]] = {
    soFar match {
      case Left(errors) =>
        current match {
          case Left(error) => Left(error +: errors)
          case Right(value) => soFar
        }
      case Right(values) =>
        current match {
          case Left(error) => Left(Seq(error))
          case Right(value) => Right(value +: values)
        }
    }
  }

  object Step extends CronRange {
    override def matches(text: String): Boolean = text.contains("/")

    override def parse(text: String, partParser: CronPartParser): Either[String, String] = {
      val parts = text.split("/", -1).toSeq
      if (parts.size == 2) {
        val step = validateInt(parts(1))
        val start = partParser.parse(parts(0))
        (step, start) match {
          case (Right("1"), Right("0")) =>
            Right(s"every ${partParser.singular}")
          case (Right(stepValue), Right("0")) =>
            Right(s"every $stepValue ${partParser.plural}")
          case (Right("1"), Right(startValue)) =>
            Right(s"every ${partParser.singular} starting at $startValue")
          case (Right(stepValue), Right(startValue)) =>
            Right(s"every $stepValue ${partParser.plural} starting at $startValue")
          case (Left(x), Left(y)) => Left(s"$x, $y")
          case (Left(x), _) => Left(x)
          case (_, Left(x)) => Left(x)
        }
      } else {
        Left(s"When using /, must have exactly 2 values, got ${parts.size}")
      }
    }
  }

  object Values extends CronRange {
    override def matches(text: String): Boolean = text.contains(",")

    override def parse(text: String, partParser: CronPartParser): Either[String, String] = {
      val parts = text.split(",", -1).toSeq
      collapseEithers(parts.map(partParser.parse)) match {
        case Right(values) =>
          val valuesString = values.reverse.mkString(" ")
          Right(s"${partParser.singular} is $valuesString")
        case Left(errors) =>
          Left(errors.mkString(", "))
      }
    }
  }

  object Question extends CronRange {
    override def matches(text: String): Boolean = text.contains("?")

    override def parse(text: String, partParser: CronPartParser): Either[String, String] = {
      if (text == "?") Right(s"any ${partParser.singular}")
      else Left(s"Expected '?', got '$text'")

    }
  }

  object Single extends CronRange {
    override def matches(text: String): Boolean =
      !text.contains("?") &&
        !text.contains("*") &&
        !text.contains("/") &&
        !text.contains("-") &&
        !text.contains(",")

    override def parse(text: String, partParser: CronPartParser): Either[String, String] = {
      partParser.parse(text) match {
        case Left(error) => Left(error)
        case Right(value) => Right(s"${partParser.singular} is $value")
      }
    }
  }

  val cronRanges: Seq[CronRange] = Seq(Every, Between, Step, Values, Question, Single)

  trait CronPartParser {
    def parse(text: String): Either[String, String]

    def parseRange(text: String): Either[String, String] = {
      val howManyMatching = cronRanges.count(_.matches(text))
      val result = if (howManyMatching == 0) {
        Left(s"'$text' does not match a cron range type")
      } else if (howManyMatching == 1) {
        val theRange = cronRanges.filter(_.matches(text)).head
        theRange.parse(text, this)
      } else {
        Left(s"'$text' matches multiple cron range types, which is not currently supported")
      }
      result
    }

    def singular: String

    def plural: String
  }

  def validateInt(text: String): Either[String, String] = {
    try {
      Right(text.toInt.toString)
    } catch {
      case _: NumberFormatException => Left(s"Unable to convert '$text' to a number")
    }
  }

  class Second extends CronPartParser {
    def parse(text: String): Either[String, String] = {
      try {
        val value = text.toInt
        if (value < 0 || value > 59) Left(s"invalid value for $singular: '$value' is not between 0 and 59")
        else Right(s"$value")
      } catch {
        case ex: NumberFormatException =>
          Left(s"invalid value for $singular: '$text' is not a whole number")
      }
    }

    override def singular: String = "second"

    override def plural: String = "seconds"
  }

  class Minute extends CronPartParser {
    def parse(text: String): Either[String, String] = {
      try {
        val value = text.toInt
        if (value < 0 || value > 59) Left(s"invalid value for $singular: '$value' is not between 0 and 59")
        else Right(s"$value")
      } catch {
        case ex: NumberFormatException =>
          Left(s"invalid value for $singular: '$text' is not a whole number")
      }
    }

    override def singular: String = "minute"

    override def plural: String = "minutes"
  }

  class Hour extends CronPartParser {
    def parse(text: String): Either[String, String] = {
      try {
        val value = text.toInt
        if (value < 0 || value > 59) Left(s"invalid value for $singular: '$value' is not between 0 and 59")
        else Right(s"$value")
      } catch {
        case ex: NumberFormatException =>
          Left(s"invalid value for $singular: '$text' is not a whole number")
      }
    }

    override def singular: String = "hour"

    override def plural: String = "hours"
  }

  class DayOfMonth extends CronPartParser {
    def parse(text: String): Either[String, String] = {
      try {
        val value = text.toInt
        if (value < 1 || value > 31) Left(s"invalid value for $singular: '$value' is not between 1 and 31")
        else Right(s"$value")
      } catch {
        case ex: NumberFormatException =>
          Left(s"invalid value for $singular: '$text' is not a whole number")
      }
    }

    override def singular: String = "day"

    override def plural: String = "days"
  }

  class Month extends CronPartParser {
    def parse(text: String): Either[String, String] = {
      val maybeMonth = MonthEnum.fromString(text)
      maybeMonth match {
        case Some(monthEnum) =>
          Right(s"${monthEnum.name}")
        case None =>
          Left(s"invalid value for $singular: '$text' is not a whole number between 1 and 12, and is not a 3-letter $singular abbreviation")
      }
    }

    override def singular: String = "month"

    override def plural: String = "months"
  }

  class DayOfWeek extends CronPartParser {
    def parse(text: String): Either[String, String] = {
      val maybeDayOfWeek = DayOfWeekEnum.fromString(text)
      maybeDayOfWeek match {
        case Some(dayOfWeekEnum) =>
          Right(s"${dayOfWeekEnum.name}")
        case None =>
          Left(s"invalid value for $singular: '$text' is not a whole number between 1 and 7, and is not a 3-letter $singular abbreviation")
      }
    }

    override def singular: String = "day of week"

    override def plural: String = "days of week"
  }

  class Year extends CronPartParser {
    def parse(text: String): Either[String, String] = {
      try {
        val value = text.toInt
        if (value < 1900 || value > 3000) Left(s"invalid value for $singular: '$value' is not between 1900 and 3000")
        else Right(s"$value")
      } catch {
        case ex: NumberFormatException =>
          Left(s"invalid value for $singular: '$text' is not a whole number")
      }
    }

    override def singular: String = "year"

    override def plural: String = "years"
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

  val second: Second = new Second
  val minute: Minute = new Minute
  val hour: Hour = new Hour
  val dayOfMonth: DayOfMonth = new DayOfMonth
  val month: Month = new Month
  val dayOfWeek: DayOfWeek = new DayOfWeek
  val year: Year = new Year

  val cronPartParsers: Seq[CronPartParser] = Seq(second, minute, hour, dayOfMonth, month, dayOfWeek, year)
}

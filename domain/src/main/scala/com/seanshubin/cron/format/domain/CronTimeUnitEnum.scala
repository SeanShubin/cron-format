package com.seanshubin.cron.format.domain

import com.seanshubin.cron.format.domain.CronMatchResult.CronNoMatch

import scala.collection.mutable.ArrayBuffer

sealed abstract case class CronTimeUnitEnum(defaultValue: String, singularName: String, pluralName: String, minimumAllowed: Int, maximumAllowed: Int, verboseOrdinalPosition: String) {
  CronTimeUnitEnum.valuesBuffer += this

  def format(cronElement: String): Option[String]

  def name(quantity: Int): String = if (quantity == 1) singularName else pluralName

  def stringToIntInRange(intAsString: String): Int = {
    val value = intAsString.toInt
    if (value < minimumAllowed) throw new RuntimeException("%d is less than the minimum allowed value of %d".format(value, minimumAllowed))
    if (value > maximumAllowed) throw new RuntimeException("%d is greater than the maximum allowed value of %d".format(value, maximumAllowed))
    value
  }
}

object CronTimeUnitEnum {
  private val valuesBuffer = new ArrayBuffer[CronTimeUnitEnum]
  lazy val values: Seq[CronTimeUnitEnum] = valuesBuffer
  val Second: CronTimeUnitEnum = new CronTimeUnitEnum("0", "second", "seconds", 0, 59, "first") {
    def format(cronElement: String): Option[String] = {
      if (cronElement == defaultValue) None
      else {
        CronPatternMatcher.cronElement(cronElement, this) match {
          case noMatch: CronNoMatch => throw new RuntimeException(noMatch.formatVerbose(this))
          case foundMatch => Some(foundMatch.formatVerbose(this))
        }
      }
    }
  }
  val Minute: CronTimeUnitEnum = new CronTimeUnitEnum("0", "minute", "minutes", 0, 59, "second") {
    def format(cronElement: String): Option[String] = {
      if (cronElement == defaultValue) None
      else {
        CronPatternMatcher.cronElement(cronElement, this) match {
          case noMatch: CronNoMatch => throw new RuntimeException(noMatch.formatVerbose(this))
          case foundMatch => Some(foundMatch.formatVerbose(this))
        }
      }
    }
  }
  val Hour: CronTimeUnitEnum = new CronTimeUnitEnum("0", "hour", "hours", 0, 23, "third") {
    def format(cronElement: String): Option[String] = {
      if (cronElement == defaultValue) None
      else {
        CronPatternMatcher.cronElement(cronElement, this) match {
          case noMatch: CronNoMatch => throw new RuntimeException(noMatch.formatVerbose(this))
          case foundMatch => Some(foundMatch.formatVerbose(this))
        }
      }
    }
  }
  val Day: CronTimeUnitEnum = new CronTimeUnitEnum("*", "day", "days", 1, 31, "fourth") {
    def format(cronElement: String): Option[String] = {
      if (cronElement == defaultValue) None
      else {
        CronPatternMatcher.cronElement(cronElement, this) match {
          case noMatch: CronNoMatch => throw new RuntimeException(noMatch.formatVerbose(this))
          case foundMatch => Some(foundMatch.formatVerbose(this))
        }
      }
    }
  }
  val Month: CronTimeUnitEnum = new CronTimeUnitEnum("*", "month", "months", 1, 12, "fifth") {
    def format(cronElement: String): Option[String] = {
      if (cronElement == defaultValue) None
      else {
        CronPatternMatcher.cronElement(cronElement, this) match {
          case noMatch: CronNoMatch => throw new RuntimeException(noMatch.formatVerbose(this))
          case foundMatch => Some(foundMatch.formatVerbose(this))
        }
      }
    }
  }
  val DayOfWeek: CronTimeUnitEnum = new CronTimeUnitEnum("?", "day-of-week", "day-of-week", 1, 7, "sixth") {
    def format(cronElement: String): Option[String] = {
      if (cronElement == "?") None
      else throw new RuntimeException("Day of week field (the sixth field) not supported yet, set it to '?', you have it set to '%s'".format(cronElement))
    }
  }
  val Year: CronTimeUnitEnum = new CronTimeUnitEnum("*", "year", "years", 1970, 2099, "seventh") {
    def format(cronElement: String): Option[String] = {
      if (cronElement == defaultValue) None
      else {
        CronPatternMatcher.cronElement(cronElement, this) match {
          case noMatch: CronNoMatch => throw new RuntimeException(noMatch.formatVerbose(this))
          case foundMatch => Some(foundMatch.formatVerbose(this))
        }
      }
    }
  }

  def fromName(name: String): CronTimeUnitEnum = {
    val maybeTimeUnit = values.find(timeUnit => timeUnit.singularName == name || timeUnit.pluralName == name)
    maybeTimeUnit match {
      case Some(timeUnit) => timeUnit
      case None =>
        val timeUnitNames = values.flatMap(x => Seq(x.singularName, x.pluralName))
        throw new RuntimeException("'%s' does not match a valid time unit, valid values are %s".format(name, timeUnitNames.mkString(", ")))
    }
  }
}

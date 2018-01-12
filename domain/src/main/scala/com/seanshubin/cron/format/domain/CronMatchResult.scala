package com.seanshubin.cron.format.domain

sealed trait CronMatchResult {
  def formatVerbose(timeUnit: CronTimeUnitEnum): String

  def formatCron(timeUnit: CronTimeUnitEnum): String
}

object CronMatchResult {
  case class CronNumber(value: Int) extends CronMatchResult {
    def formatVerbose(timeUnit: CronTimeUnitEnum) = "%s is %d".format(timeUnit.singularName, value)

    def formatCron(timeUnit: CronTimeUnitEnum) = value.toString
  }

  case class CronRange(beginRange: Int, endRange: Int) extends CronMatchResult {
    def formatVerbose(timeUnit: CronTimeUnitEnum) = "%s is between %d and %d".format(timeUnit.singularName, beginRange, endRange)

    def formatCron(timeUnit: CronTimeUnitEnum) = "%d-%d".format(beginRange, endRange)
  }

  case class CronIncrement(start: Int, step: Int) extends CronMatchResult {
    def formatVerbose(timeUnit: CronTimeUnitEnum) = {
      if (start == 0) {
        if (step == 1) "every %s".format(timeUnit.singularName)
        else "every %d %s".format(step, timeUnit.pluralName)
      }
      else {
        if (step == 1) "every %s starting at %d".format(timeUnit.singularName, start)
        else "every %d %s starting at %d".format(step, timeUnit.pluralName, start)
      }
    }

    def formatCron(timeUnit: CronTimeUnitEnum) = "%d/%d".format(start, step)
  }

  case class CronMultiple(values: Int*) extends CronMatchResult {
    def formatVerbose(timeUnit: CronTimeUnitEnum) = "%s is %s".format(timeUnit.singularName, values.mkString(" "))

    def formatCron(timeUnit: CronTimeUnitEnum) = values.mkString(",")

    override def toString = s"CronMultiple(${values.mkString(",")})"
  }

  case object CronAll extends CronMatchResult {
    def formatVerbose(timeUnit: CronTimeUnitEnum) = "every %s".format(timeUnit.singularName)

    def formatCron(timeUnit: CronTimeUnitEnum) = "*"
  }

  case class CronNoMatch(message: String) extends CronMatchResult {
    def formatVerbose(timeUnit: CronTimeUnitEnum) = "for field %s (the %s field), %s".format(timeUnit.pluralName, timeUnit.verboseOrdinalPosition, message)

    def formatCron(timeUnit: CronTimeUnitEnum) = throw new RuntimeException("error in %s field: %s".format(timeUnit.pluralName, message))
  }
}

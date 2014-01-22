package com.seanshubin.cron.format.core

object CronFormat {
  val cronFieldCharacterClass = """[\w?*/#,]+"""
  val cronDefault = "0 0 0 * * ? *"
  val verboseDefault = "every day at midnight"

  def cronToVerbose(cron: String): String = {
    val trimmedCron = cron.trim
    if (trimmedCron == "") {
      throw new RuntimeException("cron expression must not be empty")
    } else {
      val rawCronParts = trimmedCron.split( """\s+""").toSeq
      if (rawCronParts.size == 6 || rawCronParts.size == 7) {
        val cronParts = rawCronParts.padTo(7, "*")
        val secondPart = cronParts(0)
        val minutePart = cronParts(1)
        val hourPart = cronParts(2)
        val dayOfMonthPart = cronParts(3)
        val monthPart = cronParts(4)
        val dayOfWeekPart = cronParts(5)
        val yearPart = cronParts(6)
        val formatted =
          CronTimeUnitEnum.Second.format(secondPart) ::
            CronTimeUnitEnum.Minute.format(minutePart) ::
            CronTimeUnitEnum.Hour.format(hourPart) ::
            CronTimeUnitEnum.Day.format(dayOfMonthPart) ::
            CronTimeUnitEnum.Month.format(monthPart) ::
            CronTimeUnitEnum.DayOfWeek.format(dayOfWeekPart) ::
            CronTimeUnitEnum.Year.format(yearPart) ::
            Nil
        val flattened = formatted.flatten
        if (flattened.isEmpty) {
          verboseDefault
        } else {
          flattened.mkString(", ")
        }
      } else {
        throw new RuntimeException("cron expression '%s' is invalid, it must have 6 or 7 elements, but has %d".format(cron, rawCronParts.size))
      }
    }
  }

  def verboseToCron(verbose: String): String = {
    try {
      verboseToCronWithoutErrorChecking(verbose)
    } catch {
      case originalException: Exception => {
        val exceptionToThrow = try {
          val verboseFromCron = cronToVerbose(verbose)
          new RuntimeException("'%s' appears to be a cron expression, try '%s' instead".format(verbose, verboseFromCron))
        } catch {
          case exceptionWhenCheckingIfCron: Exception => originalException
        }
        throw exceptionToThrow
      }
    }
  }

  private def verboseToCronWithoutErrorChecking(verbose: String): String = {
    if (verbose == verboseDefault) cronDefault
    else {
      val verboseParts = verbose.split(",").toSeq.map(s => s.trim)
      val parsedParts = verboseParts.map(CronPatternMatcher.verboseCronElement)
      def addToMap(map: Map[CronTimeUnitEnum, CronMatchResult], parsed: (CronTimeUnitEnum, CronMatchResult)) = {
        val (timeUnit, cronMatchResult) = parsed
        if (map.contains(timeUnit)) throw new RuntimeException("time unit %s is specificed twice in expression '%s'".format(timeUnit.pluralName, verbose))
        map + (timeUnit -> cronMatchResult)
      }
      val map: Map[CronTimeUnitEnum, CronMatchResult] = parsedParts.foldLeft(Map[CronTimeUnitEnum, CronMatchResult]())(addToMap)
      def verboseToCron(timeUnit: CronTimeUnitEnum) = {
        if (map.contains(timeUnit)) {
          val cronMatch = map(timeUnit)
          cronMatch.formatCron(timeUnit)
        } else timeUnit.defaultValue
      }
      val cronElements = CronTimeUnitEnum.values.map(verboseToCron)
      cronElements.mkString(" ")
    }
  }
}

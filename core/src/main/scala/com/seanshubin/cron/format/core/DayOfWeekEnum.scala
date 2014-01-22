package com.seanshubin.cron.format.core

import scala.collection.mutable.ArrayBuffer

sealed abstract case class DayOfWeekEnum(ordinal: Int, name: String, shortName: String) {
  DayOfWeekEnum.valuesBuffer += this
}

object DayOfWeekEnum {
  private val valuesBuffer = new ArrayBuffer[DayOfWeekEnum]
  lazy val values = valuesBuffer.toSeq
  val Sunday = new DayOfWeekEnum(1, "sunday", "sun") {}
  val Monday = new DayOfWeekEnum(2, "monday", "mon") {}
  val Tuesday = new DayOfWeekEnum(3, "tuesday", "tue") {}
  val Wednesday = new DayOfWeekEnum(4, "wednesday", "wed") {}
  val Thursday = new DayOfWeekEnum(5, "thursday", "thu") {}
  val Friday = new DayOfWeekEnum(6, "friday", "fri") {}
  val Saturday = new DayOfWeekEnum(7, "saturday", "sat") {}

  def fromName(name: String) = {
    val maybeDayOfWeek = values.find(dayOfWeek => dayOfWeek.name == name || dayOfWeek.shortName == name)
    maybeDayOfWeek match {
      case Some(dayOfWeek) => dayOfWeek
      case None => {
        val dayOfWeekNames = values.flatMap(x => Seq(x.name, x.shortName))
        throw new RuntimeException("'%s' does not match a valid day of week, valid values are %s".format(name, dayOfWeekNames.mkString(", ")))
      }
    }
  }
}

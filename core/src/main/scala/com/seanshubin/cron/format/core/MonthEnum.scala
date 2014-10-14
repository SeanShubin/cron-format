package com.seanshubin.cron.format.core

import scala.collection.mutable.ArrayBuffer

sealed abstract case class MonthEnum(ordinal: Int, name: String, shortName: String) {
  MonthEnum.valuesBuffer += this
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
}

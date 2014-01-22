package com.seanshubin.cron.format.core

import scala.collection.mutable.ArrayBuffer

sealed abstract case class MonthEnum(ordinal: Int, name: String, shortName: String) {
  MonthEnum.valuesBuffer += this
}

object MonthEnum {
  private val valuesBuffer = new ArrayBuffer[MonthEnum]
  lazy val values = valuesBuffer.toSeq
  val January = new MonthEnum(1, "january", "jan") {}
  val February = new MonthEnum(2, "february", "feb") {}
  val March = new MonthEnum(3, "march", "mar") {}
  val April = new MonthEnum(4, "april", "apr") {}
  val May = new MonthEnum(5, "may", "may") {}
  val June = new MonthEnum(6, "june", "jun") {}
  val July = new MonthEnum(7, "july", "jul") {}
  val August = new MonthEnum(8, "august", "aug") {}
  val September = new MonthEnum(9, "september", "sep") {}
  val October = new MonthEnum(10, "october", "oct") {}
  val November = new MonthEnum(11, "november", "nov") {}
  val December = new MonthEnum(12, "december", "dec") {}
}

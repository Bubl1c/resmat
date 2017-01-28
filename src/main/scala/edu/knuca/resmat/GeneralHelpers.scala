package edu.knuca.resmat

import org.joda.time.format.{DateTimeFormat, PeriodFormatterBuilder}
import org.joda.time.{DateTime, DateTimeZone}

object GeneralHelpers {

  /**
    * MySql default date-time type format used for DATETIME columns with default precision.
    */
  val MysqlDefaultDateTimeFormat = "yyyy-MM-dd HH:mm:ss"

  def toUTC(timestamp: Long): DateTime = new DateTime(timestamp).withZone(DateTimeZone.UTC)

  /**
    * Format current UTC time as MySql date-time, using [[GeneralHelpers.MysqlDefaultDateTimeFormat]] format.
    *
    * @return formatted current time as date-time
    */
  def currentMysqlTime: String = DateTime.now(DateTimeZone.UTC).toString(MysqlDefaultDateTimeFormat)

  /**
    * Format date as MySql date-time using [[GeneralHelpers.MysqlDefaultDateTimeFormat]]
    *
    * @param date date to format
    * @return formatted as date-time MySql string
    */
  def toMysql(date: DateTime): String = date.withZone(DateTimeZone.UTC)
    .toString(DateTimeFormat.forPattern(MysqlDefaultDateTimeFormat))

  /**
    * Format timestamp as MySql date-time using [[GeneralHelpers.MysqlDefaultDateTimeFormat]]
    *
    * @param timestamp timestamp in milliseconds
    * @return formatted timestamp as date-time MySql string
    */
  def toMysql(timestamp: Long): String =
    new DateTime(timestamp).withZone(DateTimeZone.UTC).toString(MysqlDefaultDateTimeFormat)

  def recipientsToSeq(recipients: Option[String]): Seq[String] = {
    recipients match {
      case None => Seq.empty
      case null => Seq.empty
      case rs if rs.isEmpty => Seq.empty
      case Some(rs) => rs.split(",")
    }
  }

  def seqToRecipients(recipients: Seq[String]): String = {
    recipients match {
      case rs if rs.isEmpty => null
      case rs if rs.nonEmpty => rs.mkString(",")
    }
  }

  def periodFormatter =
    new PeriodFormatterBuilder()
      .appendYears()
      .appendSuffix(" years ")
      .appendMonths()
      .appendSuffix(" months")
      .toFormatter

}
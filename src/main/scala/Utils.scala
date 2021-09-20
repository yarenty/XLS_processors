package com.yarenty.xls


import org.apache.poi.ss.usermodel.{CellType, Row}
import org.joda.time.format.DateTimeFormatter
import org.joda.time.{DateTime, DateTimeZone}

object Utils {


  def getString(row: Row, id: Int, default: String = ""): String = {
    val r = Option(row.getCell(id, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL)) // lift null to None
    if (r.isDefined) {
      if (r.get.getCellType == CellType.STRING) {
        r.get.getStringCellValue.trim.replaceAll("\n", "")
          .replaceAll("\t", "")
          .replaceAll("\"", "")
          .replaceAll(" ", " ")
      } else if (r.get.getCellType == CellType.BOOLEAN) {
        r.get.getBooleanCellValue.toString
      } else {
        r.get.getNumericCellValue.toString
      }
    } else default
  }


  def getInt(row: Row, id: Int, default: Int = 0): Int = {
    val r = Option(row.getCell(id, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL)) // lift null to None
    if (r.get.getCellType == CellType.STRING) {
      if (r.isDefined) r.get.getStringCellValue.toInt else default
    } else {
      if (r.isDefined) r.get.getNumericCellValue.toInt else default
    }
  }


  def getLong(row: Row, id: Int, default: Long = 0): Long = {
    val r = Option(row.getCell(id, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL)) // lift null to None
    if (r.get.getCellType == CellType.STRING) {
      if (r.isDefined) r.get.getStringCellValue.toLong else default
    } else {
      if (r.isDefined) r.get.getNumericCellValue.toLong else default
    }
  }

  def getDate(row: Row, id: Int, formatter: DateTimeFormatter, default: DateTime = new DateTime(DateTimeZone.UTC)): DateTime = {
    val r = Option(row.getCell(id, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL)) // lift null to None
    if (r.isDefined) {
      if (r.get.getCellType == CellType.STRING) {
        if (r.get.getStringCellValue.length > 4) formatter.parseDateTime(r.get.getStringCellValue.substring(0, 18)) else default
      }
      else {
        //				if (r.get.getCellType == CellType.STRING) {
        //					if (r.isDefined) new DateTime(r.get.getStringCellValue.toLong, DateTimeZone.UTC) else default
        //				} else {
        //					if (r.isDefined) r.get.getNumericCellValue.toLong else default
        //				}
        ////				new DateTime(r.get.getDateCellValue.getTime, DateTimeZone.UTC)
        //				println(r.get.getCellType)
        new DateTime(r.get.getNumericCellValue.toLong, DateTimeZone.UTC)
      }
    } else default
  }

  def getBoolean(row: Row, id: Int, default: Boolean = false): Boolean = {
    val r = Option(row.getCell(id, Row.MissingCellPolicy.RETURN_BLANK_AS_NULL))
    if (r.isDefined) {
      r.get.getStringCellValue == "Y"
    } else {
      default
    }
  }
}
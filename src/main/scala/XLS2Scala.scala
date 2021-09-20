package com.yarenty.xls

import org.apache.log4j.Logger
import org.apache.poi.ss.usermodel.*

import java.io.{File, FileInputStream, PrintWriter}
import scala.language.postfixOps
import org.apache.poi.ss.usermodel.Workbook
import org.apache.poi.xssf.usermodel.XSSFWorkbook

case class Fields(claz: String, field: String, default: String, desc: String)


object XLS2Scala {

  val logger = Logger.getLogger(getClass.getName)


  def process(inputFile: String) :List[Fields] = {
    logger.info("Processing: " + inputFile)
    
    /* First need to open the file. *//* First need to open the file. */
    val fInputStream = new FileInputStream(inputFile.trim)

    /* Create the workbook object. */ 
    val workbook = new XSSFWorkbook(fInputStream)
    
    val sheet = workbook.getSheetAt(0)

    var claz: String = ""
    var field: String = ""
    var default: String = ""
    var description: String = ""

    logger.debug("ROWS:" + sheet.getLastRowNum)
    val firstRowNum = sheet.getFirstRowNum
    val lastRowNum = sheet.getLastRowNum
    
    val out = 
      for( i <- firstRowNum until lastRowNum )  yield {
       val row = sheet.getRow(i)

      claz = Utils.getString(row, 0)
      field = Utils.getString(row, 1)
      default = Utils.getString(row, 2)
      description = Utils.getString(row, 3)

      Fields(claz, field, default, description)
    } 

    workbook.close()
    logger.info("imported: " + out.size)
    out.toList
  }

  def fields2Scala(f: List[Fields]): Unit = {
    val vars = new StringBuilder()
    val methods = new StringBuilder()
    val detectionFunctions = new StringBuilder()
    val functionList = new StringBuilder()
    val claz:String = f.head.claz
    
   

    for (x <- f) {
      val v = x.field
      if (v.length>0) then
        val VAR = v.toUpperCase.replaceAll("\\.", "_").replaceAll("-","_")
        val mname = v.charAt(0) +  v.replaceAll("-",".").split("\\.").map(_.capitalize).mkString("").substring(1)
        vars
          .append("\n//")
          .append(x.desc)
          .append("\n val ")
          .append(VAR)
          .append(" = \"")
          .append(v)
          .append("\"\n")
        methods.append("\ndef ")
          .append(mname)
          .append("(data: SparkApplicationData): String =\n")
          .append("\tdata.getSparkProperty(")
          .append(VAR)
        .append(s").getOrElse(\"Default:${x.default}\")\n\n")
        
        functionList.append(mname).append("Detection,")


        detectionFunctions.append("\ndef ")
          .append(mname)
          .append("Detection(data: SparkApplicationData) = HeuristicResultDetails(\n")
          .append(VAR)
          .append(",\n")
          .append(mname)
          .append("(data),\n")
          .append("\"").append(x.desc).append("\"\n")
          .append(")\n\n")
      
    }





    val f1 =f"Configuration${claz}Heuristic.scala"
    val pw1 = new PrintWriter(f1)
    // Passing reference of file to the printwriter.
    pw1.write(
      s"""
         |package com.sparkman.spark.heuristics.configuration.${claz.toLowerCase()}
         |
         |import com.sparkman.core.analysis.heuristics.Heuristic
         |import com.sparkman.core.configurations.HeuristicConfig
         |import com.sparkman.spark.data.SparkApplicationData
         |
         |/**
         | * A heuristic based on an app's known configuration.
         | *
         | * The results from this heuristic primarily inform users about key app SQL configuration settings:
         | *  @see: https://spark.apache.org/docs/latest/configuration.html#${claz}
         | *
         | * It also checks whether the values specified are within threshold.
         | */
         |class Configuration${claz}Heuristic(private val heuristicConfigurationData: HeuristicConfig)
         |    extends Heuristic[SparkApplicationData]
         |    with Configuration${claz}DetectionFunctions
         |    with Configuration${claz}RecommendationFunctions {
         |
         |  override def getHeuristicConfData(): HeuristicConfig = heuristicConfigurationData
         |
         |}
         |
         |
         |""".stripMargin) // Writing to the file.
    pw1.close() // Closing printwriter.



    val f2 = "Configuration" +claz +"HeuristicFunctions.scala"
    val pw2 = new PrintWriter(f2)
    pw2.write(
      s"""
         |package com.sparkman.spark.heuristics.configuration.${claz.toLowerCase()}
         |
         |import com.sparkman.spark.data.SparkApplicationData
         |
         |import scala.util.Try
         |
         |trait Configuration${claz}HeuristicFunctions {
         |
         |${vars.toString()}
         |
         |${methods.toString()}
         |
         |}
         |""".stripMargin)
    pw2.close()


    val f3 = "Configuration" +claz +"DetectionFunctions.scala"
    val pw3 = new PrintWriter(f3)
    pw3.write(
    s"""
      |package com.sparkman.spark.heuristics.configuration.${claz.toLowerCase()}
      |
      |import com.sparkman.core.analysis.heuristics.{ HeuristicDetectionFunctions, HeuristicResultDetails }
      |import com.sparkman.core.utils.MemoryFormatUtils
      |import com.sparkman.spark.data.SparkApplicationData
      |
      |trait Configuration${claz}DetectionFunctions
      |    extends HeuristicDetectionFunctions[SparkApplicationData]
      |    with Configuration${claz}HeuristicFunctions {
      |
      |  override def getDetectionFunctions: List[SparkApplicationData => HeuristicResultDetails] =
      |    List(
      |      ${functionList.toString().substring(0,functionList.length()-1)}
      |    )
      |
      |${detectionFunctions}
      |
      |}
      |""".stripMargin
    )
    pw3.close()


    val f4 = "Configuration" +claz +"RecommendationFunctions.scala"
    val pw4 = new PrintWriter(f4)
    pw4.write(
    s"""package com.sparkman.spark.heuristics.configuration.${claz.toLowerCase()}
      |
      |import com.sparkman.core.analysis.SeverityThresholds
      |import com.sparkman.core.analysis.heuristics.{ HeuristicRecommendationFunctions, HeuristicResultDetails }
      |import com.sparkman.core.configurations.HeuristicConfig
      |import com.sparkman.core.model.Severity
      |import com.sparkman.core.utils.MemoryFormatUtils
      |import com.sparkman.spark.data.SparkApplicationData
      |
      |trait Configuration${claz}RecommendationFunctions
      |    extends HeuristicRecommendationFunctions[SparkApplicationData]
      |    with Configuration${claz}HeuristicFunctions {
      |
      |  override def getRecommendationFunctions
      |    : List[(SparkApplicationData, HeuristicConfig) => Option[HeuristicResultDetails]] =
      |    List(
      |     tempRecommendation
      |    //put it here
      |    )
      |
      |  override def severity(data: SparkApplicationData, heuristicConfig: HeuristicConfig): Severity.Value = Severity.NONE
      |
      |
      |  def tempRecommendation(data: SparkApplicationData, heuristicConfig: HeuristicConfig) = None
      |   // if (tempSeverity(data) != Severity.NONE) {
      |   //   Some(
      |   //     HeuristicResultDetails(
      |   //       "filed",
      |   //       "value",
      |   //      "description"
      |   //     )
      |   //   )
      |   // } else None
      |
      |  private def tempSeverity(data: SparkApplicationData): Severity.Value = Severity.NONE
      |  //temp(data) match {
      |  //  case None => Severity.NONE
      |  //  case _    => Severity.MODERATE
      |  //}
      |
      |
      |}
      |""".stripMargin)
    pw4.close()
    
  }

  def main(args: Array[String]): Unit = {

//    val l = process("/opt/workspace/xls_processors/input.xlsx")
    val l = process("input.xlsx")
    l.groupBy(_.claz).map(g => fields2Scala(g._2))

  }

}

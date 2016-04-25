import java.io._
import scala.io.Source

case class DataRow(name: String, dataArray: Array[Long])
case class Csv(headers: Array[String], rows: Array[DataRow], minTestCount: Int, maxTestCount: Int)
case class Chart(name: String, title: String, data: Csv)

def readCsv(srcDir: File, filePath: String): Csv = { // Does not cater for csv escape, should be fine for our data.
  val lines = Source.fromFile(new File(srcDir, filePath)).getLines().toList
  if (lines.length == 0)
    throw new RuntimeException(filePath + " is empty.")
  val tableWithPossiblyUnequalRowsLengths = lines.map(_.split(",").toList)
  val headersCount = tableWithPossiblyUnequalRowsLengths(0).length
  if (headersCount <= 1)
    throw new RuntimeException(filePath + " does not contains valid data.")
  val minTestCount = tableWithPossiblyUnequalRowsLengths(0)(1).toInt
  val maxTestCount = tableWithPossiblyUnequalRowsLengths(0)(headersCount -1).toInt
   // This makes all rows the same, so I can transpose (rows may not be the same because of an OOM exception)
  val tableWithEqualRowLengths = for (xs <- tableWithPossiblyUnequalRowsLengths) yield xs.padTo(headersCount, "0")
  // val table = lines.map(_.split(",").toList).transpose
  val table = tableWithEqualRowLengths.transpose
  val headers = table(0).map(_.trim).toArray
  val rows = table.drop(1).map { dataList => 
    val name = dataList(0)
    val dataArray = dataList.drop(1).map(_.toLong).toArray
    DataRow(name, dataArray)
  }.toArray
  new Csv(headers, rows, minTestCount, maxTestCount)
}

def filterCsv(csv: Csv, filterFun: DataRow => Boolean): Csv = {
  val filteredRows = csv.rows.filter(filterFun(_))
  val testCounts = filteredRows.map(_.name.toInt)
  val minTestCount = testCounts.min
  val maxTestCount = testCounts.max
  Csv(csv.headers, filteredRows, minTestCount, maxTestCount)
}

def generateLineChart(charts: Array[Chart]) = 
  <html>
  <head>
    <script type="text/javascript" src="https://www.google.com/jsapi"></script>
    <script type="text/javascript">
    {
      scala.xml.Unparsed("google.load(\"visualization\", \"1\", {packages:[\"corechart\"]});\n" +
      "google.setOnLoadCallback(drawChart);\n" + 
      "function drawChart() {\n" + 
      charts.map { chart => 
        "  var " + chart.name + "Data = google.visualization.arrayToDataTable([\n" + 
        "      [" + chart.data.headers.map("'" + _ + "'").mkString(",") + "],\n" +
        chart.data.rows.map(d => "    ['" + d.name + "'," + d.dataArray.mkString(",") + "]").mkString(",\n") +
        "  ]);" + "\n" + 
        "  var " + chart.name + "Options = {\n" + 
        "    title: '" + chart.title + "',\n" + 
        "    legend: { position: 'bottom' },\n" + 
        "    colors: ['blue', 'green', 'yellow', 'red']\n" +
        "  };\n" + 
        "  var " + chart.name + "Chart = new google.visualization.LineChart(document.getElementById('" + chart.name + "_div'));\n" + 
        "  " + chart.name + "Chart.draw(" + chart.name + "Data, " + chart.name + "Options);\n"
      }.mkString("\n") + 
      "}")
    }
    </script>
  </head>
  <body>
    {
      scala.xml.Unparsed(charts.map { chart =>
        "<table><tr><td><div id=\"" + chart.name + "_div\" style=\"width: 1100px; height: 550px;\"></div></td><td></td></tr></table>"
      }.mkString("\n"))
    }
  </body>
</html>

abstract class WhichChart
case object FirstChart extends WhichChart
case object SecondChart extends WhichChart
case object BothCharts extends WhichChart

def generateChartFile(srcDir: File, targetFile: File, whichChart: WhichChart = BothCharts) {

  val durationData = readCsv(srcDir, "duration.csv")
  val fileSizeData = readCsv(srcDir, "filesize.csv")
  val fileCountData = readCsv(srcDir, "filecount.csv")

  val graphFile = new FileWriter(targetFile)

  println(durationData.minTestCount + " " + durationData.maxTestCount + " " + durationData.rows.length)
  val durationFirst =
    whichChart match {
      case SecondChart => List.empty[Chart]
      case _ => List(Chart("duration100", "Compile time (milliseconds)", filterCsv(durationData, _.name.toLong <= 100)))
    }

  val durationSecond =
    whichChart match {
      case FirstChart => List.empty[Chart]
      case _ => List[Chart](Chart("duration500", "Compile time (milliseconds)", filterCsv(durationData, (row: DataRow) => row.name.toLong == 0 || row.name.toLong >= 100)))
    }

  val fileSizeFirst =
    whichChart match {
      case SecondChart => List.empty[Chart]
      case _ => List(Chart("filesize100", "File Size (bytes)", filterCsv(fileSizeData, _.name.toLong <= 100)))
    }

  val fileSizeSecond = 
    whichChart match {
      case FirstChart => List.empty[Chart]
      case _ => List[Chart](Chart("filesize500", "File Size (bytes)", filterCsv(fileSizeData, (row: DataRow) => row.name.toLong == 0 || row.name.toLong >= 100)))
   }

  val fileCountFirst =
    whichChart match {
      case SecondChart => List.empty[Chart]
      case _ => List(Chart("filecount100", "File Count", filterCsv(fileCountData, _.name.toLong <= 100)))
    }

  val fileCountSecond =
    whichChart match {
      case FirstChart => List.empty[Chart]
      case _ => List[Chart](Chart("filecount500", "File Count", filterCsv(fileCountData, (row: DataRow) => row.name.toLong == 0 || row.name.toLong >= 100)))
    }

  val chartList: List[Chart] = 
   durationFirst ::: durationSecond ::: fileSizeFirst ::: fileSizeSecond ::: fileCountFirst ::: fileCountSecond

  graphFile.write(
    generateLineChart(
      chartList.toArray
    ).toString
  )
  graphFile.flush()
  graphFile.close()
  
  println("Generated " + targetFile.getAbsolutePath)
}

val allTestsInOneFile210Dir = new File("target/2.10/allTestsInOneFile")
val allTestsInOneFile210StatDir = new File(allTestsInOneFile210Dir, "stat")
if (allTestsInOneFile210StatDir.exists) 
  generateChartFile(allTestsInOneFile210StatDir, new File(allTestsInOneFile210Dir, "allTestsInOneFile-graph.html"))
else
  println("target/2.10/allTestsInOneFile/stat directory does not exist, target/2.10/allTestsInOneFile/allTestsInOneFile-graph.html will not be generated.")

val allTestsInOneFile211Dir = new File("target/2.11/allTestsInOneFile")
val allTestsInOneFile211StatDir = new File(allTestsInOneFile211Dir, "stat")
if (allTestsInOneFile211StatDir.exists) 
  generateChartFile(allTestsInOneFile211StatDir, new File(allTestsInOneFile211Dir, "allTestsInOneFile-graph.html"))
else
  println("target/2.11/allTestsInOneFile/stat directory does not exist, target/2.11/allTestsInOneFile/allTestsInOneFile-graph.html will not be generated.")
  
val tenTestsPerFile210Dir = new File("target/2.10/tenTestsPerFile")
val tenTestsPerFile210StatDir = new File(tenTestsPerFile210Dir, "stat")
if (tenTestsPerFile210StatDir.exists)
  generateChartFile(tenTestsPerFile210StatDir, new File(tenTestsPerFile210Dir, "tenTestsPerFile-graph.html"))
else
  println("target/2.10/tenTestsPerFile/stat directory does not exist, target/2.10/tenTestsPerFile/tenTestsPerFile-graph.html will not be generated.")

val tenTestsPerFile211Dir = new File("target/2.11/tenTestsPerFile")
val tenTestsPerFile211StatDir = new File(tenTestsPerFile211Dir, "stat")
if (tenTestsPerFile211StatDir.exists)
  generateChartFile(tenTestsPerFile211StatDir, new File(tenTestsPerFile211Dir, "tenTestsPerFile-graph.html"))
else
  println("target/2.11/tenTestsPerFile/stat directory does not exist, target/2.11/tenTestsPerFile/tenTestsPerFile-graph.html will not be generated.")
  
val testsIn100Files210Dir = new File("target/2.10/testsIn100Files")
val testsIn100Files210StatDir = new File(testsIn100Files210Dir, "stat")
if (testsIn100Files210StatDir.exists)
  generateChartFile(testsIn100Files210StatDir, new File(testsIn100Files210Dir, "testsIn100Files-graph.html"), FirstChart)
else
  println("target/2.10/testsIn100Files/stat directory does not exist, target/2.10/testsIn100Files/tenTestsPerFile-graph.html will not be generated.")

val testsIn100Files211Dir = new File("target/2.11/testsIn100Files")
val testsIn100Files211StatDir = new File(testsIn100Files211Dir, "stat")
if (testsIn100Files211StatDir.exists)
  generateChartFile(testsIn100Files211StatDir, new File(testsIn100Files211Dir, "testsIn100Files-graph.html"), FirstChart)
else
  println("target/2.11/testsIn100Files/stat directory does not exist, target/2.11/testsIn100Files/tenTestsPerFile-graph.html will not be generated.")

val dataTables210Dir = new File("target/2.10/dataTables")
val dataTables210StatDir = new File(dataTables210Dir, "stat")
if (dataTables210StatDir.exists)
  generateChartFile(dataTables210StatDir, new File(dataTables210Dir, "dataTables-graph.html"))
else
  println("target/2.10/dataTables/stat directory does not exist, target/2.10/dataTables/dataTables-graph.html will not be generated.")

val dataTables211Dir = new File("target/2.11/dataTables")
val dataTables211StatDir = new File(dataTables211Dir, "stat")
if (dataTables211StatDir.exists)
  generateChartFile(dataTables211StatDir, new File(dataTables211Dir, "dataTables-graph.html"))
else
  println("target/2.11/dataTables/stat directory does not exist, target/2.11/dataTables/dataTables-graph.html will not be generated.")
  
val allMethodTestsInOneFile210Dir = new File("target/2.10/allMethodTestsInOneFile")
val allMethodTestsInOneFile210StatDir = new File(allMethodTestsInOneFile210Dir, "stat")
if (allMethodTestsInOneFile210StatDir.exists)
  generateChartFile(allMethodTestsInOneFile210StatDir, new File(allMethodTestsInOneFile210Dir, "allMethodTestsInOneFile-graph.html"), SecondChart)
else
  println("target/2.10/allMethodTestsInOneFile/stat directory does not exist, target/2.10/allMethodTestsInOneFile/allMethodTestsInOneFile-graph.html will not be generated.")
  
val allMethodTestsInOneFile211Dir = new File("target/2.11/allMethodTestsInOneFile")
val allMethodTestsInOneFile211StatDir = new File(allMethodTestsInOneFile211Dir, "stat")
if (allMethodTestsInOneFile211StatDir.exists)
  generateChartFile(allMethodTestsInOneFile211StatDir, new File(allMethodTestsInOneFile211Dir, "allMethodTestsInOneFile-graph.html"), SecondChart)
else
  println("target/2.11/allMethodTestsInOneFile/stat directory does not exist, target/2.11/allMethodTestsInOneFile/allMethodTestsInOneFile-graph.html will not be generated.")

val assertTestsInOneFile210Dir = new File("target/2.10/assertTestsInOneFile")
val assertTestsInOneFile210StatDir = new File(assertTestsInOneFile210Dir, "stat")
if (assertTestsInOneFile210StatDir.exists)
  generateChartFile(assertTestsInOneFile210StatDir, new File(assertTestsInOneFile210Dir, "assertTestsInOneFile-graph.html"))
else
  println("target/2.10/assertTestsInOneFile/stat directory does not exist, target/2.10/assertTestsInOneFile/assertTestsInOneFile-graph.html will not be generated.")

val assertTestsInOneFile211Dir = new File("target/2.11/assertTestsInOneFile")
val assertTestsInOneFile211StatDir = new File(assertTestsInOneFile211Dir, "stat")
if (assertTestsInOneFile211StatDir.exists)
  generateChartFile(assertTestsInOneFile211StatDir, new File(assertTestsInOneFile211Dir, "assertTestsInOneFile-graph.html"))
else
  println("target/2.11/assertTestsInOneFile/stat directory does not exist, target/2.11/assertTestsInOneFile/assertTestsInOneFile-graph.html will not be generated.")
  
val allClassTestsInOneFile210Dir = new File("target/2.10/allClassTestsInOneFile")
val allClassTestsInOneFile210StatDir = new File(allClassTestsInOneFile210Dir, "stat")
if (allClassTestsInOneFile210StatDir.exists) 
  generateChartFile(allClassTestsInOneFile210StatDir, new File(allClassTestsInOneFile210Dir, "allClassTestsInOneFile-graph.html"))
else
  println("target/2.10/allClassTestsInOneFile/stat directory does not exist, target/2.10/allClassTestsInOneFile/allClassTestsInOneFile-graph.html will not be generated.")

val allClassTestsInOneFile211Dir = new File("target/2.11/allClassTestsInOneFile")
val allClassTestsInOneFile211StatDir = new File(allClassTestsInOneFile211Dir, "stat")
if (allClassTestsInOneFile211StatDir.exists) 
  generateChartFile(allClassTestsInOneFile211StatDir, new File(allClassTestsInOneFile211Dir, "allClassTestsInOneFile-graph.html"))
else
  println("target/2.11/allClassTestsInOneFile/stat directory does not exist, target/2.11/allClassTestsInOneFile/allClassTestsInOneFile-graph.html will not be generated.")

val assertMacro210Dir = new File("target/2.10/assertMacro")
val assertMacro210StatDir = new File(assertMacro210Dir, "stat")
if (assertMacro210StatDir.exists) 
  generateChartFile(assertMacro210StatDir, new File(assertMacro210Dir, "assertMacro-graph.html"))
else
  println("target/2.10/assertMacro/stat directory does not exist, target/2.10/assertMacro/assertMacro-graph.html will not be generated.")

val assertMacro211Dir = new File("target/2.11/assertMacro")
val assertMacro211StatDir = new File(assertMacro211Dir, "stat")
if (assertMacro211StatDir.exists) 
  generateChartFile(assertMacro211StatDir, new File(assertMacro211Dir, "assertMacro-graph.html"))
else
  println("target/2.11/assertMacro/stat directory does not exist, target/2.11/assertMacro/assertMacro-graph.html will not be generated.")

val scalautilsScalaz210Dir = new File("target/2.10/scalautilsScalaz")
val scalautilsScalaz210StatDir = new File(scalautilsScalaz210Dir, "stat")
if (scalautilsScalaz210StatDir.exists)
  generateChartFile(scalautilsScalaz210StatDir, new File(scalautilsScalaz210Dir, "scalautilsScalaz-graph.html"))
else
  println("target/2.10/scalautilsScalaz/stat directory does not exist, target/2.10/scalautilsScalaz/scalautilsScalaz-graph.html will not be generated.")

val scalautilsScalaz211Dir = new File("target/2.11/scalautilsScalaz")
val scalautilsScalaz211StatDir = new File(scalautilsScalaz211Dir, "stat")
if (scalautilsScalaz211StatDir.exists)
  generateChartFile(scalautilsScalaz211StatDir, new File(scalautilsScalaz211Dir, "scalautilsScalaz-graph.html"))
else
  println("target/2.11/scalautilsScalaz/stat directory does not exist, target/2.11/scalautilsScalaz/scalautilsScalaz-graph.html will not be generated.")

val shapelessTables210Dir = new File("target/2.10/shapelessTables")
val shapelessTables210StatDir = new File(shapelessTables210Dir, "stat")
if (shapelessTables210StatDir.exists)
  generateChartFile(shapelessTables210StatDir, new File(shapelessTables210Dir, "shapelessTables-graph.html"))
else
  println("target/2.10/shapelessTables/stat directory does not exist, target/2.10/shapelessTables/shapelessTables-graph.html will not be generated.")

val shapelessTables211Dir = new File("target/2.11/shapelessTables")
val shapelessTables211StatDir = new File(shapelessTables211Dir, "stat")
if (shapelessTables211StatDir.exists)
  generateChartFile(shapelessTables211StatDir, new File(shapelessTables211Dir, "shapelessTables-graph.html"))
else
  println("target/2.11/shapelessTables/stat directory does not exist, target/2.11/shapelessTables/shapelessTables-graph.html will not be generated.")

val scalatest3211Dir = new File("target/2.11/scalatest3")
val scalatest3211StatDir = new File(scalatest3211Dir, "stat")
if (scalatest3211StatDir.exists)
  generateChartFile(scalatest3211StatDir, new File(scalatest3211Dir, "scalatest3-graph.html"))
else
  println("target/2.11/scalatest3/stat directory does not exist, target/2.11/scalatest3/scalatest3-graph.html will not be generated.")

val supersafe211Dir = new File("target/2.11/supersafe")
val supersafe211StatDir = new File(supersafe211Dir, "stat")
if (supersafe211StatDir.exists)
  generateChartFile(supersafe211StatDir, new File(supersafe211Dir, "supersafe-graph.html"))
else
  println("target/2.11/supersafe/stat directory does not exist, target/2.11/supersafe/supersafe-graph.html will not be generated.")

val assertBeforeAfter210Dir = new File("target/2.10/assertBeforeAfter")
val assertBeforeAfter210StatDir = new File(assertBeforeAfter210Dir, "stat")
if (assertBeforeAfter210StatDir.exists) 
  generateChartFile(assertBeforeAfter210StatDir, new File(assertBeforeAfter210Dir, "assertBeforeAfter-graph.html"))
else
  println("target/2.10/assertBeforeAfter/stat directory does not exist, target/2.10/assertBeforeAfter/assertBeforeAfter-graph.html will not be generated.")

val assertBeforeAfter211Dir = new File("target/2.11/assertBeforeAfter")
val assertBeforeAfter211StatDir = new File(assertBeforeAfter211Dir, "stat")
if (assertBeforeAfter211StatDir.exists) 
  generateChartFile(assertBeforeAfter211StatDir, new File(assertBeforeAfter211Dir, "assertBeforeAfter-graph.html"))
else
  println("target/2.11/assertBeforeAfter/stat directory does not exist, target/2.11/assertBeforeAfter/assertBeforeAfter-graph.html will not be generated.")

val matcherBeforeAfter210Dir = new File("target/2.10/matcherBeforeAfter")
val matcherBeforeAfter210StatDir = new File(matcherBeforeAfter210Dir, "stat")
if (matcherBeforeAfter210StatDir.exists) 
  generateChartFile(matcherBeforeAfter210StatDir, new File(matcherBeforeAfter210Dir, "matcherBeforeAfter-graph.html"))
else
  println("target/2.10/matcherBeforeAfter/stat directory does not exist, target/2.10/matcherBeforeAfter/matcherBeforeAfter-graph.html will not be generated.")

val matcherBeforeAfter211Dir = new File("target/2.11/matcherBeforeAfter")
val matcherBeforeAfter211StatDir = new File(matcherBeforeAfter211Dir, "stat")
if (matcherBeforeAfter211StatDir.exists) 
  generateChartFile(matcherBeforeAfter211StatDir, new File(matcherBeforeAfter211Dir, "matcherBeforeAfter-graph.html"))
else
  println("target/2.11/matcherBeforeAfter/stat directory does not exist, target/2.11/matcherBeforeAfter/matcherBeforeAfter-graph.html will not be generated.")

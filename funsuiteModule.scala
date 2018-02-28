import java.net._
import java.io._
import java.nio.channels.Channels
import scala.annotation.tailrec
import scala.math.pow

/*
def scalaVersion = {
  val rawVersion = scala.util.Properties.scalaPropOrElse("version.number", "unknown")
  if (rawVersion.endsWith(".final"))
    rawVersion.substring(0, rawVersion.length - 6)
  else
    rawVersion
}
*/
def scalaVersion = "2.12"
val scalaTestBeforeVersion = "3.1.0-SNAP6"
val scalaTestAfterVersion = "3.1.0-SNAP7"

def downloadFile(urlString: String, targetFile: File) {
  println("Downloading " + urlString)
  val url = new URL(urlString)
  val connection = url.openConnection
  val in = connection.getInputStream
  val out = new FileOutputStream(targetFile)
  out getChannel() transferFrom(Channels.newChannel(in), 0, Long.MaxValue)
  in.close()
  out.flush()
  out.close()
}
  
def generateBeforeSourceFile(testCount: Int, targetDir: File): File = {
  targetDir.mkdirs()
  val targetFile = new File(targetDir, "ExampleSpec.scala")
  val targetOut = new BufferedWriter(new FileWriter(targetFile))
  
  try {
    targetOut.write("package Before\n\n")
    
    targetOut.write("import org.scalatest._\n")
    targetOut.write("class ExampleSpec extends FunSuite {\n\n")
    
    for (x <- 1 to testCount) {
      targetOut.write("  test(\"test " + x + "\") {\n")
      targetOut.write("    assert(" + x + " == (" + x + " + 1 -1))\n")
      targetOut.write("  }\n\n")
    }

    targetOut.write("}\n")
  }
  finally {
    targetOut.flush()
    targetOut.close()
  }
  targetFile
}

def generateAfterSourceFile(testCount: Int, targetDir: File): File = {
  targetDir.mkdirs()
  val targetFile = new File(targetDir, "ExampleSpec.scala")
  val targetOut = new BufferedWriter(new FileWriter(targetFile))
  
  try {
    targetOut.write("package After\n\n")
    
    targetOut.write("import org.scalatest.funsuite.AnyFunSuite\n")
    targetOut.write("class ExampleSpec extends AnyFunSuite {\n\n")
    
    for (x <- 1 to testCount) {
      targetOut.write("  test(\"test " + x + "\") {\n")
      targetOut.write("    assert(" + x + " == (" + x + " + 1 -1))\n")
      targetOut.write("  }\n\n")
    }

    targetOut.write("}\n")
  }
  finally {
    targetOut.flush()
    targetOut.close()
  }
  targetFile
}

def compile(srcFile: String, classpath: String, targetDir: String) = {
  import scala.collection.JavaConversions._

  val command = List("scalac", "-classpath", classpath, "-d", targetDir, srcFile)
  val builder = new ProcessBuilder(command)
  builder.redirectErrorStream(true)
  val start = System.currentTimeMillis
  val process = builder.start()

  val stdout = new BufferedReader(new InputStreamReader(process.getInputStream))

  var line = "Compiling " + srcFile + "..."
  while (line != null) {
    println (line)
    line = stdout.readLine
  }
  
  val end = System.currentTimeMillis
  end - start
}

def getFileAndByteCount(srcDir: File) = {
  @tailrec
  def getFileAndByteCountAcc(dirList: Array[File], fileCount: Long, byteCount: Long): Tuple2[Long, Long] = {
    val (files, subDirs) = dirList.partition(_.isFile)
    val classFiles = files.filter(f => f.getName.endsWith(".class"))
    val newFileCount = fileCount + classFiles.size
    val newByteCount = byteCount + classFiles.map { f => f.length.toLong }.foldLeft(0l) { (a, b) => a + b }
    if (subDirs.isEmpty) 
      (newFileCount, newByteCount)
    else 
      getFileAndByteCountAcc(subDirs.flatMap(d => d.listFiles), newFileCount, newByteCount)
  }
  getFileAndByteCountAcc(srcDir.listFiles, 0l, 0l)
}

def deleteDir(targetDir: File) {
  val children = targetDir.listFiles
  if (children != null) {
    targetDir.listFiles.foreach { child => 
      if (child.isFile) 
        child.delete()
      else
        deleteDir(child)
    }
    targetDir.delete()
  }
  else
    println("Unable to list files in " + targetDir.getAbsolutePath)
}

def getOutputDir(baseOutputDir: File, testCount: Int): File = {
  val outputDirName = "output-" + testCount
  val outputDir = new File(baseOutputDir, outputDirName)
  outputDir.mkdirs()
  outputDir
}

if (scalaVersion != "unknown") {
  val scalatestBeforeJar = new File("scalatest-app_" + scalaVersion + "-" + scalaTestBeforeVersion + ".jar")
  if (!scalatestBeforeJar.exists)
    downloadFile("https://oss.sonatype.org/content/repositories/releases/org/scalatest/scalatest-app_" + scalaVersion + "/" + scalaTestBeforeVersion + "/scalatest-app_" + scalaVersion + "-" + scalaTestBeforeVersion + ".jar", scalatestBeforeJar)

  val scalatestAfterJar = new File("scalatest-app_" + scalaVersion + "-" + scalaTestAfterVersion + ".jar")
  if (!scalatestAfterJar.exists)
    downloadFile("https://oss.sonatype.org/content/repositories/releases/org/scalatest/scalatest-app_" + scalaVersion + "/" + scalaTestAfterVersion + "/scalatest-app_" + scalaVersion + "-" + scalaTestAfterVersion + ".jar", scalatestAfterJar)

  val baseDir = new File("funsuiteModule")
  if (baseDir.exists)
    deleteDir(baseDir)
    
  val statDir = new File(baseDir, "stat")
  statDir.mkdirs()

  val durationFile = new FileWriter(new File(statDir, "duration.csv"))
  val fileCountFile = new FileWriter(new File(statDir, "filecount.csv"))
  val fileSizeFile = new FileWriter(new File(statDir, "filesize.csv"))

  val scalaTestBeforeClasspath = scalatestBeforeJar.getName
  val scalaTestAfterClasspath = scalatestAfterJar.getName

  val baseOutputDir = new File(baseDir, "output")
  baseOutputDir.mkdirs()

  val baseGeneratedDir = new File(baseDir, "generated")
  baseGeneratedDir.mkdirs()

  val testCounts = 
    Array(
       10, 
       20, 
       30, 
       40, 
       50, 
       60, 
       70, 
       80, 
       90, 
      100, 
      200, 
      300, 
      400, 
      500, 
      600, 
      700, 
      800, 
      900, 
     1000
    )
  
  val headers = "TestCount," + testCounts.mkString(",") + "\n"
  durationFile.write(headers)
  fileCountFile.write(headers)
  fileSizeFile.write(headers)

  // ScalaTest Before
  durationFile.write("Before")
  durationFile.flush()
  fileCountFile.write("Before")
  fileCountFile.flush()
  fileSizeFile.write("Before")
  fileSizeFile.flush()
  try {
    testCounts.foreach { testCount =>
      println("Working on Before test count " + testCount + "...")
      val outputDir = getOutputDir(baseOutputDir, testCount)
      val generatedDir = new File(baseGeneratedDir, "generated-" + testCount)
      
      val generatedSrc = generateBeforeSourceFile(testCount, new File(generatedDir, "Before"))
      val duration = compile(generatedSrc.getAbsolutePath, scalaTestBeforeClasspath, outputDir.getAbsolutePath)
      durationFile.write("," + duration)
      durationFile.flush()

      val (fileCount, fileSize) = getFileAndByteCount(new File(outputDir, "Before"))
      fileCountFile.write("," + fileCount)
      fileCountFile.flush()
      fileSizeFile.write("," + fileSize)
      fileSizeFile.flush()
    }
  }
  catch {
    case e: Throwable => 
      e.printStackTrace()
  }
  finally {
    durationFile.write("\n")
    durationFile.flush()
    fileCountFile.write("\n")
    fileCountFile.flush()
    fileSizeFile.write("\n")
    fileSizeFile.flush()
  }

  // ScalaTest After
  durationFile.write("After")
  durationFile.flush()
  fileCountFile.write("After")
  fileCountFile.flush()
  fileSizeFile.write("After")
  fileSizeFile.flush()
  try {
    testCounts.foreach { testCount =>
      println("Working on After test count " + testCount + "...")
      val outputDir = getOutputDir(baseOutputDir, testCount)
      val generatedDir = new File(baseGeneratedDir, "generated-" + testCount)
      
      val generatedSrc = generateAfterSourceFile(testCount, new File(generatedDir, "After"))
      val duration = compile(generatedSrc.getAbsolutePath, scalaTestAfterClasspath, outputDir.getAbsolutePath)
      durationFile.write("," + duration)
      durationFile.flush()

      val (fileCount, fileSize) = getFileAndByteCount(new File(outputDir, "After"))
      fileCountFile.write("," + fileCount)
      fileCountFile.flush()
      fileSizeFile.write("," + fileSize)
      fileSizeFile.flush()
    }
  }
  catch {
    case e: Throwable => 
      e.printStackTrace()
  }
  finally {
    durationFile.write("\n")
    durationFile.flush()
    fileCountFile.write("\n")
    fileCountFile.flush()
    fileSizeFile.write("\n")
    fileSizeFile.flush()
  }

  durationFile.flush()
  durationFile.close()
  fileCountFile.flush()
  fileCountFile.close()
  fileSizeFile.flush()
  fileSizeFile.close()
}
else
  println("ERROR: Unable to detect Scala version.")

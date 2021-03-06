import java.net._
import java.io._
import java.nio.channels.Channels
import scala.annotation.tailrec
import scala.math.pow

/*
How to sort: cat st.out | sort -nr -k 2 > stSorted.out
*/
/*def scalaVersion = {
  val rawVersion = scala.util.Properties.scalaPropOrElse("version.number", "unknown")
  if (rawVersion.endsWith(".final"))
    rawVersion.substring(0, rawVersion.length - 6)
  else
    rawVersion
}*/

/*
"scalac",
"-nobootcp",
"-J-javaagent:/usr/artima/aspectj1.7/lib/aspectjweaver.jar",
"-toolcp",
"/var/folders/kq/9ywpjf5m8xl1ph006s7d7jf00000gn/T/scalac-aspects.XXXXXXXXXX.cdxf53d2/aspects.jar",
"-d /var/folders/kq/9ywpjf5m8xl1ph006s7d7jf00000gn/T/scalac-aspects.XXXXXXXXXX.cdxf53d2"
*/

def scalaVersion = "2.10"
val scalaTestVersion = "2.0"
val junitVersion = "4.11"  // JUnit depends on hamcrestVersion
val hamcrestVersion = "1.3"
val testngVersion = "6.8"
val specs2Version = "2.3.4"
val scalazVersion = "7.0.4"

val scalacAspects = "/Users/bv/nobkp/delus/scalac-aspects/scalac-aspects"

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

val classFooter = """
  }
}"""

def generateSourceFile(testCount: Int, targetDir: File, packageName: String, importStatements: Array[String], classAnnotations: Array[String], 
                       extendsName: Option[String], withNames: Array[String], scopeBracket: Boolean, scopeDef: String, 
                       testDefFun: (Int) => String, testBodyFun: (Int) => String): File = {
  targetDir.mkdirs()
  val targetFile = new File(targetDir, "ExampleSpec.scala")
  val targetOut = new BufferedWriter(new FileWriter(targetFile))
  try {
    targetOut.write("package " + packageName + "\n\n")
    importStatements.foreach { s =>
      targetOut.write("import " + s + "\n")
    }
    targetOut.write("\n")
    if (classAnnotations.length > 0)
      targetOut.write(classAnnotations.map(n => "@" + n).mkString("\n") + "\n")
    targetOut.write("class ExampleSpec")
    if (extendsName.isDefined)
      targetOut.write(" extends " + extendsName.get)
    if (withNames.length > 0)
      targetOut.write(withNames.map(n => " with " + n).mkString(" "))
    targetOut.write(" {\n")
    targetOut.write("  " + scopeDef + (if (scopeBracket) "{" else "") + " \n")
    for (x <- 1 to testCount) {
      targetOut.write("    " + testDefFun(x) + " {\n")
      targetOut.write("      " + testBodyFun(x) + "\n")
      targetOut.write("    }\n")
    }
    targetOut.write("  " + (if (scopeBracket) "}" else "") + "\n")
    targetOut.write("}\n")
  }
  finally {
    targetOut.flush()
    targetOut.close()
  }
  targetFile
}

def expectResultBodyFun(x: Int): String = "expectResult(" + (x+1) + ") { " + x + " + 1 }"
def assertEqualsBodyFun(x: Int): String = "assertEquals(" + (x+1) + ", " + x + " + 1)"
def specs2BodyFun(x: Int): String = "" + x + " + 1 must be equalTo (" + (x+1) + ")"
// Spec 
def specTestDefFun(x: Int): String = "def `increment " + x + "`"
// WordSpec
def wordSpecTestDefFun(x: Int): String = "\"increment " + x + "\" in"
// JUnit
def junitTestDefFun(x: Int): String = "@Test def increment" + x + "()"
// TestNG
def testngTestDefFun(x: Int): String = "@Test def increment" + x + "()"
// specs2 mutable.Specification
def specs2MutableTestDefFun(x: Int): String = "\"increment " + x + "\" in"

def compile(srcFile: String, classpath: String, targetDir: String, timingFile: String) = {
  import scala.collection.JavaConversions._

  val command = List( 
    "scalac",
    "-nobootcp",
    "-J-javaagent:/usr/artima/aspectj1.7/lib/aspectjweaver.jar",
    "-toolcp",
    "/var/folders/kq/9ywpjf5m8xl1ph006s7d7jf00000gn/T/scalac-aspects.XXXXXXXXXX.cdxf53d2/aspects.jar",
    "-classpath",
    classpath,
    "-d",
    targetDir,
    srcFile
  )

  val builder = new ProcessBuilder(command)
  builder.redirectErrorStream(true)
  val start = System.currentTimeMillis
  val process = builder.start()

  val stdout = new BufferedReader(new InputStreamReader(process.getInputStream))

  val outputFile = new PrintWriter(timingFile)

  try {
    var line = "Compiling " + srcFile + "..."
    while (line != null) {
      println(line)
      outputFile.println(line)
      line = stdout.readLine
    }
  }
  finally {
    outputFile.flush()
    outputFile.close()
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

case class Style(name: String, shortName: String, importNames: Array[String], classAnnotations: Array[String], extendsName: Option[String], 
                 mixinNames: Array[String], scopeBracket: Boolean, scopeDef: String, testDefFun: (Int) => String, testBodyFun: (Int) => String, 
                 classpath: String)

if (scalaVersion != "unknown") {
  val scalatestJar = new File("scalatest_" + scalaVersion + "-" + scalaTestVersion + ".jar")
  if (!scalatestJar.exists)
    downloadFile("https://oss.sonatype.org/content/repositories/releases/org/scalatest/scalatest_" + scalaVersion + "/" + scalaTestVersion + "/scalatest_" + scalaVersion + "-" + scalaTestVersion + ".jar", scalatestJar)

    
  val specs2Jar = new File("specs2_" + scalaVersion + "-" + specs2Version + ".jar")
  if (!specs2Jar.exists)
    downloadFile("https://oss.sonatype.org/content/repositories/releases/org/specs2/specs2_" + scalaVersion + "/" + specs2Version + "/specs2_" + scalaVersion + "-" + specs2Version + ".jar", specs2Jar)

  val specs2ScalazJar = new File("scalaz-core_" + scalaVersion + "-" + scalazVersion + ".jar")
  if (!specs2ScalazJar.exists)
    downloadFile("https://oss.sonatype.org/content/repositories/releases/org/scalaz/scalaz-core_" + scalaVersion + "/" + scalazVersion + "/scalaz-core_" + scalaVersion + "-" + scalazVersion + ".jar", specs2ScalazJar)

  val junitJar = new File("junit-" + junitVersion + ".jar")
  if (!junitJar.exists)
    downloadFile("http://repo1.maven.org/maven2/junit/junit/" + junitVersion + "/junit-" + junitVersion + ".jar", junitJar)
    
  val hamcrestJar = new File("hamcrest-core-" + hamcrestVersion + ".jar")
  if (!hamcrestJar.exists)
    downloadFile("http://repo1.maven.org/maven2/org/hamcrest/hamcrest-core/" + hamcrestVersion + "/hamcrest-core-" + hamcrestVersion + ".jar", hamcrestJar)

  val testngJar = new File("testng-" + testngVersion + ".jar")
  if (!testngJar.exists)
    downloadFile("http://repo1.maven.org/maven2/org/testng/testng/" + testngVersion + "/testng-" + testngVersion + ".jar", testngJar)
    
  val baseDir = new File("profiling")
  if (baseDir.exists)
    deleteDir(baseDir)
    
  val statDir = new File(baseDir, "stat")
  statDir.mkdirs()

  val durationFile = new FileWriter(new File(statDir, "duration.csv"))
  val fileCountFile = new FileWriter(new File(statDir, "filecount.csv"))
  val fileSizeFile = new FileWriter(new File(statDir, "filesize.csv"))

  val scalaTestClasspath = scalatestJar.getName
  val specs2Classpath = specs2Jar.getName + File.pathSeparator + specs2ScalazJar.getName
  val junitClasspath = junitJar.getName + File.pathSeparator + hamcrestJar.getName
  val testngClasspath = testngJar.getName + File.pathSeparator + junitClasspath

  val baseOutputDir = new File(baseDir, "output")
  baseOutputDir.mkdirs()

  val baseGeneratedDir = new File(baseDir, "generated")
  baseGeneratedDir.mkdirs()
  
  val baseTimingDir = new File(baseDir, "timing")
  baseTimingDir.mkdirs()
  
  val styles = 
    Array(
      Style(
        name = "JUnit",
        shortName = "JUnit",
        importNames = Array("org.junit.Assert.assertEquals", "org.junit.Test"),
        classAnnotations = Array.empty,
        extendsName = None,
        mixinNames = Array.empty,
        scopeBracket = false,
        scopeDef = "",
        testDefFun = junitTestDefFun,
        testBodyFun = assertEqualsBodyFun,
        classpath = junitClasspath
      ),
      Style(
        name = "scalatest.Spec",
        shortName = "Spec",
        importNames = Array("org.scalatest.Spec"),
        classAnnotations = Array.empty,
        extendsName = Some("Spec"),
        mixinNames = Array.empty,
        scopeBracket = false,
        scopeDef = "",
        testDefFun = specTestDefFun,
        testBodyFun = expectResultBodyFun,
        classpath = scalaTestClasspath
      ),
/*
      Style(
        name = "specWithAssertions",
        shortName = "specWithAssertions",
        importNames = Array("org.scalatest.Assertions"),
        classAnnotations = Array.empty,
        extendsName = Some("Assertions"),
        mixinNames = Array.empty,
        scopeBracket = false,
        scopeDef = "",
        testDefFun = specTestDefFun,
        testBodyFun = expectResultBodyFun,
        classpath = scalaTestClasspath
      ),
*/
      Style(
        name = "specWithAssertions",
        shortName = "specWithAssertions",
        importNames = Array("org.scalatest.Assertions._"),
        classAnnotations = Array.empty,
        extendsName = None,
        mixinNames = Array.empty,
        scopeBracket = false,
        scopeDef = "",
        testDefFun = specTestDefFun,
        testBodyFun = expectResultBodyFun,
        classpath = scalaTestClasspath
      )
    )

  val testCounts = Array(100) 
  
  val headers = "TestCount," + testCounts.mkString(",") + "\n"
  durationFile.write(headers)
  fileCountFile.write(headers)
  fileSizeFile.write(headers)

  styles.foreach { style => 
    durationFile.write(style.name)
    durationFile.flush()
    fileCountFile.write(style.name)
    fileCountFile.flush()
    fileSizeFile.write(style.name)
    fileSizeFile.flush()
    try {
      testCounts.foreach { testCount =>
        println("Working on " + style.name + " test count " + testCount + "...")
        val outputDir = getOutputDir(baseOutputDir, testCount)
        val generatedDir = new File(baseGeneratedDir, "generated-" + testCount)

        val timingFile: String = baseTimingDir + style.extendsName.getOrElse("") + style.shortName + ".out"
println("timingFile = " + timingFile)

        val generatedSrc = generateSourceFile(
                             testCount, 
                             new File(generatedDir, style.extendsName.getOrElse("") + style.shortName), // target dir
                             style.extendsName.getOrElse("") + style.shortName, // package name
                             style.importNames, // imports
                             style.classAnnotations, // annotations
                             style.extendsName, // extends
                             style.mixinNames, // mixin
                             style.scopeBracket, // scope requires bracket or not
                             style.scopeDef, // scope definition
                             style.testDefFun, 
                             style.testBodyFun)
        val duration = compile(generatedSrc.getAbsolutePath, style.classpath, outputDir.getAbsolutePath, timingFile)
        durationFile.write("," + duration)
        durationFile.flush()

        val (fileCount, fileSize) = getFileAndByteCount(new File(outputDir, style.extendsName.getOrElse("") + style.shortName))
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

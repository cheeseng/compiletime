import java.net._
import java.io._
import java.nio.channels.Channels
import scala.annotation.tailrec
import scala.math.pow

val scalaHome = {
  val home = scala.util.Properties.scalaHome
  println("Scala Home: " + home)
  home
}

val scalaVersion = {
  val rawVersion = scala.util.Properties.scalaPropOrElse("version.number", "unknown")
  println("Detected Scala version: " + rawVersion)
  val versionParts = rawVersion.split("\\.")
  versionParts(0).toInt + "." + versionParts(1).toInt
}
val scalaTest2Version = "2.3.0-SNAP3"
val scalaTest3Version = "3.0.0-SNAP5"

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

def generateSourceFile(testCount: Int, targetDir: File, fileNumber: Int, packageName: String, importStatements: Array[String], 
                       extendsName: String, withNames: Array[String], scopeBracket: Boolean, scopeDef: String, 
                       testDefFun: (Int) => String, testBodyFun: (Int) => String): File = {
  targetDir.mkdirs()
  val targetFile = new File(targetDir, "ExampleSpec" + fileNumber + ".scala")
  val targetOut = new BufferedWriter(new FileWriter(targetFile))
  try {
    targetOut.write("package " + packageName + "\n\n")
    importStatements.foreach { s =>
      targetOut.write("import " + s + "\n")
    }
    targetOut.write("\n")
    targetOut.write("class ExampleSpec" + fileNumber + " extends " + extendsName + " " + withNames.map(n => " with " + n).mkString(" ") + " {\n")
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

def generateMultipleSourceFiles(testCount: Int, maxTestCount:Int, targetDir: File, packageName: String, importStatements: Array[String], 
                       extendsName: String, withNames: Array[String], scopeBracket: Boolean, scopeDef: String, 
                       testDefFun: (Int) => String, testBodyFun: (Int) => String): List[String] = {
  targetDir.mkdirs()
  val totalFiles = testCount / maxTestCount
  val remainder = testCount % maxTestCount

  val sourceFiles = 
    if (totalFiles == 0)
      List(generateSourceFile(testCount, targetDir, 0, packageName, importStatements, 
                              extendsName, withNames, scopeBracket, scopeDef, testDefFun, testBodyFun).getAbsolutePath)
    else
      for (fileNumber <- 1 to totalFiles) yield
        generateSourceFile(maxTestCount, targetDir, fileNumber, packageName, importStatements, 
                           extendsName, withNames, scopeBracket, scopeDef, testDefFun, testBodyFun).getAbsolutePath

  // For remainder
  if (remainder > 0)
    List(generateSourceFile(remainder, targetDir, totalFiles + 1, packageName, importStatements, 
                            extendsName, withNames, scopeBracket, scopeDef, testDefFun, testBodyFun).getAbsolutePath) ++ sourceFiles
  else
    sourceFiles.toList
}

// Using assert(==)
def assert2TestBodyFun(x: Int): String = "assert(" + x + " + 1 == " + (x+1) + ")"
// Using assert(===)
def assert3TestBodyFun(x: Int): String = "assert(" + x + " + 1 === " + (x+1) + ")"
// Using should equal matchers
def shouldEqualTestBodyFun(x: Int): String = x + " + 1 should equal (" + (x+1) + ")"
// Using should be matchers
def shouldBeTestBodyFun(x: Int): String = x + " + 1 should be (" + (x+1) + ")"
// Using should be matchers
def shouldContainTestBodyFun(x: Int): String = "List(" + x + " + 1) should contain (" + (x+1) + ")"
// Using should equal matchers with logical and
def shouldLogicalAndEqualTestBodyFun(x: Int): String = x + " + 1 should (not equal " + x + " and not equal " + (x-1) + ")"
// Using should be matchers with logical and
def shouldLogicalAndBeTestBodyFun(x: Int): String = x + " + 1 should (not be " + x + " and not be " + (x-1) + ")"
// Using should contain matchers with logical and
def shouldLogicalAndContainTestBodyFun(x: Int): String = "List(" + x + " + 1) should (not contain " + x + " and not contain " + (x-1) + ")"

// Spec 
def specTestDefFun(x: Int): String = "def `increment " + x + "`"
// WordSpec
def wordSpecTestDefFun(x: Int): String = "\"increment " + x + "\" in"
//FunSuite
def funSuiteTestDefFun(x: Int): String = "test(\"increment " + x + "\")"
//FunSpec
def funSpecTestDefFun(x: Int): String = "it(\"increment " + x + "\")"
//FreeSpec
def freeSpecTestDefFun(x: Int): String = "\"increment " + x + "\" in"
//FlatSpec
def flatSpecTestDefFun(x: Int): String = "it should \"increment " + x + "\" in"

def compile(srcFiles: List[String], classpath: String, targetDir: String) = {
  import scala.collection.JavaConversions._
  
  val command = List(scalaHome + "/bin/scalac", "-classpath", classpath, "-d", targetDir) ::: srcFiles
  val builder = new ProcessBuilder(command)
  builder.redirectErrorStream(true)
  val start = System.currentTimeMillis
  val process = builder.start()

  val stdout = new BufferedReader(new InputStreamReader(process.getInputStream))

  var line = "Compiling " + srcFiles.mkString(", ") + "..."
  
  while (line != null) {
    println (line)
    line = stdout.readLine
  }
  
  val end = System.currentTimeMillis
  end - start
}

def compileWithSuperSafe(srcFiles: List[String], classpath: String, targetDir: String) = {
  import scala.collection.JavaConversions._
  
  val command = List(scalaHome + "/bin/scalac", "-Xplugin:supersafe_2.11-1.1.0-SNAPSHOT.jar", "-classpath", classpath, "-d", targetDir) ::: srcFiles
  val builder = new ProcessBuilder(command)
  builder.redirectErrorStream(true)
  val start = System.currentTimeMillis
  val process = builder.start()

  val stdout = new BufferedReader(new InputStreamReader(process.getInputStream))

  var line = "Compiling " + srcFiles.mkString(", ") + "..."
  
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

case class Style(name: String, className: String, scopeBracket: Boolean, scopeDef: String, testDefFun: (Int) => String)
case class TestType(name: String, shortName: String, importNames: Array[String], mixinNames: Array[String], testBodyFun: (Int) => String)

if (scalaVersion != "unknown") {
  val scalatest2Jar = new File("scalatest-all_" + scalaVersion + "-" + scalaTest2Version + ".jar")
  if (!scalatest2Jar.exists)
    downloadFile("https://oss.sonatype.org/content/repositories/releases/org/scalatest/scalatest-all_" + scalaVersion + "/" + scalaTest2Version + "/scalatest-all_" + scalaVersion + "-" + scalaTest2Version + ".jar", scalatest2Jar)

  val scalatest3Jar = new File("scalatest-all_" + scalaVersion + "-" + scalaTest3Version + ".jar")
  if (!scalatest3Jar.exists)
    downloadFile("https://oss.sonatype.org/content/repositories/releases/org/scalatest/scalatest-all_" + scalaVersion + "/" + scalaTest3Version + "/scalatest-all_" + scalaVersion + "-" + scalaTest3Version + ".jar", scalatest3Jar)

  val baseDir = new File("target/" + scalaVersion + "/supersafe")
  if (baseDir.exists)
    deleteDir(baseDir)
    
  val statDir = new File(baseDir, "stat")
  statDir.mkdirs()

  val durationFile = new FileWriter(new File(statDir, "duration.csv"))
  val fileCountFile = new FileWriter(new File(statDir, "filecount.csv"))
  val fileSizeFile = new FileWriter(new File(statDir, "filesize.csv"))

  val scalaTest2Classpath = scalatest2Jar.getName
  val scalaTest3Classpath = scalatest3Jar.getName

  val baseOutputDir = new File(baseDir, "output")
  baseOutputDir.mkdirs()

  val baseGeneratedDir = new File(baseDir, "generated")
  baseGeneratedDir.mkdirs()

  val scalatest2Styles = 
    Array( 
      Style("scalatest2", "WordSpec", true, "\"Scala\" can ", wordSpecTestDefFun)
    )

  val scalatest2TestTypes: Array[TestType] = 
    Array(
      TestType("equal", "equals", Array("org.scalatest._", "Matchers._"), Array.empty, shouldEqualTestBodyFun)
    )

  val supersafeStyles = 
    Array( 
      Style("supersafe", "WordSpec", true, "\"Scala\" can ", wordSpecTestDefFun)
    )

  val supersafeTestTypes: Array[TestType] = 
    Array(
      TestType("equal", "equals", Array("org.scalatest._", "Matchers._"), Array.empty, shouldEqualTestBodyFun)
    )

  val testCounts = 
    Array(
        0,
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

  scalatest2Styles.foreach { style =>
    scalatest2TestTypes.foreach { testType => 
      try {
        durationFile.write(style.name + "-" + testType.shortName) // Don't write with MustMatchers to get all 4 names to fit on graph
        durationFile.flush()
        fileCountFile.write(style.name + "-" + testType.shortName)
        fileCountFile.flush()
        fileSizeFile.write(style.name + "-" + testType.shortName)
        fileSizeFile.flush()
        testCounts.foreach { testCount =>
          println("Working on " + style.className +" " + testType.name + " test count " + testCount + "...")
          val outputDir = getOutputDir(baseOutputDir, testCount)
          val generatedDir = new File(baseGeneratedDir, "generated-" + testCount)

          val generatedSrc = generateMultipleSourceFiles(
                                 testCount, 
                                 10, // maximum number of tests in a file
                                 new File(generatedDir, "ScalaTest2" + style.className), // target dir
                                 style.className + testType.shortName + "2", // package name
                                 testType.importNames, // imports
                                 style.className, // extends
                                 testType.mixinNames, // mixin
                                 style.scopeBracket, // scope requires bracket or not
                                 style.scopeDef, // scope definition
                                 style.testDefFun, 
                                 testType.testBodyFun)


          val duration = compile(generatedSrc, scalaTest2Classpath, outputDir.getAbsolutePath)
          durationFile.write("," + duration)
          durationFile.flush()

          val (fileCount, fileSize) = getFileAndByteCount(new File(outputDir, style.className + testType.shortName + "2"))
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
  }

  supersafeStyles.foreach { style =>
    supersafeTestTypes.foreach { testType => 
      try {
        durationFile.write(style.name + "-" + testType.shortName) // Don't write with MustMatchers to get all 4 names to fit on graph
        durationFile.flush()
        fileCountFile.write(style.name + "-" + testType.shortName)
        fileCountFile.flush()
        fileSizeFile.write(style.name + "-" + testType.shortName)
        fileSizeFile.flush()
        testCounts.foreach { testCount =>
          println("Working on " + style.className +" " + testType.name + " test count " + testCount + "...")
          val outputDir = getOutputDir(baseOutputDir, testCount)
          val generatedDir = new File(baseGeneratedDir, "generated-" + testCount)

          val generatedSrc = generateMultipleSourceFiles(
                                 testCount, 
                                 10, // maximum number of tests in a file
                                 new File(generatedDir, "SuperSafe" + style.className), // target dir
                                 style.className + testType.shortName + "3", // package name
                                 testType.importNames, // imports
                                 style.className, // extends
                                 testType.mixinNames, // mixin
                                 style.scopeBracket, // scope requires bracket or not
                                 style.scopeDef, // scope definition
                                 style.testDefFun, 
                                 testType.testBodyFun)


          val duration = compileWithSuperSafe(generatedSrc, scalaTest2Classpath, outputDir.getAbsolutePath)
          durationFile.write("," + duration)
          durationFile.flush()

          val (fileCount, fileSize) = getFileAndByteCount(new File(outputDir, style.className + testType.shortName + "3"))
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

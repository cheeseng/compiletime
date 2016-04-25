set -x
JAVA_OPTS="-server -Xmx1024M -Xms128M"
export JAVA_OPTS
$SCALA_210/bin/scala tenTestsPerFile.scala
$SCALA_211/bin/scala tenTestsPerFile.scala
JAVA_OPTS="-server -Xmx2048M -Xms256M"
export JAVA_OPTS
$SCALA_210/bin/scala allTestsInOneFile.scala
$SCALA_211/bin/scala allTestsInOneFile.scala
$SCALA_210/bin/scala testsIn100Files.scala
$SCALA_211/bin/scala testsIn100Files.scala
$SCALA_210/bin/scala dataTables.scala
$SCALA_211/bin/scala dataTables.scala
$SCALA_210/bin/scala allMethodTestsInOneFile.scala
$SCALA_211/bin/scala allMethodTestsInOneFile.scala
$SCALA_210/bin/scala assertTestsInOneFile.scala
$SCALA_211/bin/scala assertTestsInOneFile.scala
$SCALA_210/bin/scala allClassTestsInOneFile.scala
$SCALA_211/bin/scala allClassTestsInOneFile.scala
$SCALA_210/bin/scala scalautilsScalaz.scala
$SCALA_211/bin/scala scalautilsScalaz.scala
$SCALA_210/bin/scala shapelessTables.scala
$SCALA_211/bin/scala shapelessTables.scala
$SCALA_210/bin/scala assertBeforeAfter.scala
$SCALA_211/bin/scala assertBeforeAfter.scala
$SCALA_210/bin/scala matcherBeforeAfter.scala
$SCALA_211/bin/scala matcherBeforeAfter.scala
$SCALA_210/bin/scala google-chart.scala

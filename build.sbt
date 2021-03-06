
// Eclipse plugin
EclipseKeys.useProjectId := true
EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.ManagedClasses
EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE18)
EclipseKeys.withSource := true
EclipseKeys.withJavadoc := true
EclipseKeys.withBundledScalaContainers := false
EclipseKeys.eclipseOutput := Some(".target")

name := "dl4jtest"

version := "0.1.1"

scalaVersion in ThisBuild := "2.12.1"
//scalaVersion := "2.12.0"
//scalaVersion := "2.11.8"

// For For ScalaTestb: add in ~/.sbt/0.13/global.sbt
// export http_proxy="http://proxy.inescn.pt:3128"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

//resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
//resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
//resolvers += "mvnrepository" at "http://mvnrepository.com/artifact/"
//resolvers += "Maven Central" at "http://repo1.maven.org/maven2/"

//classpathTypes += "maven-plugin"

//libraryDependencies += "org.scala-lang" % "scala-library" % "2.12.0"

// http://alvinalexander.com/scala/how-to-use-java-style-logging-slf4j-scala
// https://github.com/typesafehub/scala-logging
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.23"
//libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.1"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

// Date and Time
// http://www.time4j.net/tutorial/appendix.html
libraryDependencies += "org.threeten" % "threeten-extra" % "1.0"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.1"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.5.1"
libraryDependencies += "org.json4s" %% "json4s-ext" % "3.5.1"


// Type-level
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

// Linear Algebra
// http://statr.me/2015/09/an-overview-of-linear-algebra-libraries-in-scala-java/
// http://ejml.org/wiki/index.php?title=Main_Page
// http://commons.apache.org/proper/commons-math/index.html
libraryDependencies += "net.sourceforge.f2j" % "arpack_combined_all" % "0.1"
libraryDependencies += "com.github.fommil.netlib" % "all" % "1.1.2" pomOnly()
libraryDependencies += "com.googlecode.matrix-toolkits-java" % "mtj" % "1.0.4"
    
// SCala I/O
libraryDependencies += "com.github.pathikrit" %% "better-files" % "2.17.1"

// CSV
// Core library, included automatically if any other module is imported.
libraryDependencies += "com.nrinaudo" %% "kantan.csv" % "0.1.18"
// Java 8 date and time instances.
libraryDependencies += "com.nrinaudo" %% "kantan.csv-java8" % "0.1.18"
// Automatic type class instances derivation.
libraryDependencies += "com.nrinaudo" %% "kantan.csv-generic" % "0.1.18"
// jackson-csv engine.
libraryDependencies += "com.nrinaudo" %% "kantan.csv-jackson" % "0.1.18"

    
// ML
libraryDependencies += "com.github.haifengl" % "smile-core" % "1.2.2"
libraryDependencies += "com.github.haifengl" %% "smile-scala" % "1.2.2"
//libraryDependencies += "com.github.lwhite1" % "tablesaw" % "0.7.6.4"
//libraryDependencies += "com.github.lwhite1" % "tablesaw" % "0.7.6.9"
libraryDependencies += "com.github.lwhite1" % "tablesaw" % "0.7.7.1"

// Apache Common
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1" withSources() withJavadoc()

libraryDependencies += "org.knowm.xchart" % "xchart" % "3.2.2"

// http://bytedeco.org/
// TODO
libraryDependencies += "org.bytedeco" % "javacpp" % "1.2.4"
//libraryDependencies += "org.bytedeco" % "javacpp" % "1.3.1"

libraryDependencies += "org.nd4j"     % "canova-api" % "0.0.0.17"
libraryDependencies += "org.nd4j"     % "canova-nd4j-image" % "0.0.0.17"


//lazy val dl4jVersion = "0.7.2"
lazy val dl4jVersion = "0.8.0"

//lazy val nd4jVersion = "0.7.2"
lazy val nd4jVersion = "0.8.0"


// fails libraryDependencies += "org.nd4j" % "nd4j-native-platform" % "0.6.0" classifier "" classifier "linux-x86_64"
//libraryDependencies += "org.nd4j"   % "nd4j-native-platform" % nd4jVersion
//libraryDependencies += "org.nd4j" % "nd4j-cuda-7.5-platform" % nd4jVersion
libraryDependencies += "org.nd4j" % "nd4j-cuda-8.0-platform" % nd4jVersion

// TODO
// https://github.com/deeplearning4j/nd4s
// we want https://github.com/deeplearning4j/nd4s/tree/nd4s-0.6.0
// https://github.com/deeplearning4j/nd4s/issues/82
// git clone https://github.com/deeplearning4j/nd4s.git
// cd nd4s/
// git tag -l
// git checkout tags/nd4s-0.6.0
// or git checkout tags/<tag_name> -b <branch_name>
// sbt
// set scalaVersion := "2.12.0"
// test:console
// (1 to 9).asNDArray(3,3)
//libraryDependencies += "org.nd4j" % "nd4s_2.12.0-M3" % "0.4-rc3.8" 
// https://github.com/deeplearning4j/nd4s/issues/96
//libraryDependencies += "org.nd4j" %% "nd4s" % "0.7.0" 
//libraryDependencies += "org.nd4j" %% "nd4s" % "0.6.0"
// https://github.com/deeplearning4j/nd4s/issues/96#issuecomment-260345356
// https://mvnrepository.com/artifact/org.nd4j/nd4s_2.11
//libraryDependencies += "org.nd4j" %% "nd4s" % nd4jVersion


libraryDependencies += "org.datavec" % "datavec-api" % "0.7.2"
libraryDependencies += "org.datavec" % "datavec-data-image" % "0.7.2"
libraryDependencies += "org.datavec" % "datavec-data-codec" % "0.7.2"
libraryDependencies += "org.datavec" % "datavec-data-audio" % "0.7.2"
//libraryDependencies += "org.datavec" % "datavec-nd4j-common" % "0.7.2" already loaded

libraryDependencies += "org.deeplearning4j" % "deeplearning4j-core" % "0.7.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "junit" % "junit" % "4.4"


// TODO
// https://scala-blitz.github.io/
// https://bitbucket.org/oscarlib/oscar/wiki/Home
// https://github.com/bruneli/scalaopt
// https://github.com/scalanlp/breeze
// https://github.com/vagmcs/Optimus


// TODO: add scalastyle checks also
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint", "-explaintypes")
//scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint")
//scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint:deprecation")
//scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint", "-Xfatal-warnings")
//scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
//scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint", "-uniqid", "-explaintypes")


//
//SBT offers 3 ways to pass JVM parameters:
//
//    Set environment variable SBT_OPTS
//    Set environment variable JAVA_OPTS
//    Pass parameters by command line option `-J-X`
//
// export JAVA_OPTS="-Xmx512m" sbt run
// JAVA_OPTS= -Dhttp.proxyHost=http://proxy2.inescn.pt -Dhttp.proxyPort=3128
// Use -J-X options to sbt for individual options, e.g. -J-Xmx2048 -J-XX:MaxPermSize=512
// This must be true for the line below to work
//fork in run := true
//
//javaOptions in run += "-ea"

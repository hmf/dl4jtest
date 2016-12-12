
// Eclipse plugin
EclipseKeys.useProjectId := true
EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.ManagedClasses
EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE18)
EclipseKeys.withSource := true
EclipseKeys.withJavadoc := true
EclipseKeys.withBundledScalaContainers := false
EclipseKeys.eclipseOutput := Some(".target")

name := "dl4jtest"

version := "0.1"

scalaVersion in ThisBuild := "2.12.1"
//scalaVersion := "2.12.0"
//scalaVersion := "2.11.8"

//resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
//resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
//resolvers += "mvnrepository" at "http://mvnrepository.com/artifact/"
//resolvers += "Maven Central" at "http://repo1.maven.org/maven2/"

//classpathTypes += "maven-plugin"

//libraryDependencies += "org.scala-lang" % "scala-library" % "2.12.0"

// http://alvinalexander.com/scala/how-to-use-java-style-logging-slf4j-scala
// https://github.com/typesafehub/scala-logging
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.21"
//libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

// Apache Common
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

libraryDependencies += "org.knowm.xchart" % "xchart" % "3.2.1"

// http://bytedeco.org/
libraryDependencies += "org.bytedeco" % "javacpp" % "1.2.4"
libraryDependencies += "org.nd4j"     % "canova-api" % "0.0.0.17"
libraryDependencies += "org.nd4j"     % "canova-nd4j-image" % "0.0.0.17"

// fails libraryDependencies += "org.nd4j" % "nd4j-native-platform" % "0.6.0" classifier "" classifier "linux-x86_64"
//libraryDependencies += "org.nd4j"   % "nd4j-native-platform" % "0.6.0"
//libraryDependencies += "org.nd4j" % "nd4j-cuda-7.5-platform" % "0.6.0"
libraryDependencies += "org.nd4j" % "nd4j-cuda-8.0-platform" % "0.6.0"
//libraryDependencies += "org.nd4j" % "nd4j-jblas" % "0.4-rc3.6"         not available
//libraryDependencies += "org.nd4j" % "nd4j-netlib-blas" % "0.4-rc3.6"   not available

// TODO
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
libraryDependencies += "org.nd4j" % "nd4s_2.12.0-M3" % "0.4-rc3.8" 
//libraryDependencies += "org.nd4j" %% "nd4s" % "0.6.0"

libraryDependencies += "org.datavec" % "datavec-api" % "0.6.0"
libraryDependencies += "org.datavec" % "datavec-data-image" % "0.6.0"
libraryDependencies += "org.datavec" % "datavec-data-codec" % "0.6.0"
libraryDependencies += "org.datavec" % "datavec-data-audio" % "0.6.0"
//libraryDependencies += "org.datavec" % "datavec-nd4j-common" % "0.6.0" already loaded

libraryDependencies += "org.deeplearning4j" % "deeplearning4j-core" % "0.6.0"


// TODO: add scalastyle checks also
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint")
//scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint:deprecation")
//scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint", "-Xfatal-warnings")
//scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")


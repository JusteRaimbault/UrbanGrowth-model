scalaVersion := "2.13.1"

name := "urbangrowth"

version := "0.1-SNAPSHOT"

//mainClass in (Compile, run) := Some("urbangrowth.Run")
mainClass in (Compile, run) := Some("urbangrowth.test.Test")

enablePlugins(SbtOsgi)
OsgiKeys.exportPackage := Seq("urbangrowth.*")
OsgiKeys.importPackage := Seq("*;resolution:=optional")
OsgiKeys.privatePackage := Seq("!scala.*,*")
OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""


resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("staging"),
  Resolver.mavenCentral,
  "osgeo" at "https://repo.osgeo.org/repository/geotools-releases",
  "geosolutions" at "https://maven.geo-solutions.it",
  "geotoolkit" at "https://maven.geotoolkit.org"
)


val geotoolsVersion = "21.0"

libraryDependencies ++= Seq(
  "gov.nist.math" % "jama" % "1.0.3",
  "org.openmole.library" %% "spatialdata" % "0.4-SNAPSHOT",
  "org.geotools" % "geotools" % geotoolsVersion exclude("javax.media", "jai_core") exclude("com.vividsolutions", "jts-core"),
  "org.geotools" % "gt-shapefile" % geotoolsVersion exclude("javax.media", "jai_core") exclude("com.vividsolutions", "jts-core")
)

//libraryDependencies += "org.geotools" % "gt-referencing" % "9.3"
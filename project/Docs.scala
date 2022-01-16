import com.lightbend.paradox.sbt.ParadoxPlugin
import com.lightbend.paradox.sbt.ParadoxPlugin.autoImport.{builtinParadoxTheme, paradox, paradoxMarkdownToHtml, paradoxProperties, paradoxTheme}
import com.typesafe.sbt.git.ConsoleGitRunner
import sbt.Keys._
import sbt._

import java.nio.file.Files


object Docs extends AutoPlugin {
  object autoImport {
    val buildCapabilitiesTable = taskKey[File]("Build the capabilities.csv table for the documentation")

    val preprocessDocs = taskKey[File]("Prepare the documentation directory for Paradox")

    val checkScaladocLinks = taskKey[Unit]("Prepare the documentation directory for Paradox")

    val scaladocDirs = taskKey[Seq[(String, File)]]("Scaladoc directories to include with documentation")

    val deployDocs = taskKey[Unit]("Deploy docs to GitHub Pages")
  }

  import autoImport._


  override def requires = ParadoxPlugin

  def versionTag(v: String) = "v" + v // get the tag for a version

  override def projectSettings = Seq(
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    Compile / paradoxProperties ++= {
      val shortVersion = version.value.replaceFirst("""(\d+\.\d+)\.\d+.*""", """$1.x""")
      Map(
        "scaladoc.scala.base_url" -> s"https://www.scala-lang.org/api/${scalaVersion.value}",
        "scaladoc.slick.base_url" -> s"https://scala-slick.org/doc/${version.value}/api",
        "scaladoc.slick.codegen.base_url" -> "https://scala-slick.org/doc/3.4.0-M1/codegen-api",
        "scaladoc.slick.jdbc.hikaricp.base_url" -> "https://scala-slick.org/doc/3.4.0-M1/hikaricp-api",
        "scaladoc.com.typesafe.slick.testkit.base_url" -> "https://scala-slick.org/doc/3.4.0-M1/testkit-api",
        "javadoc.javax.sql.base_url" -> "https://docs.oracle.com/javase/8/docs/api/",
        "github.base_url" -> (scmInfo.value.get.browseUrl.toString + "/blob/main"),
        "extref.SI.base_url" -> "https://issues.scala-lang.org/browse/SI-%s",
        "extref.about-pool-sizing.base_url" -> "https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing",
        "extref.activator.base_url" -> "https://typesafe.com/activator",
        "extref.akka-sphinx.base_url" -> "http://doc.akka.io/docs/akka/2.4.0/dev/documentation.html",
        "extref.akka-streams.base_url" -> "https://akka.io/docs/",
        "extref.akka.base_url" -> "https://akka.io/",
        "extref.db2.base_url" -> "https://www.ibm.com/analytics/db2",
        "extref.derby.base_url" -> "https://db.apache.org/derby/",
        "extref.h2.base_url" -> "http://www.h2database.com/",
        "extref.hikaricp-monitoring.base_url" ->
          "https://github.com/b rettwooldridge/HikariCP/wiki/MBean-(JMX)-Monitoring-and-Management",
        "extref.hikaricp.base_url" -> "https://github.com/brettwooldridge/HikariCP",
        "extref.hsqldb.base_url" -> "http://hsqldb.org/",
        "extref.javaapi.base_url" -> "https://docs.oracle.com/javase/7/docs/api/%s.html",
        "extref.javadb.base_url" -> "https://www.oracle.com/java/technologies/javadb.html",
        "extref.jdbc.base_url" -> "https://en.wikipedia.org/wiki/Java_Database_Connectivity",
        "extref.jmx.base_url" -> "https://en.wikipedia.org/wiki/Java_Management_Extensions",
        "extref.jpa.base_url" -> "https://en.wikipedia.org/wiki/Java_Persistence_API",
        "extref.lightbend.base_url" -> "https://www.lightbend.com/",
        "extref.logback.base_url" -> "https://logback.qos.ch/",
        "extref.mysql.base_url" -> "https://www.mysql.com/",
        "extref.oracle.base_url" -> "https://www.oracle.com/database/",
        "extref.play.base_url" -> "https://www.playframework.com/",
        "extref.postgresql.base_url" -> "https://www.postgresql.org/",
        "extref.reactive-manifesto.base_url" -> "https://www.reactivemanifesto.org/",
        "extref.reactive-streams.base_url" -> "http://www.reactive-streams.org/",
        "extref.samplerepo.base_url" -> s"https://github.com/slick/slick/tree/$shortVersion/samples/%s",
        "extref.samplezip.base_url" -> s"https://example.lightbend.com/v1/download/%s-$shortVersion",
        "extref.sbt.base_url" -> "https://www.scala-sbt.org/",
        "extref.scala-futures.base_url" -> "https://docs.scala-lang.org/overviews/core/futures.html",
        "extref.scalaquery.base_url" -> "http://scalaquery.org",
        "extref.slf4j.base_url" -> "https://www.slf4j.org/",
        "extref.slick-manuals.base_url" -> "https://scala-slick.org/docs/",
        "extref.slick.base_url" -> s"https://github.com/slick/slick/blob/${versionTag(version.value)}",
        "extref.sql-server.base_url" -> "https://www.microsoft.com/en-us/sql-server",
        "extref.sqlite.base_url" -> "https://www.sqlite.org/index.html",
        "extref.typesafe-config.base_url" -> "https://github.com/lightbend/config",
        "extref.wikipedia.base_url" -> "https://en.wikipedia.org/wiki/"
      )
    },
    sourceDirectory := baseDirectory.value / "paradox",
    Compile / paradoxTheme / sourceDirectory := baseDirectory.value / "template",
    preprocessDocs / target := target.value / "preprocessed",
    watchSources += sourceDirectory.value,
    watchSources := watchSources.value.filterNot(_.base == (preprocessDocs / target).value),
    preprocessDocs := {
      val out = (preprocessDocs / target).value
      val log = streams.value.log

      IO.copyDirectory(sourceDirectory.value, out)
      IO.copyDirectory(baseDirectory.value / "code", target.value / "code")

      for ((name, dir) <- scaladocDirs.value) {
        val dest = out / name
        log.info(s"Copying $dir to $dest")
        IO.copyDirectory(dir, dest, overwrite = true, preserveLastModified = true)

        def fixGeneratedSourceLink(s: String) =
          s.replaceAll(
            "(https://github.com/slick/slick/blob/[^\"]*)/" +
              "(Users|home)/" +
              "[^\"]*/slick/target/scala-[^\"]*/src_managed/main/" +
              "([^\"]*)\\.scala",
            """$1/scala/$3.fm"""
          )

        (dest ** "*.html").get().foreach { file =>
          IO.writeLines(file, IO.readLines(file).map(fixGeneratedSourceLink))
        }
      }

      out
    },
    Compile / paradox / unmanagedSourceDirectories := Seq((preprocessDocs / target).value),
    Compile / paradox := (Compile / paradox).dependsOn(preprocessDocs).value,
    (Compile / paradoxMarkdownToHtml / excludeFilter) :=
      (Compile / paradoxMarkdownToHtml / excludeFilter).value ||
        globFilter("capabilities.md"),
    checkScaladocLinks := {
      for ((name, dir) <- scaladocDirs.value)
        new ReusableSbtChecker(dir.toString, (Compile / paradox / target).value.toString, name, streams.value.log)
          .run()
    },
    deployDocs := {
      val log = streams.value.log
      val ver = version.value
      val dir = Files.createTempDirectory("slick-docs").toFile
      dir.deleteOnExit()
      val repo = "git@github.com:slick/doc.git"
      log.info(s"Cloning $repo into $dir")
      ConsoleGitRunner("clone", "--branch", "gh-pages", repo, ".")(dir, log)
      val dest = dir / ver
      val existed = dest.exists()
      IO.delete(dest)
      log.info("Copying docs")
      IO.copyDirectory((Compile / paradox).value, dest)
      log.info("Pushing changes")
      ConsoleGitRunner.commitAndPush((if (existed) "Updated" else "Added") + " docs for version " + ver)(dir, log)
    }
  )
}

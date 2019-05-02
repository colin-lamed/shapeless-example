package persistence

import java.sql.Connection


object ConnectionHolder {
  lazy val conn: Connection = {
    Class.forName("org.hsqldb.jdbc.JDBCDriver")
    val conn = java.sql.DriverManager.getConnection("jdbc:hsqldb:mem:myDb", "sa", "sa")
    val source = scala.io.Source.fromFile("./persistence/src/test/resources/create.sql")
    val script = try source.mkString finally source.close()
    println(s"Running $script")
    val ps = conn.prepareStatement(script)
    ps.executeUpdate()
    conn
  }
}
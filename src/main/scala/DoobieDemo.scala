import cats.effect.{ExitCode, IO, IOApp}
import doobie.util.transactor.Transactor
import doobie.implicits._

object DoobieDemo extends IOApp.Simple {
  case class Actor(id: Int, name: String)
  case class Movie(
      id: String,
      title: String,
      year: Int,
      actors: List[String],
      director: String
  )

  implicit class Debugger[A](io: IO[A]) {
    def debug: IO[A] = io.map { a =>
      println(s"[${Thread.currentThread().getName}] $a")
      a
    }
  }

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:myimdb",
    "docker",
    "docker"
  )

  def findAllActorNames: IO[List[String]] = {
    val query = sql"select name from actors".query[String]
    val action = query.to[List]
    action.transact(xa)
  }

  def findActorById(id: Int): IO[Option[Actor]] = {
    val query = sql"select id, name from actors where id = $id".query[Actor]
    val action = query.option
    action.transact(xa)
  }

  lazy val actorNamesStream = sql"select name from actors"
    .query[String]
    .stream
    .compile
    .toList
    .transact(xa)

  def run: IO[Unit] =
    actorNamesStream.debug.void
}

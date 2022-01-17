import cats.effect.{ExitCode, IO, IOApp}
import doobie.util.transactor.Transactor
import doobie.implicits._

object DoobieDemo extends IOApp {
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

  def run(args: List[String]): IO[ExitCode] =
    findAllActorNames.debug.as(ExitCode.Success)
}

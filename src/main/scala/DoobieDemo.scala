import cats.effect.{ExitCode, IO, IOApp}

object DoobieDemo extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    IO(println("hello")).as(ExitCode.Success)
}

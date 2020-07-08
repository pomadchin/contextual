package org.contextual.reader

import cats.FlatMap
import cats.data.ReaderT
import cats.effect.{ExitCode, IO, IOApp}
import tofu.common.Console
import tofu.syntax.monadic._
import tofu.{HasContext, WithRun}

object ReaderTContextApp extends IOApp {

  type ReaderIO[A] = ReaderT[IO, ReaderContext[IO], A]

  type HasDb[F[_], G[_]] = F HasContext DbContext[G]

  def findUser[F[_]: FlatMap, G[_]: FlatMap](id: String)(implicit wr: WithRun[F, G, ReaderContext[G]]): F[String] = {
    wr.ask(_.db.userRepo).flatMap(r => wr.lift(r.find(id)))
  }

  def program[F[_]: FlatMap: Console, G[_]: FlatMap](implicit wr: WithRun[F, G, ReaderContext[G]]): F[Unit] =
    for {
      u <- findUser[F, G]("userId")
      _ <- Console[F].putStrLn(u)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      ctx <- IO.pure(ReaderContext.mkContext[IO])
      _   <- ctx.use(c => WithRun[ReaderIO, IO, ReaderContext[IO]].runContext(program[ReaderIO, IO])(c))
    } yield ExitCode.Success
  }
}

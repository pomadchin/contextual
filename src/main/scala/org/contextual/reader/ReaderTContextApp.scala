package org.contextual.reader

import cats.FlatMap
import cats.data.ReaderT
import cats.effect.{ExitCode, IO, IOApp}
import tofu.common.Console
import tofu.optics.Contains
import tofu.syntax.console._
import tofu.syntax.monadic._
import tofu.{WithLocal, WithRun}

object ReaderTContextApp extends IOApp {

  implicit def subcontext[MainCtx, SubCtx](
    implicit lens: MainCtx Contains SubCtx
  ): WithLocal[ReaderT[IO, MainCtx, *], SubCtx] = WithRun[ReaderT[IO, MainCtx, *], IO, MainCtx].subcontext(lens)

  def findUser[F[_]: FlatMap](id: String)(implicit repo: UserRepo[F]): F[String] =
    repo.find(id)

  def assignWork[F[_]: FlatMap](work: String, userId: String)(implicit repo: WorkRepo[F]): F[String] =
    repo.assign(work, userId)

  def program[F[_]: FlatMap: Console: UserRepo: WorkRepo]: F[Unit] =
    for {
      u <- findUser[F]("userId")
      _ <- putStrLn(u)
      w <- assignWork("read book", "lazy user")
      _ <- putStrLn(w)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    ReaderContext
      .mk[IO]
      .use(program[ReaderIO].run)
      .map(_ => ExitCode.Success)
  }
}

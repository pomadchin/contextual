package org.contextual.reader

import cats.FlatMap
import cats.data.ReaderT
import cats.effect.{ExitCode, IO, IOApp}
import tofu.common.Console
import tofu.lift.Lift
import tofu.optics.Contains
import tofu.syntax.console._
import tofu.syntax.lift._
import tofu.syntax.monadic._
import tofu.{Context, HasContext, WithLocal, WithRun}

object ReaderTContextApp extends IOApp {

  type ReaderIO[A] = ReaderT[IO, ReaderContext[IO], A]

  type HasDb[F[_], G[_]] = F HasContext DbContext[G]

  implicit def subcontext[MainCtx, SubCtx](
    implicit lens: MainCtx Contains SubCtx
  ): WithLocal[ReaderT[IO, MainCtx, *], SubCtx] = WithRun[ReaderT[IO, MainCtx, *], IO, MainCtx].subcontext(lens)

  def findUser[F[_]: FlatMap: HasDb[*[_], G], G[_]: Lift[*[_], F]](id: String): F[String] =
    Context[F].ask(_.userRepo).flatMap(_.find(id).lift)

  def assignWork[F[_]: FlatMap: HasDb[*[_], G], G[_]: Lift[*[_], F]](work: String, userId: String): F[String] =
    Context[F].ask(_.workRepo).flatMap(_.assign(work, userId).lift)

  def program[F[_]: FlatMap: Console: HasDb[*[_], G], G[_]: Lift[*[_], F]]: F[Unit] =
    for {
      u <- findUser[F, G]("userId")
      _ <- putStrLn(u)
      w <- assignWork("read book", "lazy user")
      _ <- putStrLn(w)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      ctx <- IO.pure(ReaderContext.mkContext[IO])
      _   <- ctx.use(c => WithRun[ReaderIO, IO, ReaderContext[IO]].runContext(program[ReaderIO, IO])(c))
    } yield ExitCode.Success
  }
}

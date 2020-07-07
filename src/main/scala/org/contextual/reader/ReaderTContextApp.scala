package org.contextual.reader

import cats.data.ReaderT
import cats.effect.{ExitCode, IO, IOApp}
import cats.{FlatMap, Monad}
import tofu.common.Console
import tofu.optics.Contains
import tofu.optics.macros._
import tofu.syntax.monadic._
import tofu.{Context, HasContext, WithLocal}

object ReaderTContextApp extends IOApp {

  type ReaderIO[A] = ReaderT[IO, Ctx, A]

  type HasDb[F[_]]  = F HasContext DbContext[F]
  type HasCfg[F[_]] = F HasContext ConfigContext[F]

  final case class Ctx(@promote appCtx: ReaderContext[IO])

  type HasAppContext[F[_]] = F HasContext Ctx

  implicit def subcontext[MainCtx, SubCtx](
    implicit lens: MainCtx Contains SubCtx
  ): WithLocal[ReaderT[IO, MainCtx, *], SubCtx] =
    WithLocal[ReaderT[IO, MainCtx, *], MainCtx].subcontext(lens)

  def findUser[F[_]: HasDb: FlatMap](id: String): F[String]                     = Context[F].ask(_.userRepo).flatMap(_.find(id))
  def assignWork[F[_]: HasDb: FlatMap](work: String, userId: String): F[String] = Context[F].ask(_.workRepo) flatMap (_.assign(work, userId))

  def program[F[_]: Console: Monad: HasDb]: F[Unit] =
    for {
      u <- findUser("userId")
      _ <- Console[F].putStrLn(u)
      w <- assignWork("read book", "lazy user")
      _ <- Console[F].putStrLn(w)
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    for {
      ctx <- IO.pure(ReaderContext.mkContext[IO])
      _   <- ctx.use(c => program[ReaderIO].run(Ctx(c)))
    } yield ExitCode.Success
}

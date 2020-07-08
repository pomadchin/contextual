package org.contextual.reader

import cats.tagless.syntax.functorK._
import cats.{Applicative, Defer}
import cats.effect.Resource
import derevo.derive
import tofu.HasLocal
import tofu.concurrent.ContextT
import tofu.concurrent.impl.ContextTRunContext
import tofu.data.derived.ContextEmbed
import tofu.higherKind.derived.{embed, representableK}
import tofu.optics.Contains
import tofu.optics.macros.{ClassyOptics, promote}

@ClassyOptics
final case class ReaderContext[F[_]](
  @promote db: DbContext[F],
  @promote cfg: ConfigContext[F]
)

@ClassyOptics
final case class DbContext[F[_]](
  userRepo: UserRepo[F],
  workRepo: WorkRepo[F]
)

@derive(embed, representableK)
trait UserRepo[F[_]] {
  def find(id: String): F[String]
}

object UserRepo extends ContextEmbed[UserRepo] {
  def apply[F[_]: Applicative]: UserRepo[F] = (id: String) => Applicative[F].pure(s"Found user with id: $id.")
}

@derive(embed, representableK)
trait WorkRepo[F[_]] {
  def assign(work: String, userId: String): F[String]
}

object WorkRepo extends ContextEmbed[WorkRepo] {

  def apply[F[_]: Applicative]: WorkRepo[F] =
    (work: String, userId: String) => Applicative[F].pure(s"""Work "$work" assigned to user with id: $userId.""")
}

@derive(embed, representableK)
trait ConfigContext[F[_]] {
  def config: F[Cfg]
}

object ConfigContext extends ContextEmbed[ConfigContext] {

  private val cfg = Cfg("localhost", 777)

  def apply[F[_]: Applicative]: ConfigContext[F] = new ConfigContext[F] {
    def config: F[Cfg] = Applicative[F].pure(cfg)
  }
}

final case class Cfg(host: String, port: Int)

object ReaderContext {
  def readerContext[F[+_]: Applicative: Defer]: ContextTRunContext[F, ReaderContext] = ContextT.contextTRunContext[F, ReaderContext]

  implicit def extractContext[F[+_]: Applicative: Defer, A](implicit lens: ReaderContext[ReaderF[F, *]] Contains A): HasLocal[ReaderF[F, *], A] =
    readerContext[F].subcontext(lens)

  def mkContext[F[_]: Applicative]: Resource[F, ReaderContext[F]] = Resource.pure[F, ReaderContext[F]] {
    val dbContext: DbContext[F]      = DbContext(UserRepo.apply, WorkRepo.apply)
    val cfgContext: ConfigContext[F] = ConfigContext.apply
    ReaderContext[F](dbContext, cfgContext)
  }

  def mk[F[+_]: Applicative]: Resource[F, ReaderContext[ReaderF[F, *]]] = Resource.pure[F, ReaderContext[ReaderF[F, *]]] {
    val dbContext = DbContext(
      UserRepo.apply[F].mapK(ContextT.liftF[F, ReaderContext]),
      WorkRepo.apply[F].mapK(ContextT.liftF[F, ReaderContext])
    )
    val cfgContext = ConfigContext.apply[F].mapK(ContextT.liftF[F, ReaderContext])

    ReaderContext(dbContext, cfgContext)
  }
}

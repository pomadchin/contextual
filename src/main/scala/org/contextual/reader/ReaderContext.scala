package org.contextual.reader

import cats.Applicative
import cats.effect.Resource
import tofu.optics.macros.{promote, ClassyOptics}

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

trait UserRepo[F[_]] {
  def find(id: String): F[String]
}

object UserRepo {
  def apply[F[_]: Applicative]: UserRepo[F] = (id: String) => Applicative[F].pure(s"Found user with id: $id.")
}

trait WorkRepo[F[_]] {
  def assign(work: String, userId: String): F[String]
}

object WorkRepo {

  def apply[F[_]: Applicative]: WorkRepo[F] =
    (work: String, userId: String) => Applicative[F].pure(s"""Work "$work" assigned to user with id: $userId.""")
}

trait ConfigContext[F[_]] {
  def config: F[Cfg]
}

object ConfigContext {

  private val cfg = Cfg("localhost", 777)

  def apply[F[_]: Applicative]: ConfigContext[F] = new ConfigContext[F] {
    def config: F[Cfg] = Applicative[F].pure(cfg)
  }
}

final case class Cfg(host: String, port: Int)

object ReaderContext {

  def mkContext[F[_]: Applicative]: Resource[F, ReaderContext[F]] = Resource.pure[F, ReaderContext[F]] {
    val dbContext: DbContext[F]      = DbContext(UserRepo.apply, WorkRepo.apply)
    val cfgContext: ConfigContext[F] = ConfigContext.apply
    ReaderContext[F](dbContext, cfgContext)
  }
}

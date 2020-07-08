package org.contextual

import cats.effect.IO
import tofu.HasContext
import tofu.concurrent.ContextT

package object reader {
  type ReaderIO[+A] = ReaderF[IO, A]
  type ReaderF[F[+_], +A] = ContextT[F, ReaderContext, A]
  type HasDb[F[_], G[_]] = F HasContext DbContext[G]
}

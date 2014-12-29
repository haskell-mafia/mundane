package com.ambiata.mundane.trace

import com.ambiata.mundane.control._
import com.ambiata.mundane.data._

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import java.util.concurrent.atomic.AtomicLong

import scalaz._, Scalaz._, effect._, \&/._

/*
 * A profiling tool supporting section timings and counters.
 *
 * This construction is specifically designed to be carried around in
 * a ReaderT sharing the same 'F'.
 */
case class Profiler[F[_]](
  start: Profiler.Section => F[Unit],
  end: Profiler.Section => F[Unit],
  counter: (Profiler.Counter, Long) => F[Unit],
  done: () => F[Profiler.Profile]
) {
  def bracket[A](section: Profiler.Section)(x: => F[A])(implicit F: Applicative[F]): F[A] =
    (start(section) *> x) <* end(section)

  def wrap[A](section: Profiler.Section)(x: => A)(implicit F: Applicative[F]): F[A] =
    bracket(section)(x.pure[F])

  def tick(c: Profiler.Counter): F[Unit] =
    counter(c, 1)
}

object Profiler {
  case class Section(name: String)
  case class Counter(name: String)
  case class ThreadId(id: Long)
  case class Timestamp(millis: Long)
  case class Tag(tid: ThreadId, context: List[Section], start: Timestamp, end: Timestamp)
  case class Profile(sections: List[Tag], counters: Map[Counter, Long])

  def getThreadId: RIO[ThreadId] =
    RIO.io { ThreadId(Thread.currentThread.getId) }

  def getCurrentTime: RIO[Timestamp] =
    RIO.io { Timestamp(System.currentTimeMillis) }

  def empty[F[_]: Applicative]: Profiler[F] =
    Profiler[F](_ => ().pure[F], _ => ().pure[F], (_, _) => ().pure[F], () => Profile(Nil, Map.empty).pure[F])

  def tree: RIO[Profiler[RIO]] = RIO.io {
    case class Start(tid: ThreadId, context: Vector[Section], timestamp: Timestamp)

    val running = new ThreadLocal[Map[Vector[Section], Start]] { override def initialValue = Map.empty }
    val context = new ThreadLocal[Vector[Section]] { override def initialValue = Vector.empty }
    val counters = new ConcurrentHashMap[Counter, AtomicLong]
    val sections = new ConcurrentLinkedQueue[Tag]

    def toProfile: RIO[Profile] = RIO.io {
      import scala.collection.JavaConverters._
      Profile(
        sections.asScala.toList.sortBy(t => (t.tid.id, t.start.millis, t.context.map(_.name).mkString(".")))
      , counters.asScala.map({ case (k, v) => k -> v.get }).toMap
      )
    }

    def start(s: Section): RIO[Unit] = for {
      tid  <- getThreadId
      time <- getCurrentTime
      _    <- RIO.io {
        val ctx = context.get :+ s
        context.set(ctx);
        running.set(running.get + (ctx -> Start(tid, ctx, time)))
      }
    } yield ()

    def end(s: Section): RIO[Unit] = for {
      ctx   <- RIO.io { context.get }
      time  <- getCurrentTime
      start <- RIO.io { running.get.get(ctx) }
      _     <- start match {
        case None =>
          RIO.failIO(s"Profile end without start for key [${ctx.mkString(".")}].")
        case Some(start) if ctx.isEmpty =>
          RIO.failIO(s"Profile end without any start for section [${s}].")
        case Some(start) if ctx.last != s =>
          RIO.failIO(s"Profile end does not match start for key [${ctx.mkString(".")}], with end section [${s}].")
        case Some(start) =>
          RIO.io {
            context.set(ctx.init)
            sections.offer(Tag(start.tid, ctx.toList, start.timestamp, time)) }
          }
    } yield ()

    def count(c: Counter, n: Long): RIO[Unit] =
      RIO.io { Option(counters.get(c)).getOrElse({
        counters.putIfAbsent(c, new AtomicLong(0L))
        counters.get(c)
      }).addAndGet(n) }

    Profiler[RIO](
      s => start(s)
    , s => end(s)
    , (c, n) => count(c, n)
    , () => toProfile
    )
  }
}

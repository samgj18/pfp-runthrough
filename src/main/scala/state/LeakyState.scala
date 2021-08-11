package state
import cats.effect.{IO, _}
import cats.effect.std.{Semaphore, Supervisor}
import cats.effect.unsafe.implicits.global
import state.Regions.randomSleep

import scala.concurrent.duration._

// Let's rewrite the previous program (from Shared State) in such a way
// that our Semaphore isn't wrapped in IO (or any other abstract effect).
object LeakyState extends IOApp.Simple {

  // global access
  lazy val sem: Semaphore[IO] =
    Semaphore[IO](1).unsafeRunSync()

  def doSomethingBad(): IO[Unit] =
    IO.println("DO SOMETHING BAD THAT WE DON'T KNOW")

  def launchMissiles: IO[Unit] =
    sem.permit.surround(doSomethingBad())

  def p1: IO[Unit] =
    sem.permit.surround(IO.println("Running P1")) >> randomSleep

  def p2: IO[Unit] =
    sem.permit.surround(IO.println("Running P2")) >> randomSleep

  // We have lost our Shared State region denoted by flatMap. We don't know what does
  // launchMissiles do internally (like it acquires the single permit and never releases it).
  def run: IO[Unit] =
    Supervisor[IO].use { s =>
      s.supervise(launchMissiles) *>
        s.supervise(p1.foreverM).void *>
        s.supervise(p2.foreverM).void *>
        IO.sleep(5.seconds).void
    }
}

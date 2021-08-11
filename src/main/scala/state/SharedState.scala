package state

import scala.concurrent.duration._
import cats.effect._
import cats.effect.std.{Semaphore, Supervisor}
// To understand shared state, we need to talk about regions of sharing.
// These regions are denoted by a simple flatMap call.
// The example presented next showcases this concept.
object SharedState {}

// In this case the Semaphore is created in the run function, by calling its
// apply function with an argument 1, indicating the number of permits, and
// then calling flatMap to share it with both p1 and p2.

// That enclosing flatMap right after the apply function is what denotes our region
// of sharing.
object Regions extends IOApp.Simple {
  def randomSleep: IO[Unit] =
    IO(scala.util.Random.nextInt(100)).flatMap { ms =>
      IO.sleep((ms + 700).millis)
    }.void

  def p1(sem: Semaphore[IO]): IO[Unit] =
    sem.permit.surround(IO.println("Running P1")) >>
      randomSleep

  def p2(sem: Semaphore[IO]): IO[Unit] =
    sem.permit.surround(IO.println("Running P2")) >>
      randomSleep

  def run: IO[Unit] = {
    Supervisor[IO].use { s =>
      Semaphore[IO](1).flatMap { sem =>
        // We are in control of how we share such data structure within this block.
        s.supervise(p1(sem).foreverM).void *>
          s.supervise(p2(sem).foreverM).void *>
          IO.sleep(5.seconds).void
      }
    }
    // P.S: We make use of Supervisor,
    // which provides a safe way to execute fire-and- forget actions.
  }
}

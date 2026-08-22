package task

import scala.concurrent.duration.DurationInt
import task.Logic.*
import cats.effect.{Async, IO, IOApp, Ref}
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.util.Random

object Main extends IOApp.Simple:

  private val philosopherCount = 5

  def run: IO[Unit] =
    given Logger[IO] = Slf4jLogger.getLogger[IO]
    val (table, initialState) = Logic.createTable(philosopherCount)
    for
      ref <- Ref.of[IO, TableState](initialState)
      _ <- Logger[IO].info("Application started")
      _ <- Logger[IO].info(s"Philosophers count: ${table.philosophers.size}")
      philosopherList = table.philosophers.toList.map { philosopher =>
        runPhilosopherSafely[IO](ref, table, philosopher.id)
      }
        _ <- (printMeals[IO](ref) :: philosopherList).parSequence
    yield ()

  private def runPhilosopherSafely[F[_]: Async](
      ref: Ref[F, TableState],
      table: Table,
      philosopherId: PhilosopherId
  )(using Logger[F]): F[Unit] =
    Logger[F].info(s"Philosopher ${philosopherId.value} thread started") *>
      philosopherLoop(ref, table, philosopherId).handleErrorWith { error =>
        Logger[F].info(
          s"Philosopher ${philosopherId.value} crashed ${error.getMessage}"
        )
      }

  private def philosopherLoop[F[_]: Async](
      ref: Ref[F, TableState],
      table: Table,
      philosopherId: PhilosopherId
  )(using Logger[F]): F[Unit] =
    for
      _ <- logState(ref, s" Philosopher ${philosopherId.value} is thinking")
      _ <- pause(500, 1500)
      startedEating <- tryStartEating(ref, table, philosopherId)
      _ <-
        if startedEating then
          for
            _ <- logState(ref, s"${philosopherId.value} is eating")
            _ <- pause(500, 1500)
            _ <- finishEatingSafely(ref, table, philosopherId)
          yield ()
        else pause(300, 800)
      _ <- philosopherLoop(ref, table, philosopherId)
    yield ()

  def tryStartEating[F[_]: Async](
      ref: Ref[F, TableState],
      table: Table,
      philosopherId: PhilosopherId
  ): F[Boolean] =
    ref.modify { tableState =>
      Logic.tryEat(philosopherId, table, tableState) match {
        case Right(newState) => (newState, true)
        case Left(_)         => (tableState, false)
      }
    }

  def finishEatingSafely[F[_]: Async](
      ref: Ref[F, TableState],
      table: Table,
      philosopherId: PhilosopherId
  ): F[Unit] =
    ref.update { tableState =>
      Logic.finishEating(philosopherId, table, tableState).getOrElse(tableState)
    }

  private def pause[F[_]: Async](minMillis: Int, maxMillis: Int): F[Unit] =
    val delay = minMillis + Random.nextInt(maxMillis - minMillis + 1)
    Async[F].sleep(delay.millis)

  private def logState[F[_]: Async](
      ref: Ref[F, TableState],
      message: String
  )(using Logger[F]): F[Unit] =
    for
      state <- ref.get
      _ <- Logger[F].info(s"$message || $state")
    yield ()

  private def printMeals[F[_]: Async](
      ref: Ref[F, TableState]
  )(using Logger[F]): F[Unit] =
    for
      _ <- Async[F].sleep(5.seconds)
      state <- ref.get
      _ <- Logger[F].info(s" Stats: ${state.meals}")
      _ <- printMeals(ref)
    yield ()

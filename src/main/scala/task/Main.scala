package task

import scala.concurrent.duration.DurationInt
import task.Logic.*
import cats.effect.{Async, IO, IOApp, Ref}
import cats.syntax.all.*
import java.util.logging.Logger
import scala.util.Random

object Main extends IOApp.Simple:

  private val logger: Logger =
    Logger.getLogger("DiningTable")

  private val philosopherCount = 5

  private def log[F[_]: Async](msg: String): F[Unit] =
    Async[F].blocking(logger.info(msg))

  def run: IO[Unit] =
    val (table, initialState) = Logic.createTable(philosopherCount)
    for
      ref <- Ref.of[IO, TableState](initialState)
      _ <- log[IO]("Application started")
      _ <- log[IO](s"Philosophers count: ${table.philosophers.size}")
      philosopherList = table.philosophers.toList.map {philosopher =>
        runPhilosopherSafely[IO](ref, table, philosopher.id)
      }
      _ <- (printMeals[IO](ref) :: philosopherList).parSequence
    yield()


  private def runPhilosopherSafely[F[_]: Async](ref: Ref[F, TableState], table: Table, philosopherId: Int): F[Unit] =
    log(s"Philosopher $philosopherId thread started") *>
      philosopherLoop(ref, table, philosopherId).handleErrorWith{error =>
        log(s"Philosopher $philosopherId crashed ${error.getMessage}")
      }


  private def philosopherLoop[F[_]: Async](ref: Ref[F, TableState], table: Table, philosopherId: Int): F[Unit] =
    for 
      _ <- logState(ref, s" Philosopher $philosopherId is thinking")
      _ <- pause(500, 1500)
      startedEating <- tryStartEating(ref, table, philosopherId)
      _ <- if startedEating then
            for 
              _ <- logState(ref, s"$philosopherId is eating")
              _ <- pause(500, 1500)
              _ <- finishEatingSafely(ref, table, philosopherId)
            yield ()
      else
        pause(300, 800)
      _ <- philosopherLoop(ref, table, philosopherId)
    yield ()
      
      
      
  def tryStartEating[F[_]: Async](ref: Ref[F, TableState], table: Table, philosopherId: Int): F[Boolean] =
    ref.modify{ tableState => 
      Logic.tryEat(philosopherId, table, tableState) match {
        case Right(newState) => (newState, true)
        case Left(_) => (tableState, false)
      }
    }

  def finishEatingSafely[F[_]: Async](ref: Ref[F, TableState], table: Table, philosopherId: Int): F[Unit] =
    ref.update{ tableState => 
      Logic.finishEating(philosopherId, table, tableState).getOrElse(tableState)
    }

  private def pause[F[_]: Async](minMillis: Int, maxMillis: Int): F[Unit] =
    val delay = minMillis + Random.nextInt(maxMillis - minMillis + 1)
    Async[F].sleep(delay.millis)

  private def logState[F[_]: Async](ref: Ref[F, TableState], message: String): F[Unit] =
    for
      state <- ref.get
      _ <- log(s"$message || $state")
    yield()


  private def printMeals[F[_]: Async](ref: Ref[F, TableState]): F[Unit] =
    for
      _ <- Async[F].sleep(5.seconds)
      state <- ref.get
      _ <- log(s" Stats: ${state.meals}")
      _ <- printMeals(ref)
    yield()
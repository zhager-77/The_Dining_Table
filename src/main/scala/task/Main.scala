package task

import task.Logic.*

import java.util.logging.Logger
import scala.util.Random

object Main:

  private val logger: Logger =
    Logger.getLogger("DiningTable")

  private val philosopherCount = 5
  private val tableLock = new Object

  private var table = createTable(philosopherCount)

  def main(args: Array[String]): Unit =
    logger.info("Application started")
    logger.info(s"Created table: $table")
    logger.info(s"Philosophers count: ${table.philosophers.size}")

    table.philosophers.foreach { philosopher =>
      logger.info(s"Starting philosopher: ${philosopher.id}")

      val thread = new Thread(
        new Runnable:
          override def run(): Unit =
            runPhilosopherSafely(philosopher.id),
      s"philosopher-${philosopher.id}"
      )

      thread.start()
    }

    logger.info("All philosopher threads started")

    while true do
      Thread.sleep(10000)

  private def runPhilosopherSafely(philosopherId: Int): Unit =
    try
      logger.info(s"Philosopher $philosopherId thread started")
      philosopherLoop(philosopherId)
    catch
      case error: Throwable =>
        logger.severe(s"Philosopher $philosopherId crashed: ${error.getMessage}")
        error.printStackTrace()

  private def philosopherLoop(philosopherId: Int): Unit =
    while true do
      logState(s"Philosopher $philosopherId is thinking")
      pause(500, 1500)

      val startedEating =
        tryStartEating(philosopherId)

      if startedEating then
        logState(s"Philosopher $philosopherId is eating")
        pause(500, 1500)
        finishEatingSafely(philosopherId)
      else
        pause(300, 800)

  private def tryStartEating(philosopherId: Int): Boolean =
    tableLock.synchronized {
      table = becomeHungry(table, philosopherId).getOrElse(table)

      tryEat(table, philosopherId) match
        case Right(updatedTable) =>
          table = updatedTable
          logState(s"Philosopher $philosopherId started eating")
          true

        case Left(error) =>
          logState(s"Philosopher $philosopherId could not eat: $error")
          false
    }

  private def finishEatingSafely(philosopherId: Int): Unit =
    tableLock.synchronized {
      table = finishEating(table, philosopherId).getOrElse(table)
      logState(s"Philosopher $philosopherId finished eating")
    }

  private def pause(minMillis: Int, maxMillis: Int): Unit =
    val delay = minMillis + Random.nextInt(maxMillis - minMillis + 1)
    Thread.sleep(delay.toLong)

  private def logState(message: String): Unit =
    logger.info(s"$message || $table")
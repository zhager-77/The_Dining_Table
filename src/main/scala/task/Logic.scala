package task

import task.Errors.{PhilosopherNotEating, PhilosopherNotHungry}
import task.PhilosopherState.{Eating, Hungry}

object Logic:
  
  //Создаем стол
  def createTable(size: Int): Table =
    Table(philosophers = Vector.tabulate(size)(id =>
    Philosopher(id, PhilosopherState.Thinking)),
      forks = Vector.tabulate(size)(id =>
      Fork(id, None)))

  
  //Даем нумерацию левой вилке каждого философа 
  def leftForkId(table: Table, philosopherId: Int): Int =
    if philosopherId == 0 then table.forks.size - 1
    else philosopherId - 1

  
  //Даем нумерацию правой вилке каждого философа 
  def rightForkId(philosopherId: Int): Int =
    philosopherId


  //Обновляем состояние философа
  private def updatePhilosopher(table: Table, philosopherId: Int)
                               (f: Philosopher => Philosopher): Either[Errors, Table] =
    if table.philosophers.exists(_.id == philosopherId) then
      Right(table.copy(
        philosophers = table.philosophers.map{philosopher =>
          if philosopher.id == philosopherId then f(philosopher)
          else philosopher
          }
        )
      )
    else
      Left(Errors.PhilosopherNotFound(philosopherId))

  //Делаем философа голодным, меняя состояние на Hungry
  def becomeHungry(table: Table, philosopherId: Int): Either[Errors, Table] =
    updatePhilosopher(table, philosopherId){philosopher =>
      philosopher.copy(state = Hungry)
    }


  //Меняем состояние на Eating
  def tryEat(table: Table, philosopherId: Int): Either[Errors, Table] =
    for
      philosopher <- findPhilosopher(table, philosopherId)
      _ <- ensureHungry(philosopher)
      leftId = leftForkId(table, philosopherId)
      rightId = rightForkId(philosopherId)
      leftFork <- findFork(table, leftId)
      rightFork <- findFork(table, rightId)
      _ <- ensureForkFree(leftFork)
      _ <- ensureForkFree(rightFork)
      tableWithTakenForks = takeForks(table, philosopherId, leftId, rightId)
      tableWithEatingPhilosopher <- updatePhilosopher(tableWithTakenForks, philosopherId){philosopher =>
        philosopher.copy(state = PhilosopherState.Eating)
      }
    yield tableWithEatingPhilosopher

  def findPhilosopher(table: Table, philosopherId: Int): Either[Errors, Philosopher] =
    table.philosophers.find(_.id == philosopherId).toRight(Errors.PhilosopherNotFound(philosopherId))

  def ensureHungry(philosopher: Philosopher): Either[Errors, Unit] =
    if philosopher.state == Hungry then Right(())
    else Left(PhilosopherNotHungry(philosopher.id))

  def findFork(table: Table, forkId: Int): Either[Errors, Fork] =
    table.forks.find(_.id == forkId).toRight(Errors.ForkNotFound(forkId))

  def ensureForkFree(fork: Fork): Either[Errors, Unit] =
    fork.takeBy match {
      case None => Right(())
      case Some(philosopherId) => Left(Errors.ForkTaken(fork.id, philosopherId))
    }

  def takeForks(table: Table, philosopherId: Int, leftForkId: Int, rightForkId: Int): Table =
    table.copy(forks = table.forks.map{fork =>
      if fork.id == leftForkId || fork.id == rightForkId then
        fork.copy(takeBy = Some(philosopherId))
      else
        fork
    })


  def finishEating(table: Table, philosopherId: Int): Either[Errors, Table] =
    for 
      philosopher <- findPhilosopher(table, philosopherId)
      _ <- ensureEating(philosopher)
      leftId = leftForkId(table, philosopherId)
      rightId = rightForkId(philosopherId)
      tableWithReleasedForks = releaseForks(table, leftId, rightId)
      tableWithThinkingPhilosopher <- updatePhilosopher(tableWithReleasedForks, philosopherId) { philosopher =>
        philosopher.copy(state = PhilosopherState.Thinking)
      }
    yield tableWithThinkingPhilosopher


  def ensureEating(philosopher: Philosopher): Either[Errors, Unit] =
    if philosopher.state == Eating then Right(())
    else Left(PhilosopherNotEating(philosopher.id))

  def releaseForks(table: Table, leftForkId: Int, rightForkId: Int): Table =
    table.copy(forks = table.forks.map{fork =>
      if fork.id == leftForkId || fork.id == rightForkId then
        fork.copy(takeBy = None)
      else
        fork
    })
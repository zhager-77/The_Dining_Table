package task

import task.Errors.{PhilosopherNotEating, PhilosopherNotFound, PhilosopherNotThinking}
import task.PhilosopherState.{Eating, Thinking}

object Logic:
  
  //Создаем стол
  def createTable(size: Int): (Table, TableState) = 
    val table = Table(
      philosophers = Vector.tabulate(size)(id => Philosopher(id)),
      forks = Vector.tabulate(size)(id => Fork(id))
    )
    val state = TableState(
      takenForks = Map.empty,
      philosopherState = (0 until size).map(id => id -> PhilosopherState.Thinking).toMap,
      meals = (0 until size).map(id => id -> 0).toMap
    )
    (table, state)


  //Даем нумерацию левой вилке каждого философа 
  def leftForkId(table: Table, philosopherId: Int): Int =
    if philosopherId == 0 then table.forks.size - 1
    else philosopherId - 1

  
  //Даем нумерацию правой вилке каждого философа 
  def rightForkId(philosopherId: Int): Int =
    philosopherId


  //Обновляем состояние философа
  private def updatedState(tableState: TableState, philosopherId: Int)
                          (f: PhilosopherState => PhilosopherState): Either[Errors, TableState] =
    if tableState.philosopherState.contains(philosopherId) then 
      Right(tableState.copy(
        philosopherState = tableState.philosopherState.map{ case (id, state) => 
          if id == philosopherId then id -> f(state)
          else id -> state
        }
      )
      )
    else 
      Left(PhilosopherNotFound(philosopherId))
  
  //Меняем состояние на Eating
  def tryEat(philosopherId: Int, table: Table, tableState: TableState): Either[Errors, TableState] =
    for 
      state <- findPhilosopherState(tableState, philosopherId)
      _ <- ensureThinking(state, philosopherId)
      leftId = leftForkId(table, philosopherId)
      rightId = rightForkId(philosopherId)
      _ <- ensureForkFree(tableState, leftId)
      _ <- ensureForkFree(tableState, rightId)
      stateWithForks = takeForks(tableState, philosopherId, leftId, rightId)
      finalState <- updatedState(stateWithForks, philosopherId)(_ => Eating)
    yield incrementMeals(finalState, philosopherId)
      

  def findPhilosopherState(tableState: TableState, philosopherId: Int): Either[Errors, PhilosopherState] =
    tableState.philosopherState.get(philosopherId).toRight(Errors.PhilosopherNotFound(philosopherId))

  def ensureThinking(philosopherState: PhilosopherState, philosopherId: Int): Either[Errors, Unit] =
    if philosopherState == Thinking then Right(())
    else Left(PhilosopherNotThinking(philosopherId))

  def findFork(table: Table, forkId: Int): Either[Errors, Fork] =
    table.forks.find(_.id == forkId).toRight(Errors.ForkNotFound(forkId))

  def ensureForkFree(tableState: TableState, forkId: Int): Either[Errors, Unit] =
    tableState.takenForks.get(forkId) match {
      case None => Right(())
      case Some(philosopher) => Left(Errors.ForkTaken(forkId, philosopher))
    }

  def takeForks(tableState: TableState, philosopherId: Int, leftId: Int, rightId: Int): TableState =
    tableState.copy(takenForks = tableState.takenForks + (leftId -> philosopherId) + (rightId -> philosopherId))
  
  private def incrementMeals(tableState: TableState, philosopherId: Int): TableState =
    tableState.copy(meals = 
    tableState.meals.updatedWith(philosopherId)(_.map(_ + 1))
    )


  def finishEating(philosopherId: Int, table: Table, tableState: TableState): Either[Errors, TableState] =
    for 
      state <- findPhilosopherState(tableState, philosopherId)
      _ <- ensureEating(state, philosopherId)
      leftId = leftForkId(table, philosopherId)
      rightId = rightForkId(philosopherId)
      stateWithReleasedForks = releaseForks(tableState, leftId, rightId)
      finalState <- updatedState(stateWithReleasedForks, philosopherId)(_ => Thinking)
    yield
      finalState

  def ensureEating(philosopherState: PhilosopherState, philosopherId: Int): Either[Errors, Unit] =
    if philosopherState == Eating then Right(())
    else Left(PhilosopherNotEating(philosopherId))

  def releaseForks(tableState: TableState, leftId: Int, rightID: Int): TableState =
    tableState.copy(takenForks = tableState.takenForks - leftId - rightID)  
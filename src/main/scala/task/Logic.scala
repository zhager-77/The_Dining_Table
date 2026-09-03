package task

import task.Errors.{PhilosopherNotEating, PhilosopherNotThinking}
import task.PhilosopherState.{Eating, Thinking}

object Logic:

  // Создаем стол
  def createTable(size: Int): (Table, TableState) =
    val table = Table(
      philosophers =
        Vector.tabulate(size)(id => Philosopher(PhilosopherId(id))),
      forks = Vector.tabulate(size)(id => Fork(ForkId(id)))
    )
    val state = TableState(
      takenForks = Map.empty,
      philosopherState = (0 until size)
        .map(id => PhilosopherId(id) -> PhilosopherState.Thinking)
        .toMap,
      meals = (0 until size).map(id => PhilosopherId(id) -> 0).toMap
    )
    (table, state)

  // Даем нумерацию левой вилке каждого философа
  def leftForkId(table: Table, philosopherId: PhilosopherId): ForkId =
    if philosopherId.value == 0 then ForkId(table.forks.size - 1)
    else ForkId(philosopherId.value - 1)

  // Даем нумерацию правой вилке каждого философа
  def rightForkId(philosopherId: PhilosopherId): ForkId =
    ForkId(philosopherId.value)

  // Обновляем состояние философа
  private def updatedState(
      tableState: TableState,
      philosopherId: PhilosopherId
  )(
      f: PhilosopherState => PhilosopherState
  ): TableState = {
    tableState.copy(philosopherState =
      tableState.philosopherState.updatedWith(philosopherId)(_.map(f))
    )
  }

  // Меняем состояние на Eating
  def tryEat(
      philosopherId: PhilosopherId,
      table: Table,
      tableState: TableState
  ): Either[Errors, TableState] =
    for
      state = findPhilosopherState(tableState, philosopherId)
      _ <- ensureThinking(state, philosopherId)
      _ <- ensureMaxEat(tableState, table, philosopherId)
      leftId = leftForkId(table, philosopherId)
      rightId = rightForkId(philosopherId)
      _ <- ensureForkFree(tableState, leftId)
      _ <- ensureForkFree(tableState, rightId)
      stateWithForks = takeForks(tableState, philosopherId, leftId, rightId)
      finalState = updatedState(stateWithForks, philosopherId)(_ => Eating)
    yield incrementMeals(finalState, philosopherId)

  def findPhilosopherState(
      tableState: TableState,
      philosopherId: PhilosopherId
  ): PhilosopherState =
    tableState.philosopherState(philosopherId)

  def ensureThinking(
      philosopherState: PhilosopherState,
      philosopherId: PhilosopherId
  ): Either[Errors, Unit] =
    if philosopherState == Thinking then Right(())
    else Left(PhilosopherNotThinking(philosopherId))

  def ensureForkFree(
      tableState: TableState,
      forkId: ForkId
  ): Either[Errors, Unit] =
    tableState.takenForks.get(forkId) match {
      case None              => Right(())
      case Some(philosopher) => Left(Errors.ForkTaken(forkId, philosopher))
    }

  def takeForks(
      tableState: TableState,
      philosopherId: PhilosopherId,
      leftId: ForkId,
      rightId: ForkId
  ): TableState =
    tableState.copy(takenForks =
      tableState.takenForks + (leftId -> philosopherId) + (rightId -> philosopherId)
    )

  private def incrementMeals(
      tableState: TableState,
      philosopherId: PhilosopherId
  ): TableState =
    tableState.copy(meals =
      tableState.meals.updatedWith(philosopherId)(_.map(_ + 1))
    )

  def finishEating(
      philosopherId: PhilosopherId,
      table: Table,
      tableState: TableState
  ): Either[Errors, TableState] =
    for
      state = findPhilosopherState(tableState, philosopherId)
      _ <- ensureEating(state, philosopherId)
      leftId = leftForkId(table, philosopherId)
      rightId = rightForkId(philosopherId)
      stateWithReleasedForks = releaseForks(tableState, leftId, rightId)
      finalState = updatedState(stateWithReleasedForks, philosopherId)(_ =>
        Thinking
      )
    yield finalState

  def ensureEating(
      philosopherState: PhilosopherState,
      philosopherId: PhilosopherId
  ): Either[Errors, Unit] =
    if philosopherState == Eating then Right(())
    else Left(PhilosopherNotEating(philosopherId))

  def releaseForks(
      tableState: TableState,
      leftId: ForkId,
      rightID: ForkId
  ): TableState =
    tableState.copy(takenForks = tableState.takenForks - leftId - rightID)

  def leftNeighborId(
      table: Table,
      philosopherId: PhilosopherId
  ): PhilosopherId =
    val n = table.philosophers.size
    PhilosopherId((philosopherId.value - 1 + n) % n)

  def rightNeighborId(
      table: Table,
      philosopherId: PhilosopherId
  ): PhilosopherId =
    val n = table.philosophers.size
    PhilosopherId((philosopherId.value + 1) % n)

  def ensureMaxEat(
      tableState: TableState,
      table: Table,
      philosopherId: PhilosopherId
  ): Either[Errors, Unit] =
    val myMeals = tableState.meals.getOrElse(philosopherId, 0)
    val leftMeals =
      tableState.meals.getOrElse(leftNeighborId(table, philosopherId), 0)
    val rightMeals =
      tableState.meals.getOrElse(rightNeighborId(table, philosopherId), 0)
    val maxMeals = 2

    if myMeals - math.min(leftMeals, rightMeals) > maxMeals then
      Left(Errors.MoreThenMaxMeal(philosopherId))
    else Right(())

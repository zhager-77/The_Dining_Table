package task

final case class TableState(
    takenForks: Map[ForkId, PhilosopherId],
    philosopherState: Map[PhilosopherId, PhilosopherState],
    meals: Map[PhilosopherId, Int]
)

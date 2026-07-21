package task

final case class TableState(
                           takenForks: Map[Int, Int],
                           philosopherState: Map[Int, PhilosopherState],
                           meals: Map[Int, Int]
                           )
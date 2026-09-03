package task
// Один философ
opaque type PhilosopherId = Int
object PhilosopherId:
  def apply(value: Int): PhilosopherId = value
  extension (id: PhilosopherId) def value: Int = id

final case class Philosopher(id: PhilosopherId)

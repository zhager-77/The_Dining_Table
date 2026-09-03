package task
//Одна вилка

opaque type ForkId = Int
object ForkId:
  def apply(value: Int): ForkId = value
  extension (id: ForkId) def value: Int = id
final case class Fork(id: ForkId)

package task
//Ошибки из которых будет состоять логика
enum Errors:
  case ForkNotFound(id: Int)
  case PhilosopherNotFound(id: Int)
  case PhilosopherNotHungry(id: Int)
  case PhilosopherNotEating(id: Int)
  case ForkTaken(forkId: Int, takeBy: Int)
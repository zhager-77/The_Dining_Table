package task
//Ошибки из которых будет состоять логика
enum Errors:
  case PhilosopherNotFound(id: PhilosopherId)
  case PhilosopherNotThinking(id: PhilosopherId)
  case PhilosopherNotEating(id: PhilosopherId)
  case ForkTaken(forkId: ForkId, takeBy: PhilosopherId)
  case MoreThenMaxMeal(philosopherId: PhilosopherId)
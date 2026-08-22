package task

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import task.PhilosopherState.{Eating, Thinking}

class LogicTest extends AnyFunSuite with ScalaCheckPropertyChecks:

  test("createTable creates the required number of philosophers and forks") {
    val (table, _) = Logic.createTable(5)

    assert(table.philosophers.size == 5)
    assert(table.forks.size == 5)
  }

  test("createTable creates thinking philosophers and free forks") {
    val (_, state) = Logic.createTable(5)

    assert(state.philosopherState.values.forall(_ == Thinking))
    assert(state.takenForks.isEmpty)
  }

  test("tryEat changes Thinking philosopher to Eating and takes both forks") {
    val (table, state) = Logic.createTable(5)

    val result = Logic.tryEat(PhilosopherId(2), table, state)

    assert(result.exists(_.philosopherState(PhilosopherId(2)) == Eating))
    assert(
      result.exists(s =>
        s.takenForks(ForkId(1)) == PhilosopherId(2) && s.takenForks(
          ForkId(2)
        ) == PhilosopherId(2)
      )
    )
  }

  test(
    "tryEat returns PhilosopherNotThinking if philosopher is already Eating"
  ) {
    val (table, state) = Logic.createTable(5)

    val result =
      for
        eatingState <- Logic.tryEat(PhilosopherId(2), table, state)
        secondTry <- Logic.tryEat(PhilosopherId(2), table, eatingState)
      yield secondTry

    assert(result == Left(Errors.PhilosopherNotThinking(PhilosopherId(2))))
  }

  test("tryEat returns PhilosopherNotFound for a missing philosopher") {
    val (table, state) = Logic.createTable(5)

    val result = Logic.tryEat(PhilosopherId(99), table, state)

    assert(result == Left(Errors.PhilosopherNotFound(PhilosopherId(99))))
  }

  test("tryEat returns ForkTaken when one of the required forks is occupied") {
    val (table, state) = Logic.createTable(5)

    val result =
      for
        afterPhilosopher2 <- Logic.tryEat(PhilosopherId(2), table, state)
        result <- Logic.tryEat(PhilosopherId(3), table, afterPhilosopher2)
      yield ()

    assert(result == Left(Errors.ForkTaken(ForkId(2), PhilosopherId(2))))
  }

  test("tryEat does not change unrelated philosophers and forks") {
    val (table, state) = Logic.createTable(5)

    val result = Logic.tryEat(PhilosopherId(2), table, state)

    assert(
      result.exists(s =>
        s.philosopherState
          .filter(_._1 != PhilosopherId(2))
          .values
          .forall(_ == Thinking)
      )
    )
    assert(
      result.exists(s =>
        !s.takenForks.contains(ForkId(0)) && !s.takenForks.contains(
          ForkId(3)
        ) && !s.takenForks
          .contains(ForkId(4))
      )
    )
  }

  test(
    "finishEating changes Eating philosopher to Thinking and releases both forks"
  ) {
    val (table, state) = Logic.createTable(5)

    val result =
      for
        eatingState <- Logic.tryEat(PhilosopherId(2), table, state)
        finishedState <- Logic.finishEating(
          PhilosopherId(2),
          table,
          eatingState
        )
      yield finishedState

    assert(result.exists(_.philosopherState(PhilosopherId(2)) == Thinking))
    assert(
      result.exists(s =>
        !s.takenForks.contains(ForkId(1)) && !s.takenForks.contains(ForkId(2))
      )
    )
  }

  test(
    "finishEating returns PhilosopherNotEating if philosopher is not Eating"
  ) {
    val (table, state) = Logic.createTable(5)

    val result = Logic.finishEating(PhilosopherId(2), table, state)

    assert(result == Left(Errors.PhilosopherNotEating(PhilosopherId(2))))
  }

  test("finishEating returns PhilosopherNotFound for a missing philosopher") {
    val (table, state) = Logic.createTable(5)

    val result = Logic.finishEating(PhilosopherId(99), table, state)

    assert(result == Left(Errors.PhilosopherNotFound(PhilosopherId(99))))
  }

  test("finishEating does not change unrelated philosophers or forks") {
    val (table, state) = Logic.createTable(5)

    val result =
      for
        eatingState <- Logic.tryEat(PhilosopherId(2), table, state)
        finishedState <- Logic.finishEating(
          PhilosopherId(2),
          table,
          eatingState
        )
      yield finishedState

    assert(
      result.exists(s =>
        s.philosopherState
          .filter(_._1 != PhilosopherId(2))
          .values
          .forall(_ == Thinking)
      )
    )
    assert(
      result.exists(s =>
        !s.takenForks.contains(ForkId(0)) && !s.takenForks.contains(
          ForkId(3)
        ) && !s.takenForks
          .contains(ForkId(4))
      )
    )
  }

  test("tryEat for philosopher 0 takes the last and the first fork") {
    val (table, state) = Logic.createTable(5)

    val result = Logic.tryEat(PhilosopherId(0), table, state)

    assert(result.exists(_.philosopherState(PhilosopherId(0)) == Eating))
    assert(
      result.exists(s =>
        s.takenForks(ForkId(4)) == PhilosopherId(0) && s.takenForks(
          ForkId(0)
        ) == PhilosopherId(0)
      )
    )
  }

  test(
    "property: createTable keeps philosophers and forks count equal to size"
  ) {
    forAll(Gen.choose(1, 100)) { size =>
      val (table, _) = Logic.createTable(size)

      assert(table.philosophers.size == size)
      assert(table.forks.size == size)
    }
  }

  test(
    "property: createTable creates only Thinking philosophers and free forks"
  ) {
    forAll(Gen.choose(1, 100)) { size =>
      val (_, state) = Logic.createTable(size)

      assert(state.philosopherState.values.forall(_ == Thinking))
      assert(state.takenForks.isEmpty)
    }
  }

  test("property: tryEat changes Thinking philosopher to Eating") {
    forAll(Gen.choose(2, 100)) { size =>
      forAll(Gen.choose(0, size - 1)) { likePhilosopherId =>
        val philosopherId = PhilosopherId(likePhilosopherId)
        val (table, state) = Logic.createTable(size)

        val result = Logic.tryEat(philosopherId, table, state)

        assert(result.exists(_.philosopherState(philosopherId) == Eating))
      }
    }
  }

  test(
    "property: tryEat takes the left and right forks of the selected philosopher"
  ) {
    forAll(Gen.choose(2, 100)) { size =>
      forAll(Gen.choose(0, size - 1)) { likePhilosopherId =>
        val philosopherId = PhilosopherId(likePhilosopherId)
        val (table, state) = Logic.createTable(size)

        val result = Logic.tryEat(philosopherId, table, state)

        val expectedLeftForkId =
          if likePhilosopherId == 0 then ForkId(size - 1)
          else ForkId(likePhilosopherId - 1)
        val expectedRightForkId = ForkId(likePhilosopherId)

        assert(
          result.exists(s =>
            s.takenForks(expectedLeftForkId) == philosopherId &&
              s.takenForks(expectedRightForkId) == philosopherId
          )
        )
      }
    }
  }

  test("property: finishEating changes philosopher back to Thinking") {
    forAll(Gen.choose(2, 100)) { size =>
      forAll(Gen.choose(0, size - 1)) { likePhilosopherId =>
        val philosopherId = PhilosopherId(likePhilosopherId)
        val (table, state) = Logic.createTable(size)

        val result =
          for
            eatingState <- Logic.tryEat(philosopherId, table, state)
            finishedState <- Logic.finishEating(
              philosopherId,
              table,
              eatingState
            )
          yield finishedState

        assert(result.exists(_.philosopherState(philosopherId) == Thinking))
      }
    }
  }

  test(
    "property: finishEating releases the left and right forks of the selected philosopher"
  ) {
    forAll(Gen.choose(2, 100)) { size =>
      forAll(Gen.choose(0, size - 1)) { likePhilosopherId =>
        val philosopherId = PhilosopherId(likePhilosopherId)
        val (table, state) = Logic.createTable(size)

        val result =
          for
            eatingState <- Logic.tryEat(philosopherId, table, state)
            finishedState <- Logic.finishEating(
              philosopherId,
              table,
              eatingState
            )
          yield finishedState

        val expectedLeftForkId =
          if likePhilosopherId == 0 then ForkId(size - 1)
          else ForkId(likePhilosopherId - 1)
        val expectedRightForkId = ForkId(likePhilosopherId)

        assert(
          result.exists(s =>
            !s.takenForks.contains(expectedLeftForkId) &&
              !s.takenForks.contains(expectedRightForkId)
          )
        )
      }
    }
  }

  test(
    "property: tryEat returns PhilosopherNotThinking for already Eating philosopher"
  ) {
    forAll(Gen.choose(2, 100)) { size =>
      forAll(Gen.choose(0, size - 1)) { likePhilosopherId =>
        val philosopherId = PhilosopherId(likePhilosopherId)
        val (table, state) = Logic.createTable(size)

        val result =
          for
            eatingState <- Logic.tryEat(philosopherId, table, state)
            secondTry <- Logic.tryEat(philosopherId, table, eatingState)
          yield secondTry

        assert(result == Left(Errors.PhilosopherNotThinking(philosopherId)))
      }
    }
  }

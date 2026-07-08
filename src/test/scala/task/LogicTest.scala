package task

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import task.PhilosopherState.{Eating, Hungry, Thinking}

class LogicTest extends AnyFunSuite with ScalaCheckPropertyChecks:

  test("createTable creates the required number of philosophers and forks"){
    val table = Logic.createTable(5)

    assert(table.philosophers.size == 5)
    assert(table.forks.size == 5)
  }

  test("createTable creates thinking philosophers and free forks"){
    val table = Logic.createTable(5)

    assert(table.philosophers.forall(_.state == Thinking))
    assert(table.forks.forall(_.takeBy.isEmpty))
  }

  test("becomeHungry changes the selected philosopher state to Hungry"){
    val table = Logic.createTable(5)

    val result = Logic.becomeHungry(table, 2)

    assert(result.exists{updatedTable =>
      updatedTable.philosophers.find(_.id == 2).exists(_.state == Hungry)
    })
  }

  test("becomeHungry does not change other philosophers and forks"){
    val table = Logic.createTable(5)

    val result = Logic.becomeHungry(table, 2)

    assert(result.exists{updatedTable =>
      updatedTable.philosophers.filterNot(_.id == 2).forall(_.state == Thinking)
    })

    assert(result.exists{updatedTable =>
      updatedTable.forks == table.forks
    })
  }

  test("becomeHungry not missing philosopher"){
    val table = Logic.createTable(5)

    val result = Logic.becomeHungry(table, 99)

    assert(result == Left(Errors.PhilosopherNotFound(99)))
  }

  test("tryEat changes Hungry philosopher to Eating and takes both forks"){
    val table = Logic.createTable(5)

    val result =
      for
        hungryTable <- Logic.becomeHungry(table, 2)
        eatingTable <- Logic.tryEat(hungryTable, 2)
      yield eatingTable

    assert(result.exists(updatedTable =>
    updatedTable.philosophers.find(_.id == 2).exists(_.state == Eating)))

    assert(result.exists(updatedTable =>
    updatedTable.forks.find(_.id == 1).exists(_.takeBy.contains(2)) &&
    updatedTable.forks.find(_.id == 2).exists(_.takeBy.contains(2))
    ))
  }

  test("tryEat returns PhilosopherNotHungry if philosopher state is Thinking"){
    val table = Logic.createTable(5)

    val result = Logic.tryEat(table, 2)

    assert(result == Left(Errors.PhilosopherNotHungry(2)))
  }

  test("tryEat returns PhilosopherNotFound for a missing philosopher"){
    val table = Logic.createTable(5)

    val result = Logic.tryEat(table, 99)

    assert(result == Left(Errors.PhilosopherNotFound(99)))
  }

  test("tryEat returns ForkAlreadyTaken when one of the required forks is occupied"){
    val table = Logic.createTable(5)

    val result =
      for
        hungryPhilosopher2 <- Logic.becomeHungry(table, 2)
        philosopher2Eating <- Logic.tryEat(hungryPhilosopher2, 2)
        hungryPhilosopher3 <- Logic.becomeHungry(philosopher2Eating, 3)
        result <- Logic.tryEat(hungryPhilosopher3, 3)
      yield result

    assert(result == Left(Errors.ForkTaken(2, 2)))
  }


  test("tryEat does not change unrelated philosophers and forks"){
    val table = Logic.createTable(5)

    val result =
      for
        hungryTable <- Logic.becomeHungry(table, 2)
        eatingTable <- Logic.tryEat(hungryTable, 2)
      yield eatingTable

    assert(result.exists(updatedTable =>
    updatedTable.philosophers.filterNot(_.id == 2).forall(_.state == Thinking)))

    assert(result.exists(updatedTable =>
    updatedTable.forks.find(_.id == 0).exists(_.takeBy.isEmpty) &&
    updatedTable.forks.find(_.id == 3).exists(_.takeBy.isEmpty) &&
    updatedTable.forks.find(_.id == 4).exists(_.takeBy.isEmpty)))
  }

  test("finishEating changes Eating philosopher to Thinking and releases both forks"){
    val table = Logic.createTable(5)

    val result =
      for
        hungryTable <- Logic.becomeHungry(table, 2)
        eatingTable <- Logic.tryEat(hungryTable, 2)
        finishTable <- Logic.finishEating(eatingTable, 2)
      yield finishTable

    assert(result.exists(updatedTable =>
    updatedTable.philosophers.find(_.id == 2).exists(_.state == Thinking)))

    assert(result.exists(updatedTable =>
    updatedTable.forks.find(_.id == 1).exists(_.takeBy.isEmpty) &&
    updatedTable.forks.find(_.id == 2).exists(_.takeBy.isEmpty)))
  }

  test("finishEating returns PhilosopherNotEating if philosopher is not Eating"){
    val table = Logic.createTable(5)

    val result = Logic.finishEating(table, 2)

    assert(result == Left(Errors.PhilosopherNotEating(2)))
  }

  test("finishEating returns PhilosopherNotFound for a missing philosopher"){
    val table = Logic.createTable(5)

    val result = Logic.finishEating(table, 99)

    assert(result == Left(Errors.PhilosopherNotFound(99)))
  }

  test("finishEating does not change unrelated philosophers or forks"){
    val table = Logic.createTable(5)

    val result = {
      for
        hungryTable <- Logic.becomeHungry(table, 2)
        eatingTable <- Logic.tryEat(hungryTable, 2)
        finishTable <- Logic.finishEating(eatingTable, 2)
      yield finishTable
    }

    assert(result.exists(updatedTable =>
    updatedTable.philosophers.filterNot(_.id == 2).forall(_.state == Thinking)))

    assert(result.exists(updatedTable =>
    updatedTable.forks.find(_.id == 0).exists(_.takeBy.isEmpty) &&
    updatedTable.forks.find(_.id == 3).exists(_.takeBy.isEmpty) &&
    updatedTable.forks.find(_.id == 4).exists(_.takeBy.isEmpty)))
  }

  test("tryEat for philosopher 0 takes the last and the first fork"){
    val table = Logic.createTable(5)

    val result =
      for
        hungryTable <- Logic.becomeHungry(table, 0)
        eatingTable <- Logic.tryEat(hungryTable, 0)
      yield eatingTable

    assert(result.exists(updatedTable =>
    updatedTable.philosophers.find(_.id == 0).exists(_.state == Eating)))

    assert(result.exists(updatedTable =>
    updatedTable.forks.find(_.id == 4).exists(_.takeBy.contains(0) &&
    updatedTable.forks.find(_.id == 0).exists(_.takeBy.contains(0)))))
  }


  test("property: createTable keeps philosophers and forks count equal to size"){
    val positiveTableSizes = Gen.choose(1, 100)

    forAll(positiveTableSizes){size =>
      val table = Logic.createTable(size)

      assert(table.philosophers.size == size)
      assert(table.forks.size == size)
    }
  }

  test("property: createTable creates only Thinking philosophers and free forks"){
    val positiveTable = Gen.choose(1, 100)

    forAll(positiveTable){size =>
      val table = Logic.createTable(size)

      assert(table.philosophers.forall(_.state == Thinking))
      assert(table.forks.forall(_.takeBy.isEmpty))
    }
  }

  test("property: becomeHungry changes selected philosopher to Hungry"){
    val tableSizes = Gen.choose(1, 100)

    forAll(tableSizes){size =>
      val philosopherIds = Gen.choose(0, size - 1)

      forAll(philosopherIds){philosopherId =>
        val table = Logic.createTable(size)
        val result = Logic.becomeHungry(table, philosopherId)

        assert(result.exists(updatedTable =>
        updatedTable.philosophers.find(_.id == philosopherId).exists(_.state == Hungry)))
    }
  }
  }

  test("property: becomeHungry does not change other philosophers or forks"){
    val tableSizes = Gen.choose(1, 100)

    forAll(tableSizes){size =>
      val philosopherIds = Gen.choose(0, size - 1)

      forAll(philosopherIds){philosopherId =>
        val table = Logic.createTable(size)
        val result = Logic.becomeHungry(table, philosopherId)

        assert(result.exists(updatedTable =>
        updatedTable.philosophers.filterNot(_.id == philosopherId).forall(_.state == Thinking)))

        assert(result.exists(updatedTable =>
          updatedTable.forks == table.forks))
      }
    }
  }

  test("property: tryEat changes Hungry philosopher to Eating"){
    val tableSizes = Gen.choose(2, 100)

    forAll(tableSizes){size =>
      val philosopherIds = Gen.choose(0, size - 1)

      forAll(philosopherIds){philosopherId =>
        val table = Logic.createTable(size)

        val result =
          for
            hungryTable <- Logic.becomeHungry(table, philosopherId)
            eatingTable <- Logic.tryEat(hungryTable, philosopherId)
          yield eatingTable

        assert(result.exists(updatedTable =>
        updatedTable.philosophers.find(_.id == philosopherId).exists(_.state == Eating)))
      }
    }
  }

  test("property: tryEat takes the left and right forks of the selected philosopher"){
    val tableSizes = Gen.choose(2, 100)

    forAll(tableSizes){size =>
      val philosopherIds = Gen.choose(0, size - 1)

      forAll(philosopherIds){philosopherId =>
        val table = Logic.createTable(size)

        val result =
          for
            hungryTable <- Logic.becomeHungry(table, philosopherId)
            eatingTable <- Logic.tryEat(hungryTable, philosopherId)
          yield eatingTable

        val expectedLeftForkId =
          if philosopherId == 0 then size - 1
          else philosopherId - 1

        val expectedRightForkId = philosopherId

        assert(result.exists(updatedTable =>
        updatedTable.forks.find(_.id == expectedLeftForkId).exists(_.takeBy.contains(philosopherId))
        &&
        updatedTable.forks.find(_.id == expectedRightForkId).exists(_.takeBy.contains(philosopherId))))
      }
    }
  }

  test("property: finishEating changes Eating philosopher back to Thinking"){
    val tableSizes = Gen.choose(2, 100)

    forAll(tableSizes){size =>
      val philosopherIds = Gen.choose(0, size - 1)

      forAll(philosopherIds){philosopherId =>
        val table = Logic.createTable(size)

        val result =
          for
            hungryTable <- Logic.becomeHungry(table, philosopherId)
            eatingTable <- Logic.tryEat(hungryTable, philosopherId)
            finishedTable <- Logic.finishEating(eatingTable, philosopherId)
          yield finishedTable

        assert(result.exists(updatedTable =>
            updatedTable.philosophers.find(_.id == philosopherId).exists(_.state == Thinking)))
      }
    }
  }

  test("property: finishEating releases the left and right forks of the selected philosopher"){
    val tableSizes = Gen.choose(2, 100)

    forAll(tableSizes){size =>
      val philosopherIds = Gen.choose(0, size - 1)

      forAll(philosopherIds){philosopherId =>
        val table = Logic.createTable(size)

        val result =
          for
            hungryTable <- Logic.becomeHungry(table, philosopherId)
            eatingTable <- Logic.tryEat(hungryTable, philosopherId)
            finishedTable <- Logic.finishEating(eatingTable, philosopherId)
          yield finishedTable

        val expectedLeftForkId =
          if philosopherId == 0 then size - 1
          else philosopherId - 1

        val expectedRightForkId = philosopherId

        assert(result.exists(updatedTable =>
        updatedTable.forks.find(_.id == expectedLeftForkId).exists(_.takeBy.isEmpty) &&
        updatedTable.forks.find(_.id == expectedRightForkId).exists(_.takeBy.isEmpty)))
      }
    }
  }

  test("property: tryEat returns PhilosopherNotHungry for Thinking philosopher"){
    val tableSizes = Gen.choose(2, 100)

    forAll(tableSizes){size =>
      val philosopherIds = Gen.choose(0, size - 1)

      forAll(philosopherIds){philosopherId =>
        val table = Logic.createTable(size)
        val result = Logic.tryEat(table, philosopherId)

        assert(result == Left(Errors.PhilosopherNotHungry(philosopherId)))
      }
    }
  }
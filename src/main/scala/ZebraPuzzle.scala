import scala.collection.mutable

/**
 * Solve the zebra puzzle.
 *
 * 1. There are five houses.
 * 2. The Englishman lives in the red house.
 * 3. The Spaniard owns the dog.
 * 4. Coffee is drunk in the green house.
 * 5. The Ukrainian drinks tea.
 * 6. The green house is immediately to the right of the ivory house.
 * 7. The Old Gold smoker owns snails.
 * 8. Kools are smoked in the yellow house.
 * 9. Milk is drunk in the middle house.
 * 10. The Norwegian lives in the first house.
 * 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
 * 12. Kools are smoked in the house next to the house where the horse is kept.
 * 13. The Lucky Strike smoker drinks orange juice.
 * 14. The Japanese smokes Parliaments.
 * 15. The Norwegian lives next to the blue house.
 *
 * Each of the five houses is painted a different color,
 * and their inhabitants are of different national extractions,
 * own different pets, drink different beverages
 * and smoke different brands of cigarettes.
 *
 * Which of the residents drinks water? Who owns the zebra?
 * */

object ZebraPuzzle {

  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Yellow extends Color
  case object Blue extends Color
  case object Ivory extends Color
  object Color {
    val all: List[Color] = List(Red, Green, Yellow, Blue, Ivory)
    def restOf(houses: List[House]): List[Color] = all.diff(houses.flatMap(_.color))
    def update(houses: List[House]): List[List[House]] =
      (for {
        color <- Color.restOf(houses)
        ind <- houses.zipWithIndex.filter(_._1.color.isEmpty).map(_._2)
      } yield {
        houses.updated(ind, houses(ind).copy(color = Some(color)))
      }) match {
        case Nil => List(houses)
        case list => list
      }
  }

  sealed trait Resident
  case object Englishman extends Resident
  case object Spaniard extends Resident
  case object Ukrainian extends Resident
  case object Norwegian extends Resident
  case object Japanese extends Resident
  object Resident {
    val all: List[Resident] = List(Englishman, Spaniard, Ukrainian, Norwegian, Japanese)
    def restOf(houses: List[House]): List[Resident] = all.diff(houses.flatMap(_.resident))
    def update(houses: List[House]): List[List[House]] =
      (for {
        resident <- Resident.restOf(houses)
        ind <- houses.zipWithIndex.filter(_._1.resident.isEmpty).map(_._2)
      } yield {
        houses.updated(ind, houses(ind).copy(resident = Some(resident)))
      }) match {
        case Nil => List(houses)
        case list => list
      }
  }

  sealed trait Animal
  case object Dog extends Animal
  case object Snails extends Animal
  case object Fox extends Animal
  case object Horse extends Animal
  case object Zebra extends Animal
  object Animal {
    val all: List[Animal] = List(Dog, Snails, Fox, Horse, Zebra)
    def restOf(houses: List[House]): List[Animal] = all.diff(houses.flatMap(_.animal))
    def update(houses: List[House]): List[List[House]] =
      (for {
        animal <- Animal.restOf(houses)
        ind <- houses.zipWithIndex.filter(_._1.animal.isEmpty).map(_._2)
      } yield {
        houses.updated(ind, houses(ind).copy(animal = Some(animal)))
      }) match {
        case Nil => List(houses)
        case list => list
      }
  }

  sealed trait Drink
  case object Coffee extends Drink
  case object Tea extends Drink
  case object Milk extends Drink
  case object OrangeJuice extends Drink
  case object Water extends Drink
  object Drink {
    val all: List[Drink] = List(Coffee, Tea, Milk, OrangeJuice, Water)
    def restOf(houses: List[House]): List[Drink] = all.diff(houses.flatMap(_.drink))
    def update(houses: List[House]): List[List[House]] =
      (for {
        drink <- Drink.restOf(houses)
        ind <- houses.zipWithIndex.filter(_._1.drink.isEmpty).map(_._2)
      } yield {
        houses.updated(ind, houses(ind).copy(drink = Some(drink)))
      }) match {
        case Nil => List(houses)
        case list => list
      }
  }

  sealed trait Smoke
  case object TheOldGold extends Smoke
  case object Kools extends Smoke
  case object Chesterfields extends Smoke
  case object LuckyStrike extends Smoke
  case object Parliaments extends Smoke
  object Smoke {
    val all: List[Smoke] = List(TheOldGold, Kools, Chesterfields, LuckyStrike, Parliaments)
    def restOf(houses: List[House]): List[Smoke] = all.diff(houses.flatMap(_.smoke))
    def update(houses: List[House]): List[List[House]] =
      (for {
        smoke <- Smoke.restOf(houses)
        ind <- houses.zipWithIndex.filter(_._1.smoke.isEmpty).map(_._2)
      } yield {
        houses.updated(ind, houses(ind).copy(smoke = Some(smoke)))
      }) match {
        case Nil => List(houses)
        case list => list
      }
  }

  case class House(
                    num: Int,
                    color: Option[Color],
                    resident: Option[Resident],
                    animal: Option[Animal],
                    drink: Option[Drink],
                    smoke: Option[Smoke]
                  ) {
    def countUndefined: Int =
      color.isEmpty.compare(false) +
        resident.isEmpty.compare(false) +
        animal.isEmpty.compare(false) +
        drink.isEmpty.compare(false) +
        smoke.isEmpty.compare(false)

    def isDefined: Boolean =
      color.isDefined &&
        resident.isDefined &&
        animal.isDefined &&
        drink.isDefined &&
        smoke.isDefined

    override def toString: String =
      f"House(num=$num%1d," +
        f"color=[${color.map(_.toString).getOrElse("")}%8s]," +
        f"resident=[${resident.map(_.toString).getOrElse("")}%10s]," +
        f"animal=[${animal.map(_.toString).getOrElse("")}%10s]," +
        f"drink=[${drink.map(_.toString).getOrElse("")}%12s]," +
        f"smoke=[${smoke.map(_.toString).getOrElse("")}%15s])"
  }

  case class Solution(waterDrinker: Resident, zebraOwner: Resident)

  val conditionsNum: PartialFunction[House, Boolean] = {
    // 9. Milk is drunk in the middle(2) house.
    case House(2, _, _, _, Some(drink), _)    => drink == Milk
    // 10. The Norwegian lives in the first house.
    case House(0, _, Some(resident), _, _, _) => resident == Norwegian
  }

  val conditionsColor: PartialFunction[House, Boolean] = {
    // 2. The Englishman lives in the red house.
    case House(_, Some(Red), Some(resident), _, _, _)  => resident == Englishman
    // 4. Coffee is drunk in the green house && The green house is immediately to the right of the ivory house.
    case House(num, Some(Green), _, _, Some(drink), _) => List(3, 4).contains(num) && drink == Coffee
    // 6. The green house is immediately to the right of the ivory house.
    case House(num, Some(Green), _, _, _, _)   => List(3, 4).contains(num)
    // 8. Kools are smoked in the yellow house.
    case House(_, Some(Yellow), _, _, _, Some(smoke))  => smoke == Kools
    // 15. The Norwegian lives next to the blue house.
    case House(_, Some(Blue), Some(resident), _, _, _) => resident != Norwegian
    // 6. The green house is immediately to the right of the ivory house.
    case House(num, Some(Ivory), _, _, _, _)           => List(2, 3).contains(num)
  }

  val conditionsResident: PartialFunction[House, Boolean] = {
    // 2. The Englishman lives in the red house.
    case House(_, Some(color), Some(Englishman), _, _, _) => color == Red
    // 3. The Spaniard owns the dog.
    case House(_, _, Some(Spaniard), Some(animal), _, _) => animal == Dog
    // 5. The Ukrainian drinks tea.
    case House(_, _, Some(Ukrainian), _, Some(drink), _) => drink == Tea
    // 10. The Norwegian lives in the first house && next to the blue house.
    case House(num, Some(color), Some(Norwegian), _, _, _) => num == 0 && color != Blue
    // 10. The Norwegian lives in the first house.
    case House(num, _, Some(Norwegian), _, _, _) => num == 0
    // 14. The Japanese smokes Parliaments.
    case House(_, _, Some(Japanese), _, _, Some(smoke)) => smoke == Parliaments
  }

  val conditionsAnimal: PartialFunction[House, Boolean] = {
    // 3. The Spaniard owns the dog.
    case House(_, _, Some(resident), Some(Dog), _, _) => resident == Spaniard
    // 7. The Old Gold smoker owns snails.
    case House(_, _, _, Some(Snails), _, Some(smoke)) => smoke == TheOldGold
    // 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
    case House(_, _, _, Some(Fox), _, Some(smoke)) => smoke != Chesterfields
    // 12. Kools are smoked in the house next to the house where the horse is kept.
    case House(_, _, _, Some(Horse), _, Some(smoke)) => smoke != Kools
  }

  val conditionsDrink: PartialFunction[House, Boolean] = {
    // 4. Coffee is drunk in the green house.
    case House(_, Some(color), _, _, Some(Coffee), _) => color == Green
    // 5. The Ukrainian drinks tea.
    case House(_, _, Some(resident), _, Some(Tea), _) => resident == Ukrainian
    // 9. Milk is drunk in the middle(2) house.
    case House(num, _, _, _, Some(Milk), _) => num == 2
    // 13. The Lucky Strike smoker drinks orange juice.
    case House(_, _, _, _, Some(OrangeJuice), Some(smoke)) => smoke == LuckyStrike
  }

  val conditionsSmoke: PartialFunction[House, Boolean] = {
    // 7. The Old Gold smoker owns snails.
    case House(_, _, _, Some(animal), _, Some(TheOldGold)) => animal == Snails
    // 8. Kools are smoked in the yellow house.
    case House(_, Some(color), _, _, _, Some(Kools)) => color == Yellow
    // 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
    case House(_, _, _, Some(animal), _, Some(Chesterfields)) => animal != Fox
    // 12. Kools are smoked in the house next to the house where the horse is kept.
    case House(_, _, _, Some(animal), _, Some(Kools)) => animal != Horse
    // 13. The Lucky Strike smoker drinks orange juice.
    case House(_, _, _, _, Some(drink), Some(LuckyStrike)) => drink == OrangeJuice
    // 14. The Japanese smokes Parliaments.
    case House(_, _, Some(resident), _, _, Some(Parliaments)) => resident == Japanese
  }

  val conditions: List[PartialFunction[House, Boolean]] =
    List(
      conditionsNum,
      conditionsColor,
      conditionsResident,
      conditionsAnimal,
      conditionsDrink,
      conditionsSmoke
    )

  val conditionsBothColor: PartialFunction[(House, House), Boolean] = {
    // 6. The green house is immediately to the right of the ivory house.
    case (House(num1, Some(Green), _, _, _, _), House(num2, Some(Ivory), _, _, _, _)) => num1 == num2 + 1
    case (House(num1, Some(Ivory), _, _, _, _), House(num2, Some(Green), _, _, _, _)) => num1 == num2 - 1
    // 15. The Norwegian lives next to the blue house.
    case (House(num1, Some(Blue), _, _, _, _), House(num2, _, Some(Norwegian), _, _, _)) => (num1 - num2).abs == 1
  }

  val conditionsBothResident: PartialFunction[(House, House), Boolean] = {
    // 15. The Norwegian lives next to the blue house.
    case (House(num1, _, Some(Norwegian), _, _, _), House(num2, Some(Blue), _, _, _, _)) => (num1 - num2).abs == 1
  }

  val conditionsBothAnimal: PartialFunction[(House, House), Boolean] = {
    // 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
    case (House(num1, _, _, Some(Fox), _, _), House(num2, _, _, _, _, Some(Chesterfields))) => (num1 - num2).abs == 1
    // 12. Kools are smoked in the house next to the house where the horse is kept.
    case (House(num1, _, _, Some(Horse), _, _), House(num2, _, _, _, _, Some(Kools))) => (num1 - num2).abs == 1
  }

  val conditionsBothSmoke: PartialFunction[(House, House), Boolean] = {
    // 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
    case (House(num1, _, _, _, _, Some(Chesterfields)), House(num2, _, _, Some(Fox), _, _)) => (num1 - num2).abs == 1
    // 12. Kools are smoked in the house next to the house where the horse is kept.
    case (House(num1, _, _, _, _, Some(Kools)), House(num2, _, _, Some(Horse), _, _)) => (num1 - num2).abs == 1
  }

  val conditionsBoth: List[PartialFunction[(House, House), Boolean]] =
    List(
      conditionsBothColor,
      conditionsBothResident,
      conditionsBothAnimal,
      conditionsBothSmoke
    )

  lazy val setup: List[List[House]] = {
    // 1. There are five houses.
    val houses = mutable.ListBuffer.tabulate(5)(i => House(i, None, None, None, None, None))

    // 10. The Norwegian lives in the first house.
    houses(0) = houses.head.copy(resident = Some(Norwegian))
    // 15. The Norwegian lives next to the blue house.
    houses(1) = houses(1).copy(color = Some(Blue))
    // 9. Milk is drunk in the middle house.
    houses(2) = houses(2).copy(drink = Some(Milk))

    // 6. The green house is immediately to the right of the ivory house.
    // 4. Coffee is drunk in the green house.
    // 2. The Englishman lives in the red house.
    // 8. Kools are smoked in the yellow house.
    // 12. Kools are smoked in the house next to the house where the horse is kept.

    // 0 - Yellow 1 - Blue 2 - Red 3 - Ivory 4 - Green
    val houses1 = houses.clone()
    houses1(0) = houses1.head.copy(color = Some(Yellow), smoke = Some(Kools))
    houses1(1) = houses1(1).copy(color = Some(Blue), animal = Some(Horse))
    houses1(2) = houses1(2).copy(color = Some(Red), resident = Some(Englishman))
    houses1(3) = houses1(3).copy(color = Some(Ivory))
    houses1(4) = houses1(4).copy(color = Some(Green), drink = Some(Coffee))

    // 0 - Yellow 1 - Blue 2 - Ivory 3 - Green 4 - Red
    val houses2 = houses.clone()
    houses2(0) = houses2.head.copy(color = Some(Yellow), smoke = Some(Kools))
    houses2(1) = houses2(1).copy(color = Some(Blue), animal = Some(Horse))
    houses2(2) = houses2(2).copy(color = Some(Ivory))
    houses2(3) = houses2(3).copy(color = Some(Green), drink = Some(Coffee))
    houses2(4) = houses2(4).copy(color = Some(Red), resident = Some(Englishman))

    List(houses1.toList, houses2.toList)
  }

  def check(houses: List[House]): Boolean = {
    for {
      house <- houses
      fn <- conditions
    } {
      if (!fn.applyOrElse(house, (_: House) => true)) return false
    }

    for {
      ind1 <- houses.indices
      ind2 <- ind1 + 1 until houses.length
      fn <- conditionsBoth
    } {
      if (!fn.applyOrElse((houses(ind1), houses(ind2)), (_: (House, House)) => true)) return false
    }

    true
  }

  def solve: Solution = {
    implicit val ordering: Ordering[List[House]] = Ordering.by(-_.map(_.countUndefined).sum)
    val queue = mutable.PriorityQueue.empty[List[House]]
    val initialHouses: List[List[House]] = setup

    queue ++= initialHouses
    while(queue.nonEmpty) {
      val houses = queue.dequeue
      if (houses.exists(!_.isDefined)) {
        for {
          housesWithColor <- Color.update(houses) if check(housesWithColor)
          housesWithResident <- Resident.update(housesWithColor) if check(housesWithResident)
          housesWithAnimal <- Animal.update(housesWithResident) if check(housesWithAnimal)
          housesWithDrink <- Drink.update(housesWithAnimal) if check(housesWithDrink)
          housesWithSmoke <- Smoke.update(housesWithDrink) if check(housesWithSmoke)
        } {
          queue.enqueue(housesWithSmoke)
        }
      } else {
        if (check(houses)) {
          val waterDrinker = houses.find(_.drink.contains(Water)).flatMap(_.resident)
          val zebraOwner = houses.find(_.animal.contains(Zebra)).flatMap(_.resident)
          (waterDrinker zip zebraOwner).foreach { case (res1, res2) =>
            return Solution(res1, res2)
          }
        }
      }
    }

    null
  }
}

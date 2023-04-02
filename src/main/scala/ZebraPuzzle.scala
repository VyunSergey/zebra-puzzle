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
  import Color._
  import Resident._
  import Animal._
  import Drink._
  import Smoke._

  sealed trait Color
  object Color {
    case object Red    extends Color
    case object Green  extends Color
    case object Yellow extends Color
    case object Blue   extends Color
    case object Ivory  extends Color

    val all: List[Color] = List(Red, Green, Yellow, Blue, Ivory)
    def restOf(houses: List[House]): List[Color] = all diff houses.flatMap(_.color)
    def update(houses: List[House], conditions: List[House] => Boolean): Iterator[List[House]] = {
      val housesNoColor: List[House] = houses.filter(_.color.isEmpty)
      for {
        colors <- Color.restOf(houses).permutations
        housesUpd = (housesNoColor zip colors).map { case (house, color) =>
          house.copy(color = Some(color))
        } ++ houses.filter(_.color.nonEmpty) if conditions(housesUpd)
      } yield housesUpd
    }
  }

  sealed trait Resident
  object Resident {
    case object Englishman extends Resident
    case object Spaniard   extends Resident
    case object Ukrainian  extends Resident
    case object Norwegian  extends Resident
    case object Japanese   extends Resident

    val all: List[Resident] = List(Englishman, Spaniard, Ukrainian, Norwegian, Japanese)
    def restOf(houses: List[House]): List[Resident] = all diff houses.flatMap(_.resident)
    def update(houses: List[House], conditions: List[House] => Boolean): Iterator[List[House]] = {
      val housesNoResident: List[House] = houses.filter(_.resident.isEmpty)
      for {
        residents <- Resident.restOf(houses).permutations
        housesUpd = (housesNoResident zip residents).map { case (house, resident) =>
          house.copy(resident = Some(resident))
        } ++ houses.filter(_.resident.nonEmpty) if conditions(housesUpd)
      } yield housesUpd
    }
  }

  sealed trait Animal
  object Animal {
    case object Dog    extends Animal
    case object Snails extends Animal
    case object Fox    extends Animal
    case object Horse  extends Animal
    case object Zebra  extends Animal

    val all: List[Animal] = List(Dog, Snails, Fox, Horse, Zebra)
    def restOf(houses: List[House]): List[Animal] = all diff houses.flatMap(_.animal)
    def update(houses: List[House], conditions: List[House] => Boolean): Iterator[List[House]] = {
      val housesNoAnimal: List[House] = houses.filter(_.animal.isEmpty)
      for {
        animals <- Animal.restOf(houses).permutations
        housesUpd = (housesNoAnimal zip animals).map { case (house, animal) =>
          house.copy(animal = Some(animal))
        } ++ houses.filter(_.animal.nonEmpty) if conditions(housesUpd)
      } yield housesUpd
    }
  }

  sealed trait Drink
  object Drink {
    case object Coffee      extends Drink
    case object Tea         extends Drink
    case object Milk        extends Drink
    case object OrangeJuice extends Drink
    case object Water       extends Drink

    val all: List[Drink] = List(Coffee, Tea, Milk, OrangeJuice, Water)
    def restOf(houses: List[House]): List[Drink] = all diff houses.flatMap(_.drink)
    def update(houses: List[House], conditions: List[House] => Boolean): Iterator[List[House]] = {
      val housesNoDrink: List[House] = houses.filter(_.drink.isEmpty)
      for {
        drinks <- Drink.restOf(houses).permutations
        housesUpd = (housesNoDrink zip drinks).map { case (house, drink) =>
          house.copy(drink = Some(drink))
        } ++ houses.filter(_.drink.nonEmpty) if conditions(housesUpd)
      } yield housesUpd
    }
  }

  sealed trait Smoke
  object Smoke {
    case object TheOldGold    extends Smoke
    case object Kools         extends Smoke
    case object Chesterfields extends Smoke
    case object LuckyStrike   extends Smoke
    case object Parliaments   extends Smoke

    val all: List[Smoke] = List(TheOldGold, Kools, Chesterfields, LuckyStrike, Parliaments)
    def restOf(houses: List[House]): List[Smoke] = all diff houses.flatMap(_.smoke)
    def update(houses: List[House], conditions: List[House] => Boolean): Iterator[List[House]] = {
      val housesNoSmoke: List[House] = houses.filter(_.smoke.isEmpty)
      for {
        smokes <- Smoke.restOf(houses).permutations
        housesUpd = (housesNoSmoke zip smokes).map { case (house, smoke) =>
          house.copy(smoke = Some(smoke))
        } ++ houses.filter(_.smoke.nonEmpty) if conditions(housesUpd)
      } yield housesUpd
    }
  }

  final case class House(
                          num: Int,
                          color: Option[Color],
                          resident: Option[Resident],
                          animal: Option[Animal],
                          drink: Option[Drink],
                          smoke: Option[Smoke]
                        ) {
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
  object House {
    def empty(i: Int): House = House(i, None, None, None, None, None)
    def print(houses: List[House]): Unit = {
      houses.sortBy(_.num).foreach(println)
      println()
    }
  }

  final case class Solution(waterDrinker: Resident, zebraOwner: Resident)

  val conditionsSingleNum: PartialFunction[House, Boolean] = {
    // 9. Milk is drunk in the middle(2) house.
    case House(2, _, _, _, Some(drink), _)    => drink == Milk
    // 10. The Norwegian lives in the first house.
    case House(0, _, Some(resident), _, _, _) => resident == Norwegian
  }

  val conditionsSingleColor: PartialFunction[House, Boolean] = {
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

  val conditionsSingleResident: PartialFunction[House, Boolean] = {
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

  val conditionsSingleAnimal: PartialFunction[House, Boolean] = {
    // 3. The Spaniard owns the dog.
    case House(_, _, Some(resident), Some(Dog), _, _) => resident == Spaniard
    // 7. The Old Gold smoker owns snails.
    case House(_, _, _, Some(Snails), _, Some(smoke)) => smoke == TheOldGold
    // 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
    case House(_, _, _, Some(Fox), _, Some(smoke)) => smoke != Chesterfields
    // 12. Kools are smoked in the house next to the house where the horse is kept.
    case House(_, _, _, Some(Horse), _, Some(smoke)) => smoke != Kools
  }

  val conditionsSingleDrink: PartialFunction[House, Boolean] = {
    // 4. Coffee is drunk in the green house.
    case House(_, Some(color), _, _, Some(Coffee), _) => color == Green
    // 5. The Ukrainian drinks tea.
    case House(_, _, Some(resident), _, Some(Tea), _) => resident == Ukrainian
    // 9. Milk is drunk in the middle(2) house.
    case House(num, _, _, _, Some(Milk), _) => num == 2
    // 13. The Lucky Strike smoker drinks orange juice.
    case House(_, _, _, _, Some(OrangeJuice), Some(smoke)) => smoke == LuckyStrike
  }

  val conditionsSingleSmoke: PartialFunction[House, Boolean] = {
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

  val conditionsSingle: List[House => Boolean] =
    List(
      conditionsSingleNum,
      conditionsSingleColor,
      conditionsSingleResident,
      conditionsSingleAnimal,
      conditionsSingleDrink,
      conditionsSingleSmoke
    ).map(pf => (h: House) => pf.applyOrElse(h, (_: House) => true))

  val unfilledConditions: House => Boolean = (h: House) =>
    conditionsSingle.view.foldLeft(false){ case (cond, f) => cond || !f(h) }

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

  val conditionsBoth: List[(House, House) => Boolean] =
    List(
      conditionsBothColor,
      conditionsBothResident,
      conditionsBothAnimal,
      conditionsBothSmoke
    ).map(pf => (h1: House, h2: House) => pf.applyOrElse((h1, h2), (_: (House, House)) => true))

  val unfilledConditionsBoth: (House, House) => Boolean = (h1: House, h2: House) =>
    conditionsBoth.view.foldLeft(false){ case (cond, f) => cond || !f(h1, h2) }

  lazy val setup: List[List[House]] = {
    // 1. There are five houses.
    val houses = mutable.ListBuffer.tabulate(5)(i => House.empty(i))

    // 10. The Norwegian lives in the first house.
    houses(0) = houses.head.copy(resident = Some(Norwegian))
    // 15. The Norwegian lives next to the blue house.
    houses(1) = houses(1).copy(color = Some(Blue))
    // 9. Milk is drunk in the middle house.
    houses(2) = houses(2).copy(drink = Some(Milk))

    List(houses.toList)
  }

  def conditions(houses: List[House]): Boolean = {
    for (h <- houses) if (unfilledConditions(h)) return false
    for (Seq(h1, h2) <- houses.combinations(2).toSeq if unfilledConditionsBoth(h1, h2)) return false
    true
  }

  def solve: Option[Solution] = {
    val queue = mutable.Queue.empty[List[House]]
    val housesInitial: List[List[House]] = setup

    queue ++= housesInitial
    while(queue.nonEmpty) {
      println(queue.size)
      val houses = queue.dequeue

      println("---------CURRENT HOUSES-------------------")
      House.print(houses)
      println("------------------------------------------")

      if (houses.exists(!_.isDefined)) {
        for {
          housesWithColor    <- {val list = Color.update(houses, conditions).toList; println(s"with Colors len=${list.length}"); list}
          housesWithDrink    <- {val list = Drink.update(housesWithColor, conditions).toList; println(s"with Drink len=${list.length}"); list}
          housesWithResident <- {val list = Resident.update(housesWithDrink, conditions).toList; println(s"with Resident len=${list.length}"); list}
          housesWithAnimal   <- {val list = Animal.update(housesWithResident, conditions).toList; println(s"with Animal len=${list.length}"); list}
          housesWithSmoke    <- {val list = Smoke.update(housesWithAnimal, conditions).toList; println(s"with Smoke len=${list.length}"); list}
          _                   = House.print(housesWithSmoke)
        } queue.enqueue(housesWithSmoke)
      } else {
        if (conditions(houses)) {
          println("---------ANSWER HOUSES-------------------")
          House.print(houses)
          println("-----------------------------------------")

          val waterDrinker = houses.find(_.drink.contains(Water)).flatMap(_.resident)
          val zebraOwner = houses.find(_.animal.contains(Zebra)).flatMap(_.resident)
          (waterDrinker zip zebraOwner).foreach { case (res1, res2) =>
            return Some(Solution(res1, res2))
          }
        }
      }
    }

    None
  }
}

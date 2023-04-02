import org.scalatest.{FunSuite, Matchers}

import ZebraPuzzle._

/** @version created manually **/
class ZebraPuzzleTest extends FunSuite with Matchers {
  test("solve the Zebra Puzzle") {
    ZebraPuzzle.solve should be (Some(Solution(waterDrinker = Resident.Norwegian, zebraOwner = Resident.Japanese)))
  }
}

import org.scalatest.FunSuite

class MyCellTest extends FunSuite {
  test("MyCell.toString") {
    var cell = MyCell("A", 1, "5", true)
    assert(cell.toString === "A1 = 5")
    cell = MyCell("B", 54, "[.A9]+IF(14;'hi';true)", true)
    assert(cell.toString === "B54 = [.A9]+IF(14;'hi';true)")
  }
  test("MyCell.equals") {
    val cell = MyCell("AA", 1, "5", true)
    val equalCell = MyCell("AA", 1, "5", true)
    val otherCell  = MyCell("Z", 54, "[.A9]+IF(14;'hi';true)", true)
    assert(cell.equals(equalCell))
    assert(equalCell.equals(cell))
    assert(!cell.equals(otherCell))
    assert(!otherCell.equals(cell))
    assert(!equalCell.equals(otherCell))
  }

}

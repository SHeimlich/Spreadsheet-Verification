import org.scalatest.FunSuite

class MyCellTest extends FunSuite {
    test("MyCell.toString") {
        var cell = MyCell('A', 1, "5", true)
        assert(cell.toString === "A1 = 5")
        cell = MyCell('B', 54, "[.A9]+IF(14;'hi';true)", true)
        assert(cell.toString === "B54 = [.A9]+IF(14;'hi';true)")
    }
    test("MyCell.equals") {
        val cell = MyCell('A', 1, "5", true)
        val equalCell = MyCell('A', 1, "5", true)
        val otherCell  = MyCell('B', 54, "[.A9]+IF(14;'hi';true)", true)
        assert(cell.equals(equalCell))
        assert(equalCell.equals(cell))
        assert(!cell.equals(otherCell))
        assert(!otherCell.equals(cell))
        assert(!equalCell.equals(otherCell))
    }

    test("OuterParser.addTest") {
        val str = OuterParser.parse("addTest.ods");
        val expected = "A2 = 6, A1 = 5, A3 = [.A1]+[.A2],"
        assert(str === expected)
    }
    test("OuterParser.emptyCellTest") {
        val str = OuterParser.parse("emptyCellTest.ods");
        val expected = "A1 = 5, B2 = 6, C5 = [.A1]+[.B2],"
        assert(str === expected)
    }
    test("OuterParser.ifTest") {
        val str = OuterParser.parse("ifTest.ods");
        val expected = "A1 = 5, B2 = 45, B1 = 6, C1 = IF([.A1]; [.B1]; [.B2]),"
        assert(str === expected)
    }
    test("OuterParser.simpleArgTest") {
        val str = OuterParser.parse("simpleArgTest.ods");
        val expected = "A3 = 3, A8 = 8, A4 = 4, A1 = 1, A7 = 7, A5 = 5, A6 = 6, A2 = 2, B1 = SUM([.A1]; [.A2]; [.A3]),"
        assert(str === expected)
    }
    test("OuterParser.embededArgTest") {
        val str = OuterParser.parse("embededArgTest.ods");
        val expected = "A3 = 3, A8 = 8, A4 = 4, A1 = 1, A7 = 7, A5 = 5, A9 = 9, A6 = 6, A2 = 2, B1 = SUM([.A1]; IF([.A2];[.A3];[.A4]); SUM([.A5]; [.A6]; IF([.A7]; [.A8]; [.A9]))),"
        assert(str === expected)
    }
}

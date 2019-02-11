import org.scalatest.FunSuite

class OuterParserTest extends FunSuite {

  test("OuterParser.addTest") {
    val spreadSheetParser = new OuterParser()
    val str = spreadSheetParser.parse("addTest.ods");
    val expected = "A2 = 6, A1 = 5, A3 = [.A1]+[.A2],"
    assert(str === expected)
  }
  test("OuterParser.emptyCellTest") {
    val spreadSheetParser = new OuterParser()

    val str = spreadSheetParser.parse("emptyCellTest.ods");
    val expected = "A1 = 5, B2 = 6, C5 = [.A1]+[.B2],"
    assert(str === expected)
  }
  test("OuterParser.ifTest") {
    val spreadSheetParser = new OuterParser()

    val str = spreadSheetParser.parse("ifTest.ods");
    val expected = "A1 = 5, B2 = 45, B1 = 6, C1 = IF([.A1]; [.B1]; [.B2]),"
    assert(str === expected)
  }
  test("OuterParser.simpleArgTest") {
    val spreadSheetParser = new OuterParser()

    val str = spreadSheetParser.parse("simpleArgTest.ods");
    val expected = "A3 = 3, A8 = 8, A4 = 4, A1 = 1, A7 = 7, A5 = 5, A6 = 6, A2 = 2, B1 = SUM([.A1]; [.A2]; [.A3]),"
    assert(str === expected)
  }
  test("OuterParser.embededArgTest") {
    val spreadSheetParser = new OuterParser()

    val str = spreadSheetParser.parse("embededArgTest.ods");
    val expected = "A3 = 3, A8 = 8, A4 = 4, A1 = 1, A7 = 7, A5 = 5, A9 = 9, A6 = 6, A2 = 2, B1 = SUM([.A1]; IF([.A2];[.A3];[.A4]); SUM([.A5]; [.A6]; IF([.A7]; [.A8]; [.A9]))),"
    assert(str === expected)
  }
  test("OuterParser.nullCellTest") {
    val spreadSheetParser = new OuterParser()

    val str = spreadSheetParser.parse("emptyCellAsArgTest.ods");
    val expected = "C2 = null, C1 = 1, A1 = [.C1]+[.C2],"
    assert(str === expected)
  }

}

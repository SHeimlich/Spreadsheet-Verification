import java.io.{File, PrintWriter}

import org.scalatest.FunSuite
import org.jopendocument.dom.spreadsheet.{Sheet, SpreadSheet}


class ParsingTest extends FunSuite {

  test("testGetRowNum") {
    val sheet = SpreadSheet.createFromFile(new File("addTest.ods")).getSheet(0)
    var p = new Parsing(sheet);
    assert(p.getRowNum("A") === 0)
    assert(p.getRowNum("B") === 1)
    assert(p.getRowNum("Z") === 25)
    assert(p.getRowNum("AA") === 26)
    assert(p.getRowNum("AB") === 27)
    assert(p.getRowNum("AZ") === 51)
    assert(p.getRowNum("AAA") === 52)
  }

  test("testGetRowString") {
    val sheet = SpreadSheet.createFromFile(new File("addTest.ods")).getSheet(0)
    var p = new Parsing(sheet);
    assert(p.getRowString(0) === "A")
    assert(p.getRowString(1) === "B")
    assert(p.getRowString(25) === "Z")
    assert(p.getRowString(26) === "AA")
    assert(p.getRowString(27) === "AB")
    assert(p.getRowString(51) === "AZ")
    assert(p.getRowString(52) === "AAA")
  }

  test("testGetLocationName") {
    val sheet = SpreadSheet.createFromFile(new File("addTest.ods")).getSheet(0)
    var p = new Parsing(sheet);
    assert(p.getLocationName(0, 0) === "A1")
    assert(p.getLocationName(1, 56) === "B57")
    assert(p.getLocationName(25,100) === "Z101")
    assert(p.getLocationName(26, 2) === "AA3")
    assert(p.getLocationName(27, 5) === "AB6")
  }

}

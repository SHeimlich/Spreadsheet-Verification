import java.awt.Point
import java.util.regex.Pattern

import scalax.collection.Graph
import scalax.collection.edge.LDiEdge
import org.jopendocument.dom.spreadsheet.{MutableCell, Sheet, SpreadSheet}


class Parsing (){

  val constCell = MyCell("0", -1, "null", false)
  val randomCell = MyCell("0", -2, "null", false)
  val trueCondition = Condition("true")
  var dummyEdge = LDiEdge(randomCell, constCell)(trueCondition)
  var g = Graph(dummyEdge)//(a1a2)

  var start = new Point(0,0)
  var end = new Point(0,0)
  var rows = 0
  var cols = 0

  def buildGraph(s:Sheet): Unit = {
    start = s.getUsedRange.getStartPoint
    end = s.getUsedRange.getEndPoint
    rows = end.x - start.x;
    cols = end.y - start.y;
    //def Parsing(s: Sheet): Unit = {


    var reached = Array.ofDim[Boolean](rows + 1, cols + 1);
    for( r <- 1 to rows) {
      for( c <- 1 to cols) {
        reached(r)(c) = false;
      }
    }
    var r = start.y
    while (r <= end.y) {
      var c = start.x
      while (c <= end.x) {
        val cell = s.getCellAt(getLocationName(c, r));
        if(cell.getFormula != null || cell.getValue != "") {
          parseCell(s, c, r, reached)
        }
        c += 1;
      }
      r += 1;
    }
  }

  def parseCell(sheet: Sheet, row: Int, col: Int, reached: Array[Array[Boolean]]): MyCell = {
    val outsideRange = row - start.x >= reached.length | col - start.y >= reached(row - start.x).length
    if(outsideRange) {
      return new MyCell(getRowString(row), getColNum(col), "null", true);
    }

    if (!reached(row - start.x)(col - start.y)) {

      reached(row - start.x)(col - start.y) = true;
      val cell = sheet.getCellAt(getLocationName(row, col));
      val formula = getParsedFormula(cell);

      if (formula == null) { // Simple case, no formula

        val newCell = getMyCell(cell, getColNum(col), getRowString(row));
        val edge = LDiEdge(newCell, constCell)(trueCondition);
        g = g + edge;
        return newCell;

      }

      else { // There is a forumla, so we need to check for dependencies.
        val newCell = getMyCell(cell, getColNum(col), getRowString(row));

        // TODO: Deal with Arrays
        val cellRegex = "[A-Z]+[1-9][0-9]*";
        val arrayPattern = Pattern.compile("\\[\\." + cellRegex + ":\\." + cellRegex + "\\]");
        val arrayMatcher = arrayPattern.matcher(formula);
        while(arrayMatcher.find()) {
          println("There is an array here!");
          val s = arrayMatcher.group(0)

         // val startRow = s.charAt(2) - 'A';
         // val startCol = Integer.parseInt(s.substring(1, s.length())) - 1;

        }

        // Deal with Dependencies
        val pattern = Pattern.compile(cellRegex);
        val m = pattern.matcher(formula);
        while (m.find()) {
          val s = m.group(0);
          val r = Pattern.compile("[A-Z]+").matcher(s);
          r.find()
          val rStr = r.group(0)
          val depRow = getRowNum(rStr)
          val depCol = Integer.parseInt(s.substring(rStr.length, s.length())) - 1;
          val childCell = parseCell(sheet, depRow, depCol, reached);
          val edge = LDiEdge(childCell, newCell)(trueCondition);
          g = g + edge;
        }
        val hasCell = g find (g having (node = _ equals newCell))
        if(hasCell == None) {
          val edge = LDiEdge(constCell, newCell)(trueCondition);
          g = g + edge;
        }

      } // End if Formula
    } // End If reached
    return getMyCell(sheet.getCellAt(getLocationName(row, col)), getColNum(col), getRowString(row));
  }

  def getParsedFormula(cell: MutableCell[SpreadSheet]) : String = {
    val formula = cell.getFormula;
    if (formula == null) {
      return null
    }
    return formula.substring(4)
  }

  def getMyCell(cell: MutableCell[SpreadSheet], col: Int, row: String): MyCell = {
    val formula = getParsedFormula(cell)
    if (formula != null) {
      return MyCell(row, col, formula, false);
    }
    return MyCell(row, col, cell.getValue.toString, false);
  }

  def getLocationName(row: Int, col: Int): String = {
    getRowString(row) + "" + getColNum(col).toString;
  }

  def getGraph(): Graph[MyCell, LDiEdge] = return (g - randomCell - constCell);

  def getRowString(row: Int) : String = {
    var rtn = ""
    var r : Double = row;
    while(r > 25) {
      val c = (r % 26) + 'A'
      rtn = c.toChar.toString.concat(rtn)
      r = r - 26 - (r % 26)
    }
    val c = (r % 26) + 'A'
    rtn = c.toChar.toString.concat(rtn)
    return rtn
  }

  def getColNum(col: Int): Int = {
    return col + 1
  }

  def getRowNum(row: String) : Int = {
    var rtn = 0;
    for(i <- 0 until row.length) {
      var cur = row.charAt(i) - 'A';
      rtn = rtn + cur
      if(i + 1 < row.length) {
        rtn = rtn + 26
      }
    }
    return rtn
  }

}

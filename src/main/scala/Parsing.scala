import java.util.regex.Pattern

import scalax.collection.Graph
import scalax.collection.edge.LDiEdge
import org.jopendocument.dom.spreadsheet.{MutableCell, Sheet, SpreadSheet}


class Parsing (s:Sheet){

  val constCell = MyCell("0", -1, "null", false)
  val randomCell = MyCell("0", -2, "null", false)
  val trueCondition = Condition("true")
  var dummyEdge = LDiEdge(randomCell, constCell)(trueCondition)
  var g = Graph(dummyEdge)//(a1a2)

  var sheet = s;

  //def Parsing(s: Sheet): Unit = {
  var start = sheet.getUsedRange.getStartPoint
  var end = sheet.getUsedRange.getEndPoint
  var rows = end.x - start.x;
  var cols = end.y - start.y;

  var reached = Array.ofDim[Boolean](rows + 1, cols + 1);
  for( r <- 1 to rows) {
    for( c <- 1 to cols) {
      reached(r)(c) = false;
    }
  }

  def buildGraph(s:Sheet): Unit = {
    var r = start.y
    while (r <= end.y) {
      var c = start.x
      while (c <= end.x) {
        parseCell(c, r)
        c += 1;
      }
      r += 1;
    }
  }

  def parseCell(row: Int, col: Int): MyCell = {
    var sBuild = "";
    if(row - start.x >= reached.length | col - start.y >= reached(row - start.x).length) {
      return null;
    }

    if (!reached(row - start.x)(col - start.y)) {
      reached(row - start.x)(col - start.y) = true;
      val cell = sheet.getCellAt(getLocationName(row, col));
      val formula = getParsedFormula(cell);

      if (formula == null) { // Simple case, no formula
        if(cell.getValue == "") {
          return null // The cell is empty.
        }
        val newCell = getMyCell(cell, getColNum(col), getRowString(row));
        val edge = LDiEdge(newCell, constCell)(trueCondition);
        g = g + edge;
        return newCell;

      } else { // There is a forumla, so we need to check for dependencies.
        val newCell = getMyCell(cell, getColNum(col), getRowString(row));

        // TODO: Deal with Arrays
        //TODO: Deal with two letter rows
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
          println(s)
          val r = Pattern.compile("[A-Z]+").matcher(s);
          r.find()
          val rStr = r.group(0)
          val depRow = getRowNum(rStr)
          val depCol = Integer.parseInt(s.substring(rStr.length, s.length())) - 1;
          val childCell = parseCell(depRow, depCol);
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
      println("r = " + r)
      val c = (r % 26) + 'A'
      println("c = " + c.toChar)
      rtn = c.toChar.toString.concat(rtn)
      r = r - 26 - (r % 26)
    }
    println("r = " + r)
    val c = (r % 26) + 'A'
    println("c = " + c.toChar)
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

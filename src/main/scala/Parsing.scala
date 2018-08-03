import java.util.regex.Pattern

import scalax.collection.Graph
import scalax.collection.edge.LDiEdge
import org.jopendocument.dom.spreadsheet.{MutableCell, Sheet, SpreadSheet}


class Parsing (s:Sheet){

  val constCell = MyCell('0', -1, "null", false)
  val randomCell = MyCell('0', -2, "null", false)
  val trueCondition = Condition("true")
  var dummyEdge = LDiEdge(randomCell, constCell)(trueCondition)
  var g = Graph(dummyEdge)//(a1a2)

  var sheet = s;

  //def Parsing(s: Sheet): Unit = {
  var start = sheet.getUsedRange.getStartPoint
  var end = sheet.getUsedRange.getEndPoint
  var rows = end.x - start.x;
  var cols = end.y - start.y;

  println("Start: " + start + " end: " + end);
  println("Rows: " + rows + " Cols: " + cols);

  var reached = Array.ofDim[Boolean](rows + 1, cols + 1);
  for( r <- 1 to rows) {
    for( c <- 1 to cols) {
      reached(r)(c) = false;
    }
  }

  def parseCell(row: Int, col: Int): String = {
    var sBuild = "";
    if(row - start.x >= reached.length | col - start.y >= reached(row - start.x).length) {
      return ""//getString(sheet.getCellAt(getLocationName(row, col)), getLocationName(row, col));
    }

    if (!reached(row - start.x)(col - start.y)) {

      val cell = sheet.getCellAt(getLocationName(row, col));
      println(cell)
      val formula = getParsedFormula(cell);

      if (formula != null) {
        val newCell = getMyCell(cell, getColNum(col), getRowChar(row));


        // Deal with Arrays
        val cellRegex = "[A-Z]+[1-9][0-9]*";
        val arrayPattern = Pattern.compile("\\[\\." + cellRegex + ":\\." + cellRegex + "\\]");
        val arrayMatcher = arrayPattern.matcher(formula);
        if(arrayMatcher.find()) {
          println("There is an array here!");
        }

        // Deal with Dependencies
        val pattern = Pattern.compile(cellRegex);
        val matcher = pattern.matcher(formula);
        while (matcher.find()) {
          val s = matcher.group(0);
          val depRow = s.charAt(0) - 'A';
          val depCol = Integer.parseInt(s.substring(1, s.length())) - 1;
          //          sBuild = sBuild.concat(parseCell(depRow, depCol));

          val childCell = getMyCell(sheet.getCellAt(getLocationName(depRow, depCol)), getColNum(depCol), getRowChar(depRow))
          val edge = LDiEdge(childCell, newCell)(trueCondition);
          g = g + edge;

        }
        val hasCell = g find (g having (node = _ equals newCell))
        if(hasCell == None) {
          val edge = LDiEdge(constCell, newCell)(trueCondition);
          g = g + edge;
        }

      } else {
        val newCell = getMyCell(cell, getColNum(col), getRowChar(row));
        val edge = LDiEdge(newCell, constCell)(trueCondition);
        g = g + edge;
      }
      reached(row - start.x)(col - start.y) = true;

      return sBuild //+ getString(cell, getLocationName(row, col));
    }
    reached(row - start.x)(col - start.y) = true;

    return sBuild + "";
  }

  def getParsedFormula(cell: MutableCell[SpreadSheet]) : String = {
    val formula = cell.getFormula;
    if (formula == null) {
      return null
    }
    return formula.substring(4)
  }

  def getMyCell(cell: MutableCell[SpreadSheet], col: Int, row: Char): MyCell = {
    val formula = getParsedFormula(cell)
    if (formula != null) {
      return MyCell(row, col, formula, false);
    }
    return MyCell(row, col, cell.getValue.toString, false);
  }

  def getLocationName(row: Int, col: Int): String = {
    getRowChar(row) + "" + getColNum(col).toString;
  }

  def getGraph(): Graph[MyCell, LDiEdge] = return (g - randomCell - constCell);

  def getRowChar(row: Int) : Char = {
    return (row + 'A').toChar
  }

  def getColNum(col: Int): Int = {
    return col + 1
  }

}

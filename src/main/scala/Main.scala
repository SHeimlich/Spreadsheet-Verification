//import org.jopendocument.dom.spreadsheet.Sheet
//import org.jopendocument.dom.spreadsheet.SpreadSheet
import java.io.{File, FileReader, IOException, PrintWriter}

import org.jopendocument.dom.spreadsheet.{Sheet, SpreadSheet}
import org.bitbucket.inkytonik.kiama.util.{CompilerBase, Config}
import syntax.ExpParser
import syntax.ExpParserSyntax.Exp


object Main {

  var sheet: Sheet = null

  def main(args: Array[String]): Unit = { // Load the file.
    val file = new File("simpleAddition.ods")
    try {
      val writer = new PrintWriter(new File("file.exp"))
      sheet = SpreadSheet.createFromFile(file).getSheet(0)
      val start = sheet.getUsedRange.getStartPoint
      val end = sheet.getUsedRange.getEndPoint
      val p = new Parsing(sheet)
      var r = start.y
      while (r <= end.y) {
        var c = start.x
        while (c <= end.x) {
          val line = p.parseCell(c, r)
          c += 1;
        }
        r += 1;
      }



      val g = p getGraph;
      println(g.head)
      println(g.topologicalSort().toString)
      println("cycles: " + g.findCycle)
      var str = g.componentTraverser().topologicalSortByComponent.toString()
      str = str.substring(27, str.length-3)
      println("str = " + str)
      writer.write(str)
      writer.close()
      System.out.println("Succesfully output to File")

      val reader = new FileReader("input.exp");
      val par = new ExpParser(reader, "input.exp")
      //val p = new Parser();

      val pr = par.pExp(0);



      println("pr = " + pr)

      if(pr.parseError() != null) {
        println("In thing!")
        println(par.formatParseError(pr.parseError(), true));
      }
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }


}

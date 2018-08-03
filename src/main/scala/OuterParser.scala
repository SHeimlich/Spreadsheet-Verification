import java.io.{File, IOException, PrintWriter}

import org.jopendocument.dom.spreadsheet.SpreadSheet

class OuterParser {

  def parse = {
    val file = new File("simpleAddition.ods")
    try {
      val writer = new PrintWriter(new File("file.exp"))
      val sheet = SpreadSheet.createFromFile(file).getSheet(0)
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
      str = str.substring(27, str.length - 3)
      println("str = " + str)
      writer.write(str)
      writer.close()
      System.out.println("Succesfully output to File")
    }
    catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }
}

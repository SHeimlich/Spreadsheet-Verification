import java.io.{File, IOException, PrintWriter}

import org.jopendocument.dom.spreadsheet.SpreadSheet

object OuterParser {

  def parse() = {
    val file = new File("simpleAddition.ods")
    try {
      val writer = new PrintWriter(new File("file.exp"))
      val sheet = SpreadSheet.createFromFile(file).getSheet(0)
      val start = sheet.getUsedRange.getStartPoint
      val end = sheet.getUsedRange.getEndPoint
      val p = new Parsing(sheet)
      p.buildGraph(sheet);


      val g = p getGraph;
      println("cycles: " + g.findCycle)
      var str = g.topologicalSort().toString()
      str = str.substring(23, str.length - 2) + ","
      writer.write(str)
      writer.close()
    }
    catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }
}

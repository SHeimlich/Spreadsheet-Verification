import java.io.{File, IOException, PrintWriter}

import org.jopendocument.dom.spreadsheet.SpreadSheet

class OuterParser {

  def parse(fileName: String) : String = {
    val file = new File(fileName + ".ods")
    try {
      val writer = new PrintWriter(new File(fileName + ".exp"))
      val sheet = SpreadSheet.createFromFile(file).getSheet(0)
      val start = sheet.getUsedRange.getStartPoint
      val end = sheet.getUsedRange.getEndPoint
      val p = new Parsing()
      p.buildGraph(sheet);


      val g = p getGraph;
      println("cycles: " + g.findCycle)
      var str = g.topologicalSort().toString()
      str = str.substring(23, str.length - 2) + ","
      writer.write(str)
      writer.close()
      return str
    }
    catch {
      case e: IOException =>
        e.printStackTrace()
        return ""
    }
  }
}

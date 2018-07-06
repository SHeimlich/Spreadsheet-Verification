//import org.jopendocument.dom.spreadsheet.Sheet
//import org.jopendocument.dom.spreadsheet.SpreadSheet
import java.io.File
import java.io.IOException
import java.io.PrintWriter

import org.jopendocument.dom.spreadsheet.{Sheet, SpreadSheet}


object Main {
  var sheet: Sheet = null

  def main(args: Array[String]): Unit = { // Load the file.
    val file = new File("simpleAddition.ods")
    try {
      val writer = new PrintWriter(new File("simpleAddition.c"))
      var cCode = "extern void __VERIFIER_error() __attribute__ ((__noreturn__));" + "\n" + "extern float __VERIFIER_nondet_float();" + "\n" + "int main()\n" + "{"
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
      cCode = cCode.concat("}")
      writer.write(cCode)
      writer.close()
      System.out.println("Succesfully output to C File")

      val g = p getGraph;
      println(g.head)
      println(g.topologicalSort().toString)
      println("cycles: " + g.findCycle)
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }
}

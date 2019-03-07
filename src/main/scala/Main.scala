import java.io.{File, IOException, PrintWriter}

import org.bitbucket.inkytonik.kiama.util.{CompilerBase, Config}
import syntax.ExpParserSyntax.Exp
import Evaluator.expvalue
import org.jopendocument.dom.spreadsheet.SpreadSheet


object ruMain extends CompilerBase[Exp,Config] {

  import java.io.Reader
  //import Optimiser.optimise
  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
  import org.bitbucket.inkytonik.kiama.util.Source
  import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
  import syntax.ExpParser
  import syntax.ExpParserPrettyPrinter
  import syntax.ExpParserPrettyPrinter.{any, layout}

  override def main(args: Array[String]): Unit = {
    val spreadSheetParser = new OuterParser()
    spreadSheetParser.parse("divideWithIf.ods")
    super.main( Array("file.exp"))
  }

  def createConfig(args : Seq[String]) : Config = {
    val c = new Config(args)
    return c;
  }

  override def makeast (source : Source, config : Config) : Either[Exp,Messages] = {
    val p = new ExpParser (source, positions)
    val pr = p.pExp (0)
    if (pr.hasValue)
      Left (p.value (pr).asInstanceOf[Exp])
    else
      Right (Vector (p.errorToMessage (pr.parseError)))
  }

  def process (source : Source, e : Exp, config : Config) {
    val output = config.output()
    //output.emitln ("e = " + e)
    //output.emitln ("e tree:")
    output.emitln (layout (any (e)))
    //output.emitln(test(any(e)))
    //output.emitln ("value (e) = \n" + expvalue (e))
    val opt = new Optimiser();
    val o = opt.optimise (e)
    output.emitln ("e optimised = " + layout( any (o)))
    output.emitln ("value (e optimised) = " + expvalue (o))


    val file = new File("file.c")
    try {
      val writer = new PrintWriter(file)
      val str = "extern void __VERIFIER_error() __attribute__ ((__noreturn__));\n" +
        "unsigned int __VERIFIER_nondet_uint();\n" +
        "int main() {\n" + expvalue (o) + "\n}"
      writer.write(str)
      writer.close()
    }
    catch {
      case e: IOException =>
        e.printStackTrace()
        return ""
    }

  }

  def test(a: String) : Int =
  {
    return 0
  }

  override def format (ast : Exp) : Document =
    ExpParserPrettyPrinter.format (ast, 5)

}
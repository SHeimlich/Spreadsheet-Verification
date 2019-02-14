import org.bitbucket.inkytonik.kiama.util.{CompilerBase, Config}
import syntax.ExpParserSyntax.{Exp}
import Evaluator.expvalue


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
    spreadSheetParser.parse("addTest.ods")
    super.main( Array("ifTest.exp"))
  }

  def createConfig(args : Seq[String]) : Config = {
    val c = new Config(args)
    args.foreach(println)
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
    output.emitln ("e = " + e)
    output.emitln ("e tree:")
    output.emitln (layout (any (e)))
    //output.emitln(test(any(e)))
    output.emitln ("value (e) = \n" + expvalue (e))
    val opt = new Optimiser();
    val o = opt.optimise (e)
    output.emitln ("e optimised = " + layout( any (o)))
    //output.emitln ("value (e optimised) = " + expvalue (o))

  }

  def test(a: String) : Int =
  {
    return 0
  }

  override def format (ast : Exp) : Document =
    ExpParserPrettyPrinter.format (ast, 5)

}
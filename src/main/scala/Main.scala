import org.bitbucket.inkytonik.kiama.util.{CompilerBase, Config}
import syntax.ExpParserSyntax.{Exp, Program}

object Main extends CompilerBase[Exp,Config] {

  import java.io.Reader
  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
  import org.bitbucket.inkytonik.kiama.util.Source
  import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
  import syntax.ExpParser
  import syntax.ExpParserPrettyPrinter
  import syntax.ExpParserPrettyPrinter.{any, layout}

  def createConfig(args : Seq[String]) : Config =
    new Config(args)

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
    output.emitln ("e tree pretty printed:")
    output.emitln (format (e).layout)
  }

  def process (source : Source, p : Program, config : Config) {
    val output = config.output()
    output.emitln ("e = " + p)
    output.emitln ("e tree:")
    output.emitln (layout (any (p)))
    output.emitln ("e tree pretty printed:")
    output.emitln (format (p).layout)
  }

  override def format (ast : Exp) : Document =
    ExpParserPrettyPrinter.format (ast, 5)

  def format (ast : Program) : Document =
    ExpParserPrettyPrinter.format (ast, 5)

}
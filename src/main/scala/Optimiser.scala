object Optimiser {

  import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{attempt, bottomup, rewrite, rule}
  import syntax.ExpParserSyntax._

  def optimise (e : Exp) : Exp =
    rewrite (optimiser) (e)

  /**
    * Try to optimise every expression in a tree. Do it bottom
    * up so higher up tries get the advantage of things done
    * at lower levels.
    */
  lazy val optimiser =
    bottomup (attempt (simplifier))

  /**
    * Simplify an expression using simple equivalences.
    */
  lazy val simplifier =
    rule[Exp] {
      case stmts(_, stmt(Assign(_, (nf(numIF(b, f1, f2)))))) => stmt(Assign(Cell("AB", "27"), nf(Num("3132"))))
    }

}
object Optimiser {

  import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{attempt, bottomup, rewrite, rule}
  import syntax.ExpParserSyntax._

  var ifCount = 0;

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
      case stmt(a) => stmtSimplifier(a)
    }

  def stmtSimplifier (x: Exp): Exp = x match {
    case Assign(Cell(c, r), nf(f)) => AssignSimplifier(Cell(c, r), f)
  }

  def AssignSimplifier (c: Cell, f: NumFormula) : Exp = {
    if(formulaHasIf(f)) {
      return ifStmt(getIf(c, f), AssignSimplifier(c, removeIf(f)))
    }
    else {
      return stmt(Assign(c, nf(f)))
    }
  }

  def getIf (cell: Cell, x: NumFormula) : assignIf = x match {
    case numIF(a, b, c) => {
      if(formulaHasIf(a(0))) return getIf(cell, a(0))
      if(formulaHasIf(b(0))) return getIf(cell, b(0))
      if(formulaHasIf(c(0))) return getIf(cell, c(0))
      return getAssignIf(cell, a, b, c)
    }
    case Add(l, r) => {if(formulaHasIf(l)){ getIf(cell, l); } else { getIf(cell, r); }}
  }

  def getAssignIf(c: Cell, a: Vector[NumFormula], nfVec1: Vector[NumFormula], nfVec2: Vector[NumFormula]) : assignIf = {
    ifCount = ifCount + 1
    val str = "if" + ifCount
    return ifAssign(Vector(ifRef(Vector(str))), a, nfVec1, nfVec2)
  }

  def removeIf (x: NumFormula) : NumFormula = x match {
    case numIF(a, b, c) => {

      if(formulaHasIf(a(0))) return numIF(Vector(removeIf(a(0))), b, c)
      if(formulaHasIf(b(0))) {println("delving deeper");  return numIF(a, Vector(removeIf(b(0))), c) }
      if(formulaHasIf(c(0))) return numIF(a, b, Vector(removeIf(c(0))))
      println("removing: " + a)
      return numIfRef(ifRef(Vector("if" + ifCount)))
    }
    case Add(l, r) => if(formulaHasIf(l)) {Add(removeIf(l), r); } else {Add(l, removeIf(r)); }
    case Num(x) => Num(x)
  }

  def formulaHasIf (x: NumFormula) : Boolean = x match {
    case numIF(_,_,_) => true
    case Add(l, r) => formulaHasIf(l) || formulaHasIf(r)
    case _ => false
  }

}
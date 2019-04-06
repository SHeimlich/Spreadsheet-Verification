import Evaluator.{FormAsserts, FormValue, getArgAsserts, getArgsLength, numVecAsserts}

class Optimiser {

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
      case stmts(a, b) => stmts(stmtSimplifier(a), stmtSimplifier(b))
//      case stmt(a) => stmtSimplifier(a)
    }

  def stmtSimplifier (x: Exp): Exp = x match {
    case Assign(Cell(c, r), f) => AssignSimplifier(Cell(c, r), f)
    case Assign(a, b) => Assign(a, b)
    //case stmt(a) => stmt(stmtSimplifier(a))
    case stmts(a, b) => stmts(stmtSimplifier(a), stmtSimplifier(b))
    case ifStmt(a, b) => ifStmt(a, stmtSimplifier(b))
  }

  def AssignSimplifier (c: Cell, f: Formula) : Exp = {
    if(formulaHasIf(f)) {
      return ifStmt(getIf(c, f), AssignSimplifier(c, removeIf(f)))
    }
    else {
      return Assign(c, f)
    }
  }

  def getIfArgs(cell: Cell, a: Vector[NumArguments]): assignIf = {
    for (i <- 0 until a.length) {
      if(argHasIf(a(i))) return getIfArg(cell, a(i))
    }
    return null
  }

  def getIfArg(cell: Cell, a: NumArguments) : assignIf = a match {
    case Args(l, r) => {
      if(formulaHasIf(l)) return getIf(cell, l)
      return getIfArg(cell, r)
    }
    case Arg(r) => getIf(cell, r)
  }

  def getIf(cell: Cell, x: Formula) : assignIf = x match {
    case numIF(nIf(a, b, c)) => {
      if(formulaHasIf(a(0))) return getIf(cell, a(0))
      if(formulaHasIf(b(0))) return getIf(cell, b(0))
      if(formulaHasIf(c(0))) return getIf(cell, c(0))
      return getAssignIf(cell, a, b, c)
    }
    case Add(l, r) => {if(formulaHasIf(l)){ getIf(cell, l); } else { getIf(cell, r); }}
    case SUM(a) => getIfArgs(cell, a)
    case Div(l, r) => {if(formulaHasIf(l)){ getIf(cell, l); } else { getIf(cell, r); }}
  }

  def getAssignIf(c: Cell, a: Vector[Formula], nfVec1: Vector[Formula], nfVec2: Vector[Formula]) : assignIf = {
    ifCount = ifCount + 1
    val str = "if" + ifCount
    return ifAssign(Vector(ifRef(Vector(str))), nIf(a, nfVec1, nfVec2))
  }

  def removeIf (x: Formula) : Formula = x match {
    case numIF(nIf(a, b, c)) => {
      if(formulaHasIf(a(0))) return numIF(nIf(Vector(removeIf(a(0))), b, c))
      if(formulaHasIf(b(0))) {return numIF(nIf(a, Vector(removeIf(b(0))), c)) }
      if(formulaHasIf(c(0))) return numIF(nIf(a, b, Vector(removeIf(c(0)))))
      return numIfRef(ifRef(Vector("if" + ifCount)))
    }
    case Num (i)    => Num(i)
    case Boo (b)    => Boo(b)
    case Cell(c, r) => Cell(c, r)
    case Div(l, r) => if(formulaHasIf(l)) {Div(removeIf(l), r); } else {Div(l, removeIf(r)); }
    case Add (l, r) => if(formulaHasIf(l)) {Add(removeIf(l), r); } else {Add(l, removeIf(r)); }
    case Mul (l, r) => if(formulaHasIf(l)) {Mul(removeIf(l), r); } else {Mul(l, removeIf(r)); }
    case Sub (l, r) => if(formulaHasIf(l)) {Sub(removeIf(l), r); } else {Sub(l, removeIf(r)); }
    case and (l, r) => if(formulaHasIf(l)) {and(removeIf(l), r); } else {and(l, removeIf(r)); }
    case equal (l, r) => if(formulaHasIf(l)) {equal(removeIf(l), r); } else {equal(l, removeIf(r)); }
    case great (l, r)  => if(formulaHasIf(l)) {great(removeIf(l), r); } else {great(l, removeIf(r)); }
    case greatEqual (l, r)  => if(formulaHasIf(l)) {greatEqual(removeIf(l), r); } else {greatEqual(l, removeIf(r)); }
    case less (l, r)  => if(formulaHasIf(l)) {less(removeIf(l), r); } else {less(l, removeIf(r)); }
    case lessEqual (l, r)  => if(formulaHasIf(l)) {lessEqual(removeIf(l), r); } else {lessEqual(l, removeIf(r)); }
    case pow (l, r)  => if(formulaHasIf(l)) {pow(removeIf(l), r); } else {pow(l, removeIf(r)); }
    case Arr(b, e) => Arr(b, e)
    case SUM(a) => SUM(argsRemoveIf(a))
    case AVERAGE(a) => SUM(argsRemoveIf(a))
    case Ref(Cell(r,c)) => Ref(Cell(r, c))
    case numIfRef(r) => numIfRef(r)
    case nullNum() => nullNum()
    case strConst(s) => strConst(s)
  }

  def argsRemoveIf(a: Vector[NumArguments]): Vector[NumArguments] = {
    var rtn = Vector[NumArguments]();
    for (i <- 0 until a.length) {
      rtn = rtn ++ Vector[NumArguments](argRemoveIf(a(i)))
    }
    return rtn
  }

  def argRemoveIf(a: NumArguments) : NumArguments = a match {
    case Arg(f) => Arg(removeIf(f))
    case Args(b, f) => {
      if(formulaHasIf(b)) return Args(removeIf(b), f)
      return Args(b, argRemoveIf(f))
    }
  }

  def formulaHasIf (x: Formula) : Boolean = x match {
    case numIF(nIf(_,_,_)) => true
    case Num (i)    => false
    case Boo (b)    => false
    case Cell(c, r) => false
    case Div(l, r) => formulaHasIf(l) || formulaHasIf(r)
    case Add (l, r) => formulaHasIf(l) || formulaHasIf(r)
    case Mul (l, r) => formulaHasIf(l) || formulaHasIf(r)
    case Sub (l, r) => formulaHasIf(l) || formulaHasIf(r)
    case and (l, r) => formulaHasIf(l) || formulaHasIf(r)
    case equal (l, r) => formulaHasIf(l) || formulaHasIf(r)
    case great (l, r)  => formulaHasIf(l) || formulaHasIf(r)
    case greatEqual (l, r)  => formulaHasIf(l) || formulaHasIf(r)
    case less (l, r)  => formulaHasIf(l) || formulaHasIf(r)
    case lessEqual (l, r)  => formulaHasIf(l) || formulaHasIf(r)
    case pow (l, r)  => formulaHasIf(l) || formulaHasIf(r)
    case Arr(b, e) => false
    case SUM(a) => argsHasIf(a)
    case AVERAGE(a) => argsHasIf(a)
    case Ref(Cell(r,c)) => false
    case numIfRef(r) => false
    case nullNum() => false
    case strConst(s) => false
  }

  def argsHasIf(a: Vector[NumArguments]): Boolean = {
    for (i <- 0 until a.length) {
      if (argHasIf(a(i))) return true
    }
    return false
  }

  def argHasIf(a: NumArguments) : Boolean = a match {
      case Args(l, r) => formulaHasIf(l) || argHasIf(r)
      case Arg(r) => formulaHasIf(r)
  }

}
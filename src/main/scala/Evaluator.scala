import java.util

import org.bitbucket.inkytonik.kiama.attribution.Attribution
import syntax.ExpParserSyntax

/**
  * Use attribution to evaluate an expression.
  */
object Evaluator extends Attribution {

  import syntax.ExpParserSyntax._

  def expvalue (a: Exp) : String = a match {
      case Assign(c, v) => assignValue(Assign(c, v))
      case stmts(l,r) => expvalue(l) + expvalue(r)
      case ifStmt(l, r) => ifValue(l) + expvalue(r)
    }

  def assignValue (a: Exp) : String = a match {
      case Assign(l, numIF(nIf(fb, f1, f2))) => numIfVal(l, fb, f1, f2)
      case Assign(l, r) => FormAsserts(r) + getType(l, r) + FormValue(l) + "=" + FormValue(r) + ";\n"
    }

  def getType(c: Formula, f: Formula) : String = f match {
    case strConst(s) => "int " + FormValue(c) + "Num = 0; \nint "
    case _ => "int " + FormValue(c) + "Num = 1; \nint "
  }


  def getArgsLength(v: Vector[NumArguments]): Int = {
    var sum = 0;
    v.foreach((a: NumArguments) => sum = sum + argumentsValue(a).length)
    return sum
  }

  def ifValue (a: assignIf) : String = a match {
      case ifAssign(r, nIf(b, f1, f2)) =>
        "int " + getIfRef(r) + "=0;\nif(" + FormValue(b(0)) + "!= 0) { \n" +
          "\t" + getIfRef(r) + "=" + numVecVal(f1) + "; \n" +
          "\t" + getIfRef(r) + "Num =" + getIsNumForm(f1) + "; \n" +
          "} else { \n" +
          "\t" + getIfRef(r) + "Num =" + getIsNumForm(f2) + "; \n" +
          "\t" + getIfRef(r) + "=" + numVecVal(f2) + "; \n}\n"
  }

  def getIsNumForm(formulas: Vector[ExpParserSyntax.Formula]): String = {
    var rtn = "";
    for (i <- 0 until formulas.length) {
      rtn = rtn + numValue(formulas(i))
    }
    return rtn
  }

  def numValue(f: ExpParserSyntax.Formula) : String = f match {
    case Num (i)    => "1"
    case Boo (b)    => "1"
    case Cell(c, r) => r + c + "Num"
    case Div(l, r) => "1"
    case Add (l, r) => "1"
    case Mul (l, r) => "1"
    case Sub (l, r) => "1"
    case and (l, r) => "1"
    case equal (l, r) => "1"
    case great (l, r)  => "1"
    case greatEqual (l, r)  => "1"
    case less (l, r)  => "1"
    case lessEqual (l, r)  => "1"
    case pow (l, r)  => "1"
    case Arr(b, e) => "0"
    case SUM(a) => "1"
    case AVERAGE(a) => "1"
    case numIF(nIf(b, f1, f2)) => "0"
    case Ref(Cell(r,c)) => r + c + "Num"
    case numIfRef(r) => r.rows(0) + "Num"
    case nullNum() => "1"
    case strConst(r) => "0"
  }

  def getIfRef(r: Vector[ifRef]) : String = {
    return r(0).rows(0)
  }

  def FormAsserts(f: Formula) : String = f match{
      case Num (i)    => ""
      case Boo (b)    => ""
      //case S (s)    => ""
      case Cell(c, r) => ""
      case Div(l, r) => AssertNum(l) + AssertNum(r) + FormAsserts(l) + "if(" + FormValue(r) + " == 0) \n\t __VERIFIER_error(); \n"
      case Add (l, r) => AssertNum(l) + AssertNum(r) + FormAsserts (l) + FormAsserts (r)
      case Mul (l, r) => AssertNum(l) + AssertNum(r) + FormAsserts (l)  + FormAsserts (r)
      case Sub (l, r) => AssertNum(l) + AssertNum(r) + FormAsserts (l) + FormAsserts (r)
      case and (l, r) => AssertNum(l) + AssertNum(r) + FormAsserts(l) + FormAsserts (r)
      case equal (l, r) => AssertNum(l) + AssertNum(r) + FormAsserts(l) + FormAsserts (r)
      case great (l, r)  => AssertNum(l) + AssertNum(r) + FormAsserts(l) + FormAsserts (r)
      case greatEqual (l, r)  => AssertNum(l) + AssertNum(r) + FormAsserts(l) + FormAsserts (r)
      case less (l, r)  => AssertNum(l) + AssertNum(r) + FormAsserts(l) + FormAsserts (r)
      case lessEqual (l, r)  => AssertNum(l) + AssertNum(r) + FormAsserts(l) + FormAsserts (r)
      case pow (l, r)  => AssertNum(l) + AssertNum(r) + FormAsserts(l) + FormAsserts (r)
      case Arr(b, e) => ""
      // TODO: add AssertNumArgs
      case SUM(a) => getArgAsserts(a)
      case AVERAGE(a) => getArgAsserts(a) + "if(" + getArgsLength(a) + " == 0) \n\t __VERIFIER_error(); \n"
      case numIF(nIf(b, f1, f2)) => numVecAsserts(f1, f2)
      case Ref(Cell(r,c)) => ""
      case numIfRef(r) => ""
      case nullNum() => ""
      case strConst(r) => ""
    }

  def AssertNum(f: Formula) : String = {
    f match {
      case Ref(c) => "if(" + FormValue(c) + "Num == 0) \n\t __VERIFIER_error(); \n";
      case conCat(a) => "__VERIFIER_error(); \n"
      case strConst(r) => "__VERIFIER_error(); \n"
      case _ => ""
    }
  }



  def FormValue(f: Formula) : String = f match {
      case Num (i)    => "__VERIFIER_nondet_int()"
      case Boo ("true")    => "1"
      case Boo ("false") => "0"
     // case S (s)    => s
      case Cell(c, r) => c + r
      case Div(l, r) => FormValue (l) + "/" + FormValue (r)
      case Add (l, r) => FormValue (l) + "+" + FormValue (r)
      case Mul (l, r) => FormValue (l) + "*" + FormValue (r)
      case Sub (l, r) => FormValue (l) + "-" + FormValue (r)
      case and (l, r) => FormValue(l) + "&" + FormValue (r)
      case equal (l, r) => FormValue(l) + "==" + FormValue (r)
      case great (l, r)  => FormValue(l) + ">" + FormValue (r)
      case greatEqual (l, r)  => FormValue(l) + ">=" + FormValue (r)
      case less (l, r)  => FormValue(l) + "<" + FormValue (r)
      case lessEqual (l, r)  => FormValue(l) + "<=" + FormValue (r)
      case pow (l, r)  => FormValue(l) + "^" + FormValue (r)
      case Arr(b, e) => getArray(b, e)
      case Ref(c) => FormValue(c)
      case SUM(a) => getSum(a)
      case AVERAGE(a) => getAverage(a)
      case numIF(nIf(b, f1, f2)) => "IF(" + b + ") { \n" + numVecVal(f1) + "; \n } else {" + numVecVal(f2) + "; \n}"
      case Ref(Cell(c, r)) => c + "" + r;
      case numIfRef(r) => r.rows(0)
      case nullNum() => "0"
      case strConst(v) => "__VERIFIER_nondet_int()" //"\"" + v.substring(1, v.length-1) + "\""
    }

  def numIfVal(c: Formula, b: Vector[ExpParserSyntax.Formula], f: Vector[ExpParserSyntax.Formula], f1: Vector[ExpParserSyntax.Formula]) : String = {
    var rtn = numVecAsserts(f, f1)
    rtn = rtn + "if(" + numVecVal(b) + "!=0) { \n"
    rtn = rtn + FormValue(c) + "=" + numVecVal(f) + "; \n"
    rtn = rtn + "int " + FormValue(c) + "Num = " + numVecVal(f) + "} else {\n"
    rtn = rtn + FormValue(c) + "=" + numVecVal(f1)
    rtn = rtn + "int " + FormValue(c) + "Num = " + numVecVal(f) + "; \n}"
    return rtn;
  }

  def numVecAsserts(f: Vector[ExpParserSyntax.Formula], f1: Vector[ExpParserSyntax.Formula]) : String = {
    var rtn = "";
    for (i <- 0 until f.length) {
      rtn = rtn + FormAsserts(f(i))
    }
    for (i <- 0 until f1.length) {
      rtn = rtn + FormAsserts(f1(i))
    }
    return rtn
  }

  def numVecVal(f: Vector[ExpParserSyntax.Formula]) : String = {
    var rtn = "";
    for (i <- 0 until f.length) {
      rtn = rtn + FormValue(f(i))
    }
    return rtn
  }


  def getCol(f: Formula) : String = f match {
      case Ref(Cell(c, r)) => c
      case _ => ""
    }

  def getRow(f: Formula) : Int = f match {
      case Ref(Cell(c, r)) => r.toInt
      case _ => 0
    }

  def combine(str1: String, str2: String): String = {
    return str1 + str2
  }

  def argumentsValue(f: NumArguments) : List[Formula] = f match {
      case Args(Arr(b, e), r) => getArrayList(b, e) ::: argumentsValue(r)
      //case Args(numIF(b, f1, f2), r) =>
      case Args(l, r) => List(l) ::: argumentsValue(r)
      case Arg(Arr(b, e)) => getArrayList(b, e)
      case Arg(r) => List(r)
    }

  def getArgAsserts(v: Vector[NumArguments]): String = {
    var rtn = ""
    v.foreach((a: NumArguments) => rtn = rtn + argAsserts(a))
    return rtn
  }

  def argAsserts(f: NumArguments) : String = f match {
      case Args(l, r) => argAsserts(r) + FormAsserts(l)
      case Arg(r) => FormAsserts(r)
    }



  def nextColString(str: String): String = {
    var i = str;
    val lastChar = i.charAt(i.length - 1) + 1
    i = i.substring(0, i.length -1) + lastChar.toChar
    val pastZ = 'Z' + 1
    var k : Int = i.length() - 1
    while (i.charAt(k) == pastZ) {
      val pt1 = i.substring(0, k - 1)
      val pt2 = (i.charAt(k-1) + 1).toChar + "A"
      val pt3 = i.substring(k+1, i.length)
      i = pt1 + pt2 + pt3
      k = k - 1
    }
    return i
  }

  def getArray(b : Formula, e : Formula) : String = {
    var str = "["

    val bCol = getCol(b)
    val eCol = getCol(e)
    val bRow = getRow(b)
    val eRow = getRow(e)

    // Create the array.
    for (i <- getColNum(bCol) to getColNum(eCol)) {
      for (j <- bRow to eRow ) {
        str = str + getColStr(i) + j + ", "
      }
    }
    return (str.substring(0, str.length - 2) + "]")
  }

  def getArrayList (b : Formula, e : Formula) : List[Formula] = {
    var l = List[Ref]()

    val bCol = getCol(b)
    val eCol = getCol(e)
    val bRow = getRow(b)
    val eRow = getRow(e)

    for(i <- getColNum(bCol) to getColNum(eCol)){
      for (j <- bRow to eRow ) {
        val cellRef = Ref(Cell(getColStr(i), j.toString))
        l = cellRef :: l
      }
    }
    return l
  }

  def getSum (v: Vector[NumArguments]) : String = {
    var argsVec = List[Formula]()
    for (i <- 0 until v.length) {
      argsVec = argsVec ::: argumentsValue(v(i))
    }
    var builder = ""
    for (j <- 0 until argsVec.length - 1) {
      val str = FormValue(argsVec(j)) + " + "
      builder += str
    }
    builder += FormValue(argsVec.last)
    return builder
  }

  def getAverage (v: Vector[NumArguments]) : String = {
    var argsVec = List[Formula]()
    for (i <- 0 until v.length) {
      argsVec = argsVec ::: argumentsValue(v(i))
    }
    var builder = "("
    for (j <- 0 until argsVec.length - 1) {
      val str = FormValue(argsVec(j)) + " + "
      builder += str
    }
    builder += (FormValue(argsVec.last) + ") / ")
    builder += (argsVec.length.toString)
    return builder
  }

  def getColNum (c: String) : Int = {
    var rtn = 0
    val str = c.replace("@", "")
    for(i <- 0 until str.length) {
      rtn = rtn + ( Math.pow(26, str.length - i - 1).toInt * (str.charAt(i) - 'A' + 1))
    }
    return rtn
  }

  def getColStr (c: Int) : String = {
    var rtn = ""
    var col = c;
    while(col > 0) {
      col = col - 1;
      val mod = col % 26
      val charAdd = 'A' + mod
      rtn = charAdd.toChar + rtn
      col = (col - mod) / 26
    }
    return rtn
  }

}
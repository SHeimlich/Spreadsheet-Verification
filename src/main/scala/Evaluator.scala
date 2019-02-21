import java.util

import org.bitbucket.inkytonik.kiama.attribution.Attribution
import syntax.ExpParserSyntax

/**
  * Use attribution to evaluate an expression.
  */
object Evaluator extends Attribution {

  import syntax.ExpParserSyntax._

  var currentCell = "";

  def setCurrentCell(c: String) : String = {
    currentCell = c;

    return "";
  }

  val expvalue : Exp => String =
    attr {
      case Assign(c, v) => assignValue(Assign(c, v))
      case stmts(l,r) => expvalue(l) + expvalue(r)
      case ifStmt(l, r) => ifValue(l) + expvalue(r)
    }

  val assignValue : Exp => String =
    attr {
      case Assign(l, nf(numIF(nIf(fb, f1, f2)))) => numIfVal(l, fb, f1, f2)
      case Assign(l, r) => setCurrentCell(NumFormValue(l)) + FormAsserts(r) + "int " + NumFormValue(l) + "=" + FormValue(r) + ";\n"
    }


  def getArgsLength(v: Vector[NumArguments]): Int = {
    var sum = 0;
    v.foreach((a: NumArguments) => sum = sum + argumentsValue(a).length)
    return sum
  }

  val ifValue : assignIf => String =
    attr {
      case ifAssign(r, nIf(b, f1, f2)) =>
        "int " + getIfRef(r) + "=0;\nif(" + NumFormValue(b(0)) + ") { \n" +
          "\t" + getIfRef(r) + "=" + numVecVal(f1) + "; \n} else { \n" +
          "\t" + getIfRef(r) + "=" + numVecVal(f2) + "; \n}\n"
  }

  def getIfRef(r: Vector[ifRef]) : String = {
    return r(0).rows(0)
  }


  val FormAsserts : Formula => String =
    attr {
      case nf(f) => NumFormAsserts(f)
      case sf(f) => StrFormAsserts(f)
    }

  val FormValue : Formula => String =
    attr {
      case nf(f) => NumFormValue(f)
      case sf(f) => StrFormValue(f)
    }

  val NumFormAsserts: NumFormula => String =
    attr {
      case Num (i)    => ""
      case Boo (b)    => ""
      //case S (s)    => ""
      case Cell(c, r) => ""
      case Div(l, r) => NumFormAsserts(l) + "if(" + NumFormValue(r) + " == 0) \n\t __VERIFIER_error(); \n"
      case Add (l, r) => NumFormAsserts (l) + NumFormAsserts (r)
      case Mul (l, r) => NumFormAsserts (l)  + NumFormAsserts (r)
      case Sub (l, r) => NumFormAsserts (l) + NumFormAsserts (r)
      case and (l, r) => NumFormAsserts(l) + NumFormAsserts (r)
      case equal (l, r) => NumFormAsserts(l) + NumFormAsserts (r)
      case great (l, r)  => NumFormAsserts(l) + NumFormAsserts (r)
      case greatEqual (l, r)  => NumFormAsserts(l) + NumFormAsserts (r)
      case less (l, r)  => NumFormAsserts(l) + NumFormAsserts (r)
      case lessEqual (l, r)  => NumFormAsserts(l) + NumFormAsserts (r)
      case pow (l, r)  => NumFormAsserts(l) + NumFormAsserts (r)
      case Arr(b, e) => ""
      case SUM(a) => getArgAsserts(a)
      case AVERAGE(a) => getArgAsserts(a) + "if(" + getArgsLength(a) + " == 0) \n\t __VERIFIER_error(); \n"
      case numIF(nIf(b, f1, f2)) => numVecAsserts(f1, f2)
      case Ref(Cell(r,c)) => ""
      case numIfRef(r) => ""
    }


  def StrFormAsserts : StringFormula => String =
    attr {
      case strConst(s) => ""
      case NumAsStr(f) => NumFormAsserts(f)
    }


  val NumFormValue : NumFormula => String =
    attr {
      case Num (i)    => i
      case Boo ("true")    => "1"
      case Boo ("false") => "0"
     // case S (s)    => s
      case Cell(c, r) => c + r
      case Div(l, r) => NumFormValue (l) + "/" + NumFormValue (r)
      case Add (l, r) => NumFormValue (l) + "+" + NumFormValue (r)
      case Mul (l, r) => NumFormValue (l) + "*" + NumFormValue (r)
      case Sub (l, r) => NumFormValue (l) + "-" + NumFormValue (r)
      case and (l, r) => NumFormValue(l) + "&" + NumFormValue (r)
      case equal (l, r) => NumFormValue(l) + "==" + NumFormValue (r)
      case great (l, r)  => NumFormValue(l) + ">" + NumFormValue (r)
      case greatEqual (l, r)  => NumFormValue(l) + ">=" + NumFormValue (r)
      case less (l, r)  => NumFormValue(l) + "<" + NumFormValue (r)
      case lessEqual (l, r)  => NumFormValue(l) + "<=" + NumFormValue (r)
      case pow (l, r)  => NumFormValue(l) + "^" + NumFormValue (r)
      case Arr(b, e) => getArray(b, e)
      case Ref(c) => NumFormValue(c)
      case SUM(a) => getSum(a)
      case AVERAGE(a) => getAverage(a)
      case numIF(nIf(b, f1, f2)) => "IF(" + b + ") { \n" + numVecVal(f1) + "; \n } else {" + numVecVal(f2) + "; \n}"
      case Ref(Cell(c, r)) => c + "" + r;
      case numIfRef(r) => r.rows(0)
    }

  def numIfVal(c: NumFormula, b: Vector[ExpParserSyntax.NumFormula], f: Vector[ExpParserSyntax.NumFormula], f1: Vector[ExpParserSyntax.NumFormula]) : String = {
    var rtn = numVecAsserts(f, f1)
    rtn = rtn + "if(" + numVecVal(b) + "!=0) { \n"
    rtn = rtn + NumFormValue(c) + "=" + numVecVal(f) + "; \n} else {\n"
    rtn = rtn + NumFormValue(c) + "=" + numVecVal(f1) + "; \n}"
    return rtn;
  }

  def numVecAsserts(f: Vector[ExpParserSyntax.NumFormula], f1: Vector[ExpParserSyntax.NumFormula]) : String = {
    var rtn = "";
    for (i <- 0 until f.length) {
      rtn = rtn + NumFormAsserts(f(i))
    }
    for (i <- 0 until f1.length) {
      rtn = rtn + NumFormAsserts(f1(i))
    }
    return rtn
  }

  def numVecVal(f: Vector[ExpParserSyntax.NumFormula]) : String = {
    var rtn = "";
    for (i <- 0 until f.length) {
      rtn = rtn + NumFormValue(f(i))
    }
    return rtn
  }

  val StrFormValue : StringFormula => String =
    attr {
      case strConst(v) => v
      case NumAsStr(f) => NumFormValue(f)
    }

  val getCol : NumFormula => String =
    attr {
      case Ref(Cell(c, r)) => c
      case _ => ""
    }

  val getRow : NumFormula => Int =
    attr {
      case Ref(Cell(c, r)) => r.toInt
      case _ => 0
    }

  def combine(str1: String, str2: String): String = {
    return str1 + str2
  }

  //TODO: Add a thing here for if statements
  val argumentsValue : NumArguments => List[NumFormula] =
    attr {
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

  val argAsserts : NumArguments => String =
    attr {
      case Args(l, r) => argAsserts(r) + NumFormAsserts(l)
      case Arg(r) => NumFormAsserts(r)
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

  def getArray(b : NumFormula, e : NumFormula) : String = {
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

  def getArrayList (b : NumFormula, e : NumFormula) : List[NumFormula] = {
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
    var argsVec = List[NumFormula]()
    for (i <- 0 until v.length) {
      argsVec = argsVec ::: argumentsValue(v(i))
    }
    var builder = ""
    for (j <- 0 until argsVec.length - 1) {
      val str = NumFormValue(argsVec(j)) + " + "
      builder += str
    }
    builder += NumFormValue(argsVec.last)
    return builder
  }

  def getAverage (v: Vector[NumArguments]) : String = {
    var argsVec = List[NumFormula]()
    for (i <- 0 until v.length) {
      argsVec = argsVec ::: argumentsValue(v(i))
    }
    var builder = "("
    for (j <- 0 until argsVec.length - 1) {
      val str = NumFormValue(argsVec(j)) + " + "
      builder += str
    }
    builder += (NumFormValue(argsVec.last) + ") / ")
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
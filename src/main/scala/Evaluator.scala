import org.bitbucket.inkytonik.kiama.attribution.Attribution
import syntax.ExpParserSyntax

/**
  * Use attribution to evaluate an expression.
  */
object Evaluator extends Attribution {

  import syntax.ExpParserSyntax._

  val expvalue : Exp => String =
    attr {
      case Assign(l, r) => l + "=" + formvalue(r)
    }

  val formvalue : Formula => String =
    attr {
      case Num (i)    => i
      case Boo (b)    => b
      case S (s)    => s
      case Div(l, r) => formvalue (l) + "/" + formvalue (r)
      case Add (l, r) => formvalue (l) + "+" + formvalue (r)
      case Mul (l, r) => formvalue (l) + "*" + formvalue (r)
      case Sub (l, r) => formvalue (l) + "-" + formvalue (r)
      case and (l, r) => formvalue(l) + "&" + formvalue (r)
      case equal (l, r) => formvalue(l) + "==" + formvalue (r)
      case great (l, r)  => formvalue(l) + ">" + formvalue (r)
      case greatEqual (l, r)  => formvalue(l) + ">=" + formvalue (r)
      case less (l, r)  => formvalue(l) + "<" + formvalue (r)
      case lessEqual (l, r)  => formvalue(l) + "<=" + formvalue (r)
      case pow (l, r)  => formvalue(l) + "^" + formvalue (r)
      case Arr(b, e) => getArray(b, e)
      case Ref(c) => formvalue(c)
      case Cell(c, r) => c + r
      case SUM(a) => getSum(a)
      case AVERAGE(a) => getAverage(a)
    }

  val getCol : Formula => String =
    attr {
      case Ref(Cell(c, r)) => c
      case _ => ""
    }

  val getRow : Formula => Int =
    attr {
      case Ref(Cell(c, r)) => r.toInt
      case _ => 0
    }

  def combine(str1: String, str2: String): String = {
    return str1 + str2
  }

  val argumentsValue : Arguments => List[Formula] =
    attr {
      case Args(Arr(b, e), r) => argumentsValue(r) ::: getArrayList(b, e)
      case Args(l, r) => argumentsValue(r) :+ l
      case Arg(Arr(b, e)) => getArrayList(b, e)
      case Arg(r) => List(r)
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

  def getSum (v: Vector[Arguments]) : String = {
    var argsVec = List[Formula]()
    for (i <- 0 until v.length) {
      argsVec = argsVec ::: argumentsValue(v(i))
    }
    var builder = ""
    for (j <- 0 until argsVec.length - 1) {
      val str = formvalue(argsVec(j)) + " + "
      builder += str
    }
    builder += formvalue(argsVec.last)
    return builder
  }

  def getAverage (v: Vector[Arguments]) : String = {
    var argsVec = List[Formula]()
    for (i <- 0 until v.length) {
      argsVec = argsVec ::: argumentsValue(v(i))
    }
    var builder = "("
    for (j <- 0 until argsVec.length - 1) {
      val str = formvalue(argsVec(j)) + " + "
      builder += str
    }
    builder += (formvalue(argsVec.last) + ") / ")
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
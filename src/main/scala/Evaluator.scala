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
      case Args(Arr(b, e), r) => argumentsValue(r) ::: getArrayList(formvalue(b), formvalue(e))
      case Args(l, r) => argumentsValue(r) :+ l
      case Arg(Arr(b, e)) => getArrayList(formvalue(b), formvalue(e))
      case Arg(r) => List(r)
    }




  def getArray (b : Formula, e : Formula) : String = {
    println("b = " + b + " e = " + e)
    var str = "["

    val bCol = getCol(b)
    val eCol = getCol(e)
    val bRow = getRow(b)
    val eRow = getRow(e)

    println("bCol = " + bCol + " bRow = " + bRow)
    println("eCol = " + eCol + " eRow = " + eRow)

    var i = bCol.toString
    while(i.length != eCol.length) {
      val at = "@"
      i = at.toString + i.toString // "@" is just before "A" in ASCII
      println("increasing str length to: " + i)
    }


    var tmp2 = 0
    while( !i.equals(eCol) ){

      for (j <- bRow to eRow ) {
        println("Col = " + i + " Row = " + j)
        str = str + i.replace("@", "") + j + ", "
      }
      val lastChar = i.charAt(i.length - 1) + 1
      i = i.substring(0, i.length -1) + lastChar.toChar
      println("Last char = " + lastChar.toChar)
      val pastZ = 'Z' + 1
      if (lastChar == pastZ) {  // "[" is one past "Z" in ASCII
        var k : Int = i.length() - 1
        var tmp = 0
        println("k = " + k + " Char at K=" + i.charAt(k))
        while (i.charAt(k) == pastZ) {
          val pt1 = i.substring(0, k - 1)
          val pt2 = (i.charAt(k-1) + 1).toChar + "A"
          val pt3 = i.substring(k+1, i.length)
          println("pt1=" + pt1 + " pt2=" + pt2 + " pt3=" + pt3 )
          i = pt1 + pt2 + pt3
          tmp = tmp + 1
          k = k - 1
        }
      }
      tmp2 = tmp2 + 1


    }
    return (str.substring(0, str.length - 2) + "]")
  }

  def getArrayList (b : String, e : String) : List[Formula] = {
    var l = List[Ref]()
    for( i <- b.charAt(0) to e.charAt(0)){
      for (j <- b.charAt(1) to e.charAt(1)) {
        val cellStr = i + "" + j
        val cellRef = Ref(Cell(i.toString, j.toString))
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

}
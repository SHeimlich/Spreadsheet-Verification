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
      case Arr(b, e) => getArray(formvalue(b), formvalue(e))
      case Ref(b) => b
      case SUM(a) => getSum(a)
    }

  def combine(str1: String, str2: String): String = {
    return str1 + str2
  }

  val argumentsValue : Arguments => List[Formula] =
    attr {
      case Args(l, r) => argumentsValue(r) :+ l
      case Arg(Arr(b, e)) => getArrayList(formvalue(b), formvalue(e))
      case Arg(r) => List(r)
    }

  val argValue : Argument => Formula =
    attr {
      case Argument(r) => r
    }



  def getArray (b : String, e : String) : String = {
    var str = "["
    for( i <- b.charAt(0) to e.charAt(0)){
      for (j <- b.charAt(1) to e.charAt(1)) {
        str = str + i + j + ", "
      }
    }
    return (str.substring(0, str.length - 2) + "]")
  }

  def getArrayList (b : String, e : String) : List[Formula] = {
    var l = List[Ref]()
    for( i <- b.charAt(0) to e.charAt(0)){
      for (j <- b.charAt(1) to e.charAt(1)) {
        val cellStr = i + "" + j
        val cellRef = Ref("A2")
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

}
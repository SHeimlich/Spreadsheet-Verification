import org.bitbucket.inkytonik.kiama.attribution.Attribution

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
      case Ref (r)    => r
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

}
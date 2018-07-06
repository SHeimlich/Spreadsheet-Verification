import org.bitbucket.inkytonik.kiama.attribution.Attribution

/**
 * Use attribution to evaluate an expression.
 */
object Evaluator extends Attribution {

    import syntax.ExpParserSyntax._

    val expvalue : Exp => Int =
        attr {
            case Num (i)    => i
            case Add (l, r) => expvalue (l) + expvalue (r)
	    case Sub (l, r) => expvalue (l) - expvalue (r)
            case Mul (l, r) => expvalue (l) * expvalue (r)
        }

}

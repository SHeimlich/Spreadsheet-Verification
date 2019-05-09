// AUTOMATICALLY GENERATED by sbt-rats - EDIT AT YOUR OWN RISK

package syntax


object ExpParserSyntax {

    sealed abstract class ASTNode extends Product
    
    sealed abstract class Exp extends ASTNode with org.bitbucket.inkytonik.kiama.output.PrettyExpression
    case class stmts (assign : Exp, exp : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
    case class ifStmt (assignIf : assignIf, exp : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
     
    case class Assign (cell : Formula, formula : Formula) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
     
    sealed abstract class assignIf extends ASTNode
    case class ifAssign (ifRefs : Vector[ifRef], nIf : nIf) extends assignIf  {
        require (ifRefs.length > 0, "ifRefs field can't be empty")
    }
     
    sealed abstract class Formula extends ASTNode
    case class numIF (nIf : nIf) extends Formula  
    case class SUM (numArgumentss : Vector[NumArguments]) extends Formula  {
        require (numArgumentss.length > 0, "numArgumentss field can't be empty")
    }
    case class AVERAGE (numArgumentss : Vector[NumArguments]) extends Formula  {
        require (numArgumentss.length > 0, "numArgumentss field can't be empty")
    }
    case class conCat (numArgumentss : Vector[NumArguments]) extends Formula  {
        require (numArgumentss.length > 0, "numArgumentss field can't be empty")
    }
    case class now () extends Formula  
    case class Add (formula1 : Formula, formula2 : Formula) extends Formula  
    case class AddNull (formula : Formula) extends Formula  
    case class Sub (formula1 : Formula, formula2 : Formula) extends Formula  
    case class SubNull (formula : Formula) extends Formula  
    case class Mul (formula1 : Formula, formula2 : Formula) extends Formula  
    case class Div (formula1 : Formula, formula2 : Formula) extends Formula  
    case class pow (formula1 : Formula, formula2 : Formula) extends Formula  
    case class and (formula1 : Formula, formula2 : Formula) extends Formula  
    case class less (formula1 : Formula, formula2 : Formula) extends Formula  
    case class great (formula1 : Formula, formula2 : Formula) extends Formula  
    case class equal (formula1 : Formula, formula2 : Formula) extends Formula  
    case class lessEqual (formula1 : Formula, formula2 : Formula) extends Formula  
    case class greatEqual (formula1 : Formula, formula2 : Formula) extends Formula  
    case class percent (number : String) extends Formula  
    case class numIfRef (ifRef : ifRef) extends Formula  
    case class strConst (str : String) extends Formula  
    case class parenthesis (formula : Formula) extends Formula  
    case class nullNum () extends Formula  
     
    case class nIf (formulas1 : Vector[Formula], formulas2 : Vector[Formula], formulas3 : Vector[Formula]) extends ASTNode
     
    sealed abstract class NumArguments extends ASTNode
    case class Args (formula : Formula, numArguments : NumArguments) extends NumArguments  
    case class Arg (formula : Formula) extends NumArguments  
     
    case class Num (number : String) extends Formula  
    case class Boo (bool : String) extends Formula  
     
    case class Arr (cell1 : Formula, cell2 : Formula) extends Formula  
     
    case class Ref (cell : Formula) extends Formula  
     
    case class Cell (col : String, row : String) extends Formula  
       
    case class ifRef (rows : Vector[String]) extends ASTNode
      
    case class Decimal (digits1 : String, digits2 : String) extends Formula  
           
}

// AUTOMATICALLY GENERATED by sbt-rats - EDIT AT YOUR OWN RISK

package syntax

import syntax.ExpParserSyntax._
import org.bitbucket.inkytonik.kiama.output.{PrettyPrinter => PP, ParenPrettyPrinter => PPP}
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, Width}

trait ExpParserPrettyPrinter extends PP with PPP {

    
    def show (astNode : ASTNode, w : Width = defaultWidth) : String =
        format (astNode, w).layout
    
    def format (astNode : ASTNode, w : Width = defaultWidth) : Document =
        pretty (group (toDoc (astNode)), w)
    
    def toDoc (astNode : ASTNode) : Doc =
        astNode match {  
            case v : Exp =>
                toParenDoc (v)  
            case v @ ifAssign (v1, v2) =>
                ssep (v1.map (toDoc), emptyDoc) <> text ("=") <> space <> toDoc (v2) 
            case v @ numIF (v1) =>
                toDoc (v1) 
            case v @ SUM (v1) =>
                emptyDoc <> ssep (v1.map (toDoc), emptyDoc) <> text (")") <> space 
            case v @ AVERAGE (v1) =>
                emptyDoc <> ssep (v1.map (toDoc), emptyDoc) <> text (")") <> space 
            case v @ conCat (v1) =>
                emptyDoc <> ssep (v1.map (toDoc), emptyDoc) <> text (")") <> space 
            case v @ Add (v1, v2) =>
                toDoc (v1) <> text ("+") <> space <> toDoc (v2) 
            case v @ Sub (v1, v2) =>
                toDoc (v1) <> text ("-") <> space <> toDoc (v2) 
            case v @ Mul (v1, v2) =>
                toDoc (v1) <> text ("*") <> space <> toDoc (v2) 
            case v @ Div (v1, v2) =>
                toDoc (v1) <> text ("/") <> space <> toDoc (v2) 
            case v @ pow (v1, v2) =>
                toDoc (v1) <> text ("^") <> space <> toDoc (v2) 
            case v @ and (v1, v2) =>
                toDoc (v1) <> text ("&") <> space <> toDoc (v2) 
            case v @ less (v1, v2) =>
                toDoc (v1) <> text ("<") <> space <> toDoc (v2) 
            case v @ great (v1, v2) =>
                toDoc (v1) <> text (">") <> space <> toDoc (v2) 
            case v @ equal (v1, v2) =>
                toDoc (v1) <> text ("=") <> space <> toDoc (v2) 
            case v @ lessEqual (v1, v2) =>
                toDoc (v1) <> text ("<=") <> space <> toDoc (v2) 
            case v @ greatEqual (v1, v2) =>
                toDoc (v1) <> text (">=") <> space <> toDoc (v2) 
            case v @ percent (v1) =>
                value (v1) <> text ("%") <> space   
            case v @ numIfRef (v1) =>
                toDoc (v1)  
            case v @ strConst (v1) =>
                value (v1)  
            case v @ nullNum () =>
                text ("null") <> space 
            case v @ nIf (v1, v2, v3) =>
                emptyDoc <> ssep (v1.map (toDoc), emptyDoc) <> emptyDoc <> ssep (v2.map (toDoc), emptyDoc) <> emptyDoc <> ssep (v3.map (toDoc), emptyDoc) <> text (")") <> space 
            case v @ Args (v1, v2) =>
                toDoc (v1) <> text (";") <> space <> toDoc (v2) 
            case v @ Arg (v1) =>
                toDoc (v1)  
            case v @ Num (v1) =>
                value (v1) 
            case v @ Boo (v1) =>
                value (v1) 
            case v @ Arr (v1, v2) =>
                text ("[.") <> space <> toDoc (v1) <> text (":.") <> space <> toDoc (v2) <> text ("]") <> space 
            case v @ Ref (v1) =>
                text ("[.") <> space <> toDoc (v1) <> text ("]") <> space 
            case v @ Cell (v1, v2) =>
                value (v1) <> value (v2)   
            case v @ ifRef (v1) =>
                ssep (v1.map (text), emptyDoc) <> text ("if") <> space  
            case v @ Decimal (v1, v2) =>
                value (v1) <> text (".") <> space <> value (v2)       
        }
    
    override def toParenDoc (astNode : org.bitbucket.inkytonik.kiama.output.PrettyExpression) : Doc =
        astNode match {
            case v @ stmts (v1, v2) =>
                toDoc (v1) <> recursiveToDoc (v, v2, org.bitbucket.inkytonik.kiama.output.RightAssoc) 
            case v @ ifStmt (v1, v2) =>
                toDoc (v1) <> recursiveToDoc (v, v2, org.bitbucket.inkytonik.kiama.output.RightAssoc) 
            case v @ Assign (v1, v2) =>
                toDoc (v1) <> space <> text (" = ") <> space <> toDoc (v2) <> text (",") <> space
            case _ =>
                super.toParenDoc (astNode)
        }

}

object ExpParserPrettyPrinter extends ExpParserPrettyPrinter


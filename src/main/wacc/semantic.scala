package wacc

import scala.collection.mutable.ListBuffer
import parsley.{Result, Success, Failure}
import ast._
import CheckExprSemantic._
import CheckStatmentSemantic._
import CheckTypeSemantic._

object SemanticTable {

}

object CheckExprSemantic {

    def checkExprSemantic(expr: Expr) : Result[String, Stmt] = {
        expr match {
            case Add => {}
            case Sub => {}
            case Mul => {}
            case Div => {}
            case Mod => {}
            case And => {}
            case Or => {}
            case LT => {}
            case LTE => {}
            case GT => {}
            case GTE => {}
            case E => {}
            case NE => {}
            case Not => {}
            case Neg => {}
            case Len => {}
            case Ord => {}
            case Chr => {}
            case default => checkLit(expr)
        }
    }

    def checkLit (lit: Expr) : Result[String, Stmt] = {
        lit match {
            case IntLit => {}
            case BoolLit => {}
            case CharLit => {}
            case StrLit => {}
            case Ident => {}
            case ArrayElem => {}
            case Paran => {}
            case PairLit => {}
        }
    }
}

object CheckStatmentSemantic {
    def checkFunctionList(list: List[Func]) : Result[String, Stmt] {
        
    }

    def checkStatementSemantic(stmt: Stmt) : Result[String, Stmt] {

    }
}

object  CheckTypeSemantic {

    def checkTypeSemantic(stmt: Stmt) : Result[String, Stmt] {

    }
}

object Semantic {
  //Array
    def checkSemantic(prog: Program) : Result[String, Stmt] = {
        checkStatementSemantic(prog.body)
        prog.funcs.foreach(f => checkStatmentSemantic(f))
    }



// // def checkSemantic(expr: Expr, error: Error): Type = {
// //   expr match {
// //     case Var(_, varType) => varType
// //     case ArrayLiteral(elements, elementType, isFrozen) =>
// //       elements.foreach(checkSemantic(_, error))
// //       ArrayType(elementType, isFrozen)
// //     case ArrayAccess(arr, index) =>
// //       val ArrayType(elementType, _) = checkSemantic(arr, error)
// //       val indexType = checkSemantic(index, error)
// //       if (indexType != IntType) {
// //         error.addError("Array index must be of type Int.")
// //       }
// //       elementType
// //     case ArrayModification(arr, index, newValue) =>
// //       val ArrayType(elementType, isFrozen) = checkSemantic(arr, error)
// //       val indexType = checkSemantic(index, error)
// //       if (indexType != IntType) {
// //         error.addError("Array index must be of type Int.")
// //       }
// //       val newValueType = checkSemantic(newValue, error)
// //       if (isFrozen) {
// //         error.addError("Cannot modify a frozen array.")
// //       }
// //       if (newValueType != elementType) {
// //         error.addError("New value type does not match array element type.")
// //       }
// //       elementType
// //   }
// // }
}
package wacc

import scala.collection.mutable.{HashMap}
import parsley.{Result, Success, Failure}
import ast._

object symbolTable {
    //map between the variable(ident) name and the type
    //so everytime the assign somehitng create new entry
    // when reassgin check the table if the rvalue have the same typw as the variable using the table
    // check scope (change the name)
    // array check a[i] if i is in range
    //
    val symTable: scala.collection.mutable.Map[String, Type] = HashMap[String, Type]()

    def lookUp (name: String): Type = {
        symTable.get(name).get
    }

    def assign (name: String, newType: Type) = {
        if (symTable.contains(name)) {
            val oldType:Type = lookUp (name)
            if (oldType != newType) {    
                //error?
            } else {
                symTable.update(name, newType)
            }
        } else {
            symTable.addOne(name, newType)
        }
    }
}

object Semantic {
  //Array
    def checkSemantic(prog: Stmt) : Unit = {
        prog match {
            case Program =>
                checkStatementSemantic(prog.body)
                prog.funcs.foreach(f => checkStatmentSemantic(f))
            case default => System.exit(0)}
    }

    def checkExprSemantic(expr: Expr) : Unit = {
        expr match {
            case Add(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case Sub(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case Mul(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case Div(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case Mod(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case And(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case Or(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case LT(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case LTE(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case GT(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case GTE(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case E(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case NE(e1, e2) => {
                checkExprSemantic(e1)
                checkExprSemantic(e2)
            }
            case Not(e1) => {
                checkExprSemantic(e1)
            }
            case Neg(e1) => {
                checkExprSemantic(e1)
            }
            case Len(e1) => {
                checkExprSemantic(e1)
            }
            case Ord(e1) => {
                checkExprSemantic(e1)
            }
            case Chr(e1) => {
                checkExprSemantic(e1)
            }
            case default => checkLit(expr)
        }}

    def checkLit (lit: Expr) : Unit = {
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

    def checkFunctionList(list: List[Func]) : Unit = {

    }

    def checkStatementSemantic(stmt: Stmt) : Unit = {

    }

    def checkTypeSemantic(stmt: Stmt) : Unit = {

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
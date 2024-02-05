package wacc

import scala.collection.mutable.ListBuffer

sealed trait Type
case object IntType extends Type
case object StringType extends Type
case class ArrayType(elementType: Type, isFrozen: Boolean) extends Type

sealed trait Expr
case class Var(name: String, varType: Type) extends Expr
case class ArrayLiteral(elements: List[Expr], elementType: Type, isFrozen: Boolean) extends Expr
case class ArrayAccess(arr: Expr, index: Expr) extends Expr
case class ArrayModification(arr: Expr, index: Expr, newValue: Expr) extends Expr

class Semantic {
def checkSemantic(expr: Expr, error: Error): Type = {
  expr match {
    case Var(_, varType) => varType
    case ArrayLiteral(elements, elementType, isFrozen) =>
      elements.foreach(checkSemantic(_, error))
      ArrayType(elementType, isFrozen)
    case ArrayAccess(arr, index) =>
      val ArrayType(elementType, _) = checkSemantic(arr, error)
      val indexType = checkSemantic(index, error)
      if (indexType != IntType) {
        error.addError("Array index must be of type Int.")
      }
      elementType
    case ArrayModification(arr, index, newValue) =>
      val ArrayType(elementType, isFrozen) = checkSemantic(arr, error)
      val indexType = checkSemantic(index, error)
      if (indexType != IntType) {
        error.addError("Array index must be of type Int.")
      }
      val newValueType = checkSemantic(newValue, error)
      if (isFrozen) {
        error.addError("Cannot modify a frozen array.")
      }
      if (newValueType != elementType) {
        error.addError("New value type does not match array element type.")
      }
      elementType
  }
}
}
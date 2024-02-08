// package wacc

// import parsley._
// import SemanticError._
// import ast._



// object ExprEval {

//     private def binOpEval[A](a1 : A, a2 : A, op: (A, A) => A) : Either[SemanticError, A] = {
//         if (a1.instanceOf[Int]) {
//             // apply casting to BigInt
//         }
//         return Right(op(a1, a2))
//     }

//     def exprEval[A](expr: Either[SemanticError, Expr], s: String) : Either[SemanticError, Expr] = 
//         expr match {
//             // ERROR CHECKING
//             case err : SemanticError => Left(err)

//             // IDENTIFIER
//        //     case Ident(s) => getValueFromTable(s)

//             // LITERALS
//             case IntLit(_) | BoolLit(_) | CharLit(_) | StrLit(_) | ArrayElem(_,_) | Paran(_) | PairLit => Right(expr)

//             // INTEGER OPERATIONS
//             case Add(e1, e2) |  Sub(e1, e2) |  Mul(e1, e2) |  Div(e1, e2) |  Mod(e1, e2) => 
//                 (exprEval(e1, s), exprEval(e2, s)) match {
//                     case (IntLit(x), IntLit(y)) => Right(IntLit(binOpEval(x, y, expr match {
//                         case Add() => +
//                         case Sub() => -
//                         case Mul() => *
//                         case Div() => /
//                         case Mod() => %
//                     })))
//                     case (err1 : SemanticError, err2 : SemanticError) => Left(err1 + err2)
//                     case (err : SemanticError, _) | (_, err : SemanticError)  => Left(err)
//                 }


//             // BOOLEAN OPERATIONS
//             case And(e1, e2) | Or(e1, e2) | LT(e1, e2) | LTE(e1, e2) | GT(e1, e2) | GTE(e1, e2) | E(e1, e2) | NE(e1, e2) =>
//                 (exprEval(e1, s), exprEval(e2, s)) match {
//                     case (BoolLit(x), BoolLit(y)) => Right(IntLit(binOpEval(x, y, expr match {
//                         case And() => &&
//                         case Or()  => ||
//                         case LT()  => <
//                         case LTE() => <=
//                         case GT()  => >
//                         case GTE() => >=
//                         case E()   => ==
//                         case NE()  => !=
//                     })))
//                     case (err1 : List[A], err2 : List[A]) => Left(err1 + err2)
//                     case (err : List[A], _) | (_, err : List[A])  => Left(err)
//                     case _ => Left(SemanticError("Not Int type"))
//                 }
           
//             case Not(e) => exprEval(e) match {
//                 case BoolLit(x) =>  Right(BoolLit(!x))
//                 case _ => Left()
//             }
//             case Neg(e) => exprEval(e) match {
//                 case IntLit(x) =>   Right(IntLit(neg(x)))
//                 case _ => Left()
//             }
//             case Len(e) => exprEval(e) match {
//                 // Here we might need to parse through the array and chek the types of all members
//                 case ArrayElem(ident, x) =>  Right(IntLit(len(x)))
//                 case _ => Left()
//             }
//             case Ord(e) => exprEval(e) match {
//                 case CharLit(x) =>  Right(IntLit(x.toInt))
//                 case _ => Left()
//             }
//             case Chr(e) => exprEval(e) match {
//                 case IntLit(x) =>  Right(CharLit(x.toChar))
//                 case _ => Left()
//             }
//             case _ => Left(SemanticError("Undefined"))
//         }
//     }
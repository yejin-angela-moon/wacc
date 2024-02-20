package wacc

import ast._

class CodeGeneration {
    val instructions = new StringBuilder("")
    //indentify register
    val registers = Set("rax", "rbx", "rcx", "rdx", "rsi", "rdi", 
    "rsp", "rbp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15")

    def appendInstruction(instr: String): Unit = {
        instructions.append(instr + "\n")
    }

    def generateStatement(stmts: List[Stmt]): Unit = {
        stmts.head match {
            case Nil =>
            case Declare(t, ident, rvalue) =>
                //declare instruction in x86
                generateRvalue(rvalue)

            case Assign(lvalue, rvalue) =>
                //assign
                generateRvalue(rvalue )
            case BeginEnd(s) =>
                generateStatement(List(s))
            case Exit(x) =>
                generateExpression(x)
            case Free(x) =>
                generateExpression(x)
            case Func(t, ident, list, body) =>
                //idk lol
            case IfThenElse(x, s1, s2) =>
                generateExpression(x)
                generateStatement(List(s1))
                generateStatement(List(s2))
            case Print(x) =>
                generateExpression(x)
            case Println(x) =>
                generateExpression(x)
            case Program(funcs, body) =>
                //idk
            case Read(lvalue) =>
                generateLvalue(lvalue)
            case Return(x) =>
                generateExpression(x)
            case Skip =>
            case WhileDo(x, s) =>
                generateExpression(x)
                generateStatement(List(s))
        }
        generateStatement(stmts.tail)
    }

    def generateRvalue(rvalue: Rvalue): Unit = {
        rvalue match {
            case _ =>
        }
    }

    def generateLvalue(lvalue: Lvalue): Unit = {
        lvalue match {
            case _ =>
        }
    }

    def generateExpression(expr: Expr): Unit = {
        expr match {
            case Add(x, y) =>
            case Sub(x, y) =>
            case Mul(x, y) =>
            case Div(x, y) =>
            case Mod(x, y) =>
            case And(x, y) =>
            case Or(x, y) =>
            case LT(x, y) =>
            case LTE(x, y) =>
            case GT(x, y) =>
            case GTE(x, y) =>
            case E(x, y) =>
            case NE(x, y) =>
            case Not(x) =>
            case Neg(x) =>
            case Len(x) =>
            case Ord(x) =>
            case Chr(x) =>
            case IntLit(x) =>
            case BoolLit(x) =>
            case CharLit(x) =>
            case StrLit(x) =>
            case Ident(x) =>
            case ArrayElem(ident, x) =>
            case Paran(x) =>
            case PairLit =>
        }
    }
}


//
//  Ast.swift
//
//
//  Created by Nathan on 12/11/2022.
//

import Foundation

public enum Literal: CustomStringConvertible {
    case bool(Bool)
    case num(Float64)
    case str(String)
    case unit

    public var description: String {
        switch self {
        case .bool(let q):
            return q.description
        case .num(let x):
            if x - floor(x) < Float64.ulpOfOne {
                return Int(x).description
            }

            return x.description
        case .str(let s):
            return "\"\(s)\""
        case .unit:
            return "()"
        }
    }
}

public enum UnaryOperator: CustomStringConvertible {
    case logicalNegation
    case arithmeticNegation

    public var description: String {
        switch self {
        case .logicalNegation: return "!"
        case .arithmeticNegation: return "-"
        }
    }
}

public enum BinaryOperator: CustomStringConvertible {
    case add
    case sub
    case mul
    case div
    case mod
    case pow
    case equ
    case neq
    case lss
    case leq
    case gtr
    case geq
    case and
    case or

    public var description: String {
        switch self {
        case .add: return "+"
        case .sub: return "-"
        case .mul: return "*"
        case .div: return "/"
        case .mod: return "%"
        case .pow: return "**"
        case .equ: return "=="
        case .neq: return "!="
        case .lss: return "<"
        case .leq: return "<="
        case .gtr: return ">"
        case .geq: return ">="
        case .and: return "&&"
        case .or: return "||"
        }
    }
}

public enum AssignmentOperator: CustomStringConvertible {
    case eq
    case plusEq
    case minusEq
    case timesEq
    case divideEq

    public var description: String {
        switch self {
        case .eq: return "="
        case .plusEq: return "+="
        case .minusEq: return "-="
        case .timesEq: return "*="
        case .divideEq: return "/="
        }
    }
}

func indent(_ str: String) -> String {
    return
        str
        .split(separator: "\n")
        .map({ s in "    \(s)" })
        .joined(separator: "\n")
}

public indirect enum Expr: CustomStringConvertible {
    case Literal(Literal)
    case UnaryOp(UnaryOperator, Expr)
    case BinaryOp(Expr, BinaryOperator, Expr)
    case Parens(Expr)
    case Var(String)
    case Fun(args: [String], body: Expr)
    case Call(f: Expr, args: [Expr])
    case Block([Stmt], ret: Expr?)
    case If(cond: Expr, thenExpr: Expr, elseExpr: Expr?)
    case Assignment(Expr, AssignmentOperator, Expr)
    case Tuple([Expr])

    public var description: String {
        switch self {
        case let .Literal(lit):
            return lit.description
        case let .UnaryOp(op, expr):
            return op.description + expr.description
        case let .BinaryOp(lhs, op, rhs):
            return "\(lhs) \(op) \(rhs)"
        case let .Parens(expr):
            return "(\(expr))"
        case let .Var(v):
            return v
        case let .Fun(args, body):
            if args.count == 1 {
                return "\(args[0]) -> \(body)"
            }

            return "(\(args.joined(separator: ", "))) -> \(body)"
        case let .Call(f, args):
            return "\(f)(\(args.map({ e in e.description }).joined(separator: ", ")))"
        case let .Block(stmts, ret):
            if let ret = ret {
                if stmts.isEmpty {
                    return "{ \(ret) }"
                } else {
                    return
                        "{\n\(stmts.map({ s in indent(s.description) }).joined(separator: "\n"))\(ret.description)\n}"
                }
            } else {
                return "{\n\(stmts.map({ s in indent(s.description) }).joined(separator: "\n"))\n}"
            }
        case let .If(cond, thenExpr, elseExpr):
            if let elseExpr = elseExpr {
                return "if \(cond) \(thenExpr) else \(elseExpr)"
            } else {
                return "if \(cond) \(thenExpr)"
            }
        case let .Assignment(lhs, op, rhs):
            return "\(lhs) \(op) \(rhs)"
        case let .Tuple(exprs):
            return "(\(exprs.map({ e in e.description }).joined(separator: ", "))"
        }
    }
}

public enum Stmt: CustomStringConvertible {
    case Expr(Expr)
    case Let(mut: Bool, name: String, val: Expr)
    indirect case While(cond: Expr, body: [Stmt])
    indirect case For(name: String, iterator: Expr, body: [Stmt])
    case Return(Expr?)
    case Yield(Expr)
    case Break
    case Error(ParserError, span: (Int, Int))

    public var description: String {
        switch self {
        case let .Expr(expr):
            return expr.description
        case let .Let(mut: false, name, val):
            return "let \(name) = \(val)"
        case let .Let(mut: true, name, val):
            return "mut \(name) = \(val)"
        case let .While(cond, body):
            return
                "while \(cond) {\n\(body.map({ s in indent(s.description) }).joined(separator: "\n"))\n}"
        case let .For(name, iterator, body):
            return
                "for \(name) in \(iterator) {\n\(body.map({ s in indent(s.description) }).joined(separator: "\n"))\n}"
        case .Return(.none):
            return "return"
        case let .Return(.some(ret)):
            return "return \(ret)"
        case let .Yield(expr):
            return "yield \(expr)"
        case .Break:
            return "break"
        case let .Error(error, span: (start, end)):
            return "Error(\(error), span: (\(start), \(end)))"
        }
    }
}

//
//  JS.swift
//  
//
//  Created by nathan on 20/11/2022.
//

import Foundation

public indirect enum JSExpr: CustomStringConvertible {
    case boolean(Bool)
    case number(Float64)
    case undefined
    case string(String)
    case variable(String)
    case parens(JSExpr)
    case closure(args: [String], stmts: [JSStmt])
    case generator(args: [String], stmts: [JSStmt])
    case call(lhs: JSExpr, args: [JSExpr])
    case unaryOperation(UnaryOperator, JSExpr)
    case binaryOperation(JSExpr, BinaryOperator, JSExpr)
    case ternary(cond: JSExpr, thenExpr: JSExpr, elseExpr: JSExpr)
    case assignment(JSExpr, AssignmentOperator, JSExpr)
    case array([JSExpr])
    case object([(String, JSExpr)])
    case objectAccess(JSExpr, field: String)
    case objectPattern([(String, JSExpr?)])
    
    public var description: String {
        switch self {
        case let .boolean(q): return q ? "true" : "false"
        case let .number(x) where floor(x) == x: return "\(Int(x))"
        case let .number(x): return "\(x)"
        case .undefined: return "undefined"
        case let .string(s): return "\"\(s)\""
        case let .variable(v): return v
        case let .parens(expr): return "(\(expr))"
        case let .closure(args, stmts):
            let argsFmt = args.joined(separator: ", ")
            
            if stmts.count == 1, case .return_(let ret) = stmts.last, ret != nil {
                return "(\(argsFmt)) => \(ret!)"
            }
            
            return "(\(argsFmt)) => {\n\(stmts.map(indent).joined(separator: "\n"))\n}"
        case let .generator(args, stmts):
            let argsFmt = args.joined(separator: ", ")
            let bodyFmt = stmts.map(indent).joined(separator: "\n")
            return "function* (\(argsFmt)) {\n\(bodyFmt)\n}"
        case let .call(lhs, args): return "\(lhs)(\(args.map({ "\($0)" }).joined(separator: ", ")))"
        case let .unaryOperation(op, expr): return "\(op)\(expr)"
        case let .binaryOperation(lhs, op, rhs): return "\(lhs) \(op) \(rhs)"
        case let .ternary(cond, thenExpr, elseExpr): return "\(cond) ? \(thenExpr) : \(elseExpr)"
        case let .assignment(lhs, op, rhs): return "\(lhs) \(op) \(rhs)"
        case let .array(elems): return "[\(elems.map({ "\($0)" }).joined(separator: ", "))]"
        case let .object(entries) where entries.isEmpty: return "{}"
        case let .object(entries):
            return "{ \(entries.map({ (k, v) in "\(k): \(v)" }).joined(separator: ", ")) }"
        case let .objectAccess(obj, field):
            return "\(obj).\(field)"
        case let .objectPattern(entries):
            return "{ \(entries.map({ (k, p) in p != nil ? "\(k): \(p!)" : k }).joined(separator: ", ")) }"
        }
    }
}

public enum JSStmt: CustomStringConvertible {
    case expr(JSExpr)
    case varDecl(mut: Bool, JSExpr, JSExpr)
    case whileLoop(cond: JSExpr, body: [JSStmt])
    case forOfLoop(ident: String, of: JSExpr, body: [JSStmt])
    case ifThen(cond: JSExpr, body: [JSStmt])
    case return_(JSExpr?)
    case yield(JSExpr)
    case break_
    
    public var description: String {
        switch self {
        case let .expr(expr): return "\(expr);"
        case let .varDecl(mut, name, val): return "\(mut ? "let" : "const") \(name) = \(val);"
        case let .ifThen(cond, body):
            return "if (\(cond)) \("{\n\(body.map(indent).joined(separator: "\n"))\n}")"
        case let .whileLoop(cond, body):
            return "while (\(cond)) \("{\n\(body.map(indent).joined(separator: "\n"))\n}")"
        case let .forOfLoop(ident, of, body):
            return "for (const \(ident) of \(of)) \("{\n\(body.map(indent).joined(separator: "\n"))\n}")"
        case let .return_(ret):
            if let ret {
                return "return \(ret);"
            } else {
                return "return;"
            }
        case let .yield(expr): return "yield \(expr);"
        case .break_: return "break"
        }
    }
}

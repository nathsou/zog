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
    case closure(args: [JSExpr], stmts: [JSStmt])
    case generator(args: [JSExpr], stmts: [JSStmt])
    case call(lhs: JSExpr, args: [JSExpr])
    case unaryOperation(UnaryOperator, JSExpr)
    case binaryOperation(JSExpr, BinaryOperator, JSExpr)
    case ternary(cond: JSExpr, thenExpr: JSExpr, elseExpr: JSExpr)
    case assignment(JSExpr, AssignmentOperator, JSExpr)
    case array([JSExpr])
    case arraySubscript(JSExpr, JSExpr)
    case object([(String, JSExpr)])
    case objectAccess(JSExpr, field: String)
    case objectPattern([(String, JSExpr?)])
    case raw(String)
    
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
            let argsFmt = args.map({ "\($0)" }).joined(separator: ", ")
            
            if stmts.count == 1, case .return_(let ret) = stmts.last, ret != nil {
                if case .object(_) = ret {
                   return "(\(argsFmt)) => (\(ret!))"
                } else {
                    return "(\(argsFmt)) => \(ret!)"
                }
            }
            
            return "(\(argsFmt)) => {\n\(stmts.map(indent).joined(separator: "\n"))\n}"
        case let .generator(args, stmts):
            let argsFmt = args.map({ "\($0)" }).joined(separator: ", ")
            let bodyFmt = stmts.map(indent).joined(separator: "\n")
            return "function* (\(argsFmt)) {\n\(bodyFmt)\n}"
        case let .call(lhs, args): return "\(lhs)(\(args.map({ "\($0)" }).joined(separator: ", ")))"
        case let .unaryOperation(op, expr): return "\(op)\(expr)"
        case let .binaryOperation(lhs, op, rhs) where op == .equ: return "\(lhs) === \(rhs)"
        case let .binaryOperation(lhs, op, rhs): return "\(lhs) \(op) \(rhs)"
        case let .ternary(cond, thenExpr, elseExpr): return "\(cond) ? \(thenExpr) : \(elseExpr)"
        case let .assignment(lhs, op, rhs): return "\(lhs) \(op) \(rhs)"
        case let .array(elems): return "[\(elems.map({ "\($0)" }).joined(separator: ", "))]"
        case let .arraySubscript(elems, index): return "\(elems)[\(index)]"
        case let .object(entries) where entries.isEmpty: return "{}"
        case let .object(entries):
            return "{ \(entries.map({ (k, v) in "\(k): \(v)" }).joined(separator: ", ")) }"
        case let .objectAccess(obj, field):
            return "\(obj).\(field)"
        case let .objectPattern(entries):
            return "{ \(entries.map({ (k, p) in p != nil ? "\(k): \(p!)" : k }).joined(separator: ", ")) }"
        case let .raw(js):
            return js
        }
    }
}

public enum JSStmt: CustomStringConvertible {
    case expr(JSExpr)
    case varDecl(mut: Bool, JSExpr, JSExpr)
    case whileLoop(cond: JSExpr, body: [JSStmt])
    case forOfLoop(pat: JSExpr, in: JSExpr, body: [JSStmt])
    case if_(cond: JSExpr, then: [JSStmt], else_: [JSStmt]?)
    case return_(JSExpr?)
    case yield(JSExpr)
    case break_
    case switch_(subject: JSExpr, cases: [(JSExpr, [JSStmt])], defaultCase: [JSStmt]?)
    
    public var description: String {
        switch self {
        case let .expr(expr): return "\(expr);"
        case let .varDecl(mut, name, val):
            if case .undefined = val {
                return "\(mut ? "let" : "const") \(name);"
            } else {
                return "\(mut ? "let" : "const") \(name) = \(val);"
            }
        case let .if_(cond, then, else_) where else_ != nil:
            let thenFmt = "{\n\(then.map(indent).joined(separator: "\n"))\n}"
            let elseFmt = "{\n\(else_!.map(indent).joined(separator: "\n"))\n}"
            return "if (\(cond)) \(thenFmt) else \(elseFmt)"
        case let .if_(cond, then, _):
            return "if (\(cond)) \("{\n\(then.map(indent).joined(separator: "\n"))\n}")"
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
        case .break_: return "break;"
        case let .switch_(subject, cases, defaultCase):
            let showBody = { (stmts: [JSStmt]) in "{\n\(stmts.map({ indent($0) }).joined(separator: "\n"))\n}" }
            
            var tests = cases.map({ (val, stmts) in "case \(val): \(showBody(stmts))"})
            
            if let defaultCase {
                tests.append("default: \(showBody(defaultCase))")
            }
            
            return "switch (\(subject)) {\n\(indent(tests.joined(separator: "\n")))\n}"
        }
    }
}

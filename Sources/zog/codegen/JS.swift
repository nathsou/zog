//
//  JS.swift
//  
//
//  Created by nathan on 20/11/2022.
//

import Foundation

let specialChars = Set<Character>(["\"", "\n"])

indirect enum JSExpr: CustomStringConvertible {
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
    case typeof(JSExpr)
    case raw(String)
    case dynamicImport(String)
    
    func wrap() -> JSExpr {
        if case .object(_) = self {
            return .parens(self)
        }
        
        return self
    }
    
    func isUndefined() -> Bool {
        if case .undefined = self {
            return true
        }
        
        return false
    }
    
    public var description: String {
        switch self {
        case let .boolean(q): return q ? "true" : "false"
        case let .number(x) where floor(x) == x: return "\(Int(x))"
        case let .number(x): return "\(x)"
        case .undefined: return "undefined"
        case let .string(s):
            if s.contains(where: { c in specialChars.contains(c) }) {
                return "`\(s)`"
            } else {
                return "\"\(s)\""
            }
        case let .variable(v): return v
        case let .parens(expr): return "(\(expr))"
        case let .closure(args, stmts):
            let argsFmt = args.map({ "\($0)" }).joined(separator: ", ")
            
            if stmts.count == 1, case .return_(let ret?) = stmts.last {
                return "(\(argsFmt)) => \(ret.wrap())"
            }
            
            return "(\(argsFmt)) => {\n\(stmts.map(indent).joined(separator: "\n"))\n}"
        case let .generator(args, stmts):
            let argsFmt = args.map({ "\($0)" }).joined(separator: ", ")
            let bodyFmt = stmts.map(indent).joined(separator: "\n")
            return "function* (\(argsFmt)) {\n\(bodyFmt)\n}"
        case let .call(lhs, args): return "\(lhs)(\(args.map({ "\($0)" }).joined(separator: ", ")))"
        case let .unaryOperation(op, expr): return "\(op)\(expr)"
        case let .binaryOperation(lhs, op, rhs) where op == .equ: return "\(lhs) === \(rhs)"
        case let .binaryOperation(lhs, op, rhs) where op == .neq: return "\(lhs) !== \(rhs)"
        case let .binaryOperation(lhs, op, rhs) where op == .concat: return "\(lhs) + \(rhs)"
        case let .binaryOperation(lhs, op, rhs): return "\(lhs) \(op) \(rhs)"
        case let .ternary(cond, .boolean(true), .boolean(false)): return "\(cond)"
        case let .ternary(cond, .boolean(false), .boolean(true)): return "!\(JSExpr.parens(cond))"
        case let .ternary(cond, thenExpr, elseExpr):
            return "\(cond) ? \(thenExpr.wrap()) : \(elseExpr.wrap())"
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
        case let .typeof(expr):
            return "typeof \(expr)"
        case let .dynamicImport(path):
            return "import('\(path)')"
        case let .raw(js):
            return js
        }
    }
}

enum JSStmt: CustomStringConvertible {
    case expr(JSExpr)
    case varDecl(export: Bool, const: Bool, JSExpr, JSExpr)
    case whileLoop(cond: JSExpr, body: [JSStmt])
    case forOfLoop(pat: JSExpr, in: JSExpr, body: [JSStmt])
    case forLoop(name: String, start: JSExpr, end: JSExpr, step: JSExpr, body: [JSStmt])
    case if_(cond: JSExpr, then: [JSStmt], else_: [JSStmt]?)
    case return_(JSExpr?)
    case yield(JSExpr)
    case break_
    case switch_(subject: JSExpr, cases: [(JSExpr, [JSStmt])], defaultCase: [JSStmt]?)
    case importDecl(members: [String], path: String)
    
    public var description: String {
        switch self {
        case let .expr(expr): return "\(expr);"
        case let .varDecl(export, const, name, val):
            return "export ".when(export) + "\(const ? "const" : "let") \(name)" + " = \(val)".when(!val.isUndefined()) + ";"
        case let .if_(cond, then, else_?) where !else_.isEmpty:
            let thenFmt = "{\n\(then.map(indent).joined(separator: "\n"))\n}"
            let elseFmt = "{\n\(else_.map(indent).joined(separator: "\n"))\n}"
            return "if (\(cond)) \(thenFmt) else \(elseFmt)"
        case let .if_(cond, then, _):
            return "if (\(cond)) \("{\n\(then.map(indent).joined(separator: "\n"))\n}")"
        case let .whileLoop(cond, body):
            return "while (\(cond)) \("{\n\(body.map(indent).joined(separator: "\n"))\n}")"
        case let .forOfLoop(ident, of, body):
            return "for (const \(ident) of \(of)) \("{\n\(body.map(indent).joined(separator: "\n"))\n}")"
        case let .forLoop(name, start, end, step, body):
            return "for (let \(name) = \(start); \(name) < \(end); \(name) += \(step)) \("{\n\(body.map(indent).joined(separator: "\n"))\n}")"
        case let .return_(ret):
            if let ret {
                return "return \(ret);"
            } else {
                return "return;"
            }
        case let .yield(expr): return "yield \(expr);"
        case .break_: return "break;"
        case let .switch_(subject, cases, defaultCase):
            let showBody = { (stmts: [JSStmt]) in "{\n\(stmts.map({ indent($0) }).newlines())\n}" }
            
            var tests = cases.map({ (val, stmts) in "case \(val): \(showBody(stmts))"})
            
            if let defaultCase {
                tests.append("default: \(showBody(defaultCase))")
            }
            
            return "switch (\(subject)) {\n\(indent(tests.newlines()))\n}"
        case .importDecl([], _):
            return "";
        case let .importDecl(members, path):
            return "import { \(members.commas()) } from \"\(path)\";"
        }
    }
}

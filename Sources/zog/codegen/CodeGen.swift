//
//  CodeGen.swift
//  
//
//  Created by nathan on 20/11/2022.
//

import Foundation

public class CoreContext {
    let parent: CoreContext?
    var statements = [JSStmt]()
    var linearVarCounts = [String:Int]()
    var scopeVariables = [String:String]()
    
    init(parent: CoreContext? = nil) {
        self.parent = parent
    }
    
    public func child() -> CoreContext {
        return .init(parent: self)
    }
    
    public func declare(_ name: String) -> String {
        let newName = declareLinear(name)
        scopeVariables[name] = newName
        return newName
    }
    
    public func lookup(_ name: String) throws -> String {
        if let newName = scopeVariables[name] {
            return newName
        }
        
        if let parent {
            return try parent.lookup(name)
        }
        
        throw TypeError.unknownVariable(name)
    }
    
    func declareLinear(_ name: String) -> String {
        if let parent {
            return parent.declareLinear(name)
        } else {
            let newCount = (linearVarCounts[name] ?? 0) + 1
            linearVarCounts.updateValue(newCount, forKey: name)
            return formatVarName(name, count: newCount)
        }
    }
    
    func formatVarName(_ name: String, count: Int) -> String {
        if count == 1 {
            return name
        }
        
        return "\(name)$\(count)"
    }
}

extension CoreExpr {
    public func codegen(_ ctx: CoreContext) throws -> JSExpr {
        switch self {
        case .Literal(.unit, _): return .undefined
        case let .Literal(.num(x), _): return .number(x)
        case let .Literal(.bool(q), _): return .boolean(q)
        case let .Literal(.str(s), _): return .string(s)
        case let .UnaryOp(op, expr, _): return .unaryOperation(op, try expr.codegen(ctx))
        case let .BinaryOp(lhs, op, rhs, _):
            return .binaryOperation(try lhs.codegen(ctx), op, try rhs.codegen(ctx))
        case let .Parens(expr, _): return .parens(try expr.codegen(ctx))
        case let .Var(name, _): return .variable(try ctx.lookup(name))
        case let .Fun(args, body, isIterator, _):
            let funCtx = ctx.child()
            let newArgs = args.map({ arg in funCtx.declare(arg) })
            let ret = try body.codegen(funCtx)
            
            if case .undefined = ret {} else {
                funCtx.statements.append(.return_(ret))
            }
            
            if isIterator {
                return .generator(args: newArgs, stmts: funCtx.statements)
            } else {
                return .closure(args: newArgs, stmts: funCtx.statements)
            }
        case let .Call(f, args, _):
            return .call(lhs: try f.codegen(ctx), args: try args.map({ try $0.codegen(ctx) }))
        case let .Block(stmts, ret, _):
            let blockCtx = ctx.child()
            
            for stmt in stmts {
                blockCtx.statements.append(try stmt.codegen(blockCtx))
            }
            
            let ret = try ret?.codegen(blockCtx)
            
            ctx.statements.append(contentsOf: blockCtx.statements)
            
            return ret ?? .undefined
        case let .If(cond, thenExpr, elseExpr, _):
            return .ternary(
                cond: try cond.codegen(ctx),
                thenExpr: try thenExpr.codegen(ctx),
                elseExpr: try elseExpr.codegen(ctx)
            )
        case let .Assignment(lhs, op, rhs, _):
            return .assignment(try lhs.codegen(ctx), op, try rhs.codegen(ctx))
        case let .Tuple(elems, _): return .array(try elems.map({ try $0.codegen(ctx) }))
        }
    }
}

extension CoreStmt {
    public func codegen(_ ctx: CoreContext) throws -> JSStmt {
        switch self {
        case let .Expr(expr):
            return .expr(try expr.codegen(ctx))
        case let .Let(mut: false, name, val):
            return .constDecl(ctx.declare(name), try val.codegen(ctx))
        case let .Let(mut: true, name, val):
            return .letDecl(ctx.declare(name), try val.codegen(ctx))
        case let .IfThen(cond, body):
            let bodyCtx = ctx.child()
            try body.forEach({ stmt in bodyCtx.statements.append(try stmt.codegen(bodyCtx)) })
            return .ifThen(cond: try cond.codegen(ctx), body: bodyCtx.statements)
        case let .While(cond, body):
            let bodyCtx = ctx.child()
            try body.forEach({ stmt in bodyCtx.statements.append(try stmt.codegen(bodyCtx)) })
            return .whileLoop(cond: try cond.codegen(ctx), body: bodyCtx.statements)
        case let .For(name, iterator, body):
            let bodyCtx = ctx.child()
            let newName = bodyCtx.declare(name)
            try body.forEach({ stmt in bodyCtx.statements.append(try stmt.codegen(bodyCtx)) })
            return .forOfLoop(ident: newName, of: try iterator.codegen(ctx), body: bodyCtx.statements)
        case let .Return(expr):
            return .return_(try expr?.codegen(ctx))
        case let .Yield(expr):
            return .yield(try expr.codegen(ctx))
        case .Break:
            return .break_
        }
    }
}
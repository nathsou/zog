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

extension Literal {
    func codegen() -> JSExpr {
        switch self {
        case .unit: return .undefined
        case let .num(x): return .number(x)
        case let .bool(q): return .boolean(q)
        case let .str(s): return .string(s)
        }
    }
}

extension CoreExpr {
    public func codegen(_ ctx: CoreContext) throws -> JSExpr {
        switch self {
        case let .Literal(lit, _): return lit.codegen()
        case let .UnaryOp(op, expr, _): return .unaryOperation(op, try expr.codegen(ctx))
        case let .BinaryOp(lhs, op, rhs, _):
            return .binaryOperation(try lhs.codegen(ctx), op, try rhs.codegen(ctx))
        case let .Parens(expr, _): return .parens(try expr.codegen(ctx))
        case let .Var(name, _): return .variable(try ctx.lookup(name))
        case let .Fun(args, _, body, isIterator, _):
            let funCtx = ctx.child()
            let newArgs = try args.map({ (arg, _) in try arg.codegen(ctx) })
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
        case let .Tuple(elems, _), let .Array(elems, _):
            return .array(try elems.map({ try $0.codegen(ctx) }))
        case let .ArraySubscript(elems, index, _):
            return .arraySubscript(try elems.codegen(ctx), try index.codegen(ctx))
        case let .Record(entries, _):
            return .object(try entries.map({ (k, v) in (k, try v.codegen(ctx)) }))
        case let .RecordSelect(record, field, _):
            return .objectAccess(try record.codegen(ctx), field: field)
        case let .Raw(js, _):
            return .raw(js)
        case let .Match(expr, cases, ty):
            let dt = DecisionTree.from(exprTy: expr.ty, cases: cases)
            
            if case .Var(_, _) = expr {
                return try dt.rewrite(subject: expr, returnTy: ty).codegen(ctx)
            } else {
                let subjectVarName = ctx.declare("subject")
                return try CoreExpr.Block(
                    [.Let(mut: false, pat: .variable(subjectVarName), ty: expr.ty, val: expr)],
                    ret: dt.rewrite(subject: .Var(subjectVarName, ty: expr.ty), returnTy: ty),
                    ty: ty
                ).codegen(ctx)
            }
        case let .Switch(expr, cases, defaultCase, _):
            let result = JSExpr.variable(ctx.declare("result"))
            ctx.statements.append(.varDecl(mut: true, result, .undefined))
            
            let codegenBody = { (body: CoreExpr) throws -> [JSStmt] in
                let bodyCtx = ctx.child()
                let rhs = try body.codegen(bodyCtx)
                return bodyCtx.statements + [
                    .expr(.assignment(result, .eq, rhs)),
                    .break_
                ]
            }
            
            ctx.statements.append(.switch_(
                subject: try expr.codegen(ctx),
                cases: try cases.map({ (val, body) in (try val.codegen(ctx), try codegenBody(body))}),
                defaultCase: defaultCase != nil ? try codegenBody(defaultCase!) : nil
            ))
            
            return result
        }
    }
}

extension CoreStmt {
    public func codegen(_ ctx: CoreContext) throws -> JSStmt {
        switch self {
        case let .Expr(expr):
            return .expr(try expr.codegen(ctx))
        case let .Let(_, pat: .any, _, val):
            return try .expr(val.codegen(ctx))
        case let .Let(mut, pat, ty: _, val):
            return .varDecl(mut: mut, try pat.codegen(ctx), try val.codegen(ctx))
        case let .IfThen(cond, body):
            let bodyCtx = ctx.child()
            try body.forEach({ stmt in bodyCtx.statements.append(try stmt.codegen(bodyCtx)) })
            return .ifThen(cond: try cond.codegen(ctx), body: bodyCtx.statements)
        case let .While(cond, body):
            let bodyCtx = ctx.child()
            try body.forEach({ stmt in bodyCtx.statements.append(try stmt.codegen(bodyCtx)) })
            return .whileLoop(cond: try cond.codegen(ctx), body: bodyCtx.statements)
        case let .For(pat, iterator, body):
            let bodyCtx = ctx.child()
            let corePat = try pat.codegen(ctx)
            try body.forEach({ stmt in bodyCtx.statements.append(try stmt.codegen(bodyCtx)) })
            return .forOfLoop(
                pat: corePat,
                in: try iterator.codegen(ctx),
                body: bodyCtx.statements
            )
        case let .Return(expr):
            return .return_(try expr?.codegen(ctx))
        case let .Yield(expr):
            return .yield(try expr.codegen(ctx))
        case .Break:
            return .break_
        }
    }
}

extension CorePattern {
    public func codegen(_ ctx: CoreContext) throws -> JSExpr {
        switch self {
        case .any:
            return .variable(ctx.declare("_"))
        case let .variable(name):
            return .variable(ctx.declare(name))
        case let .literal(lit):
            assertionFailure("pattern is not infallible")
            return try CoreExpr.Literal(lit, ty: lit.ty).codegen(ctx)
        case let .tuple(patterns):
            return .array(try patterns.map({ try $0.codegen(ctx) }))
        case let .record(entries):
            var objectEntries = [(String, JSExpr?)]()
            
            for (key, pat) in entries {
                let rhs: JSExpr?
                
                if let pat {
                    rhs = try pat.codegen(ctx)
                } else {
                    let renaming = ctx.declare(key)
                                               
                    if renaming == key {
                       rhs = nil
                    } else {
                        rhs = .variable(renaming)
                    }
                }
                
                objectEntries.append((key, rhs))
            }
            
            return .objectPattern(objectEntries)
        }
    }
}

func caseCondition(subject: CoreExpr, ctor: Ctor) -> CoreExpr? {
    switch ctor {
    case .literal(let lit):
        return .BinaryOp(subject, .equ, .Literal(lit, ty: lit.ty), ty: .bool)
    default:
        return nil
    }
}

extension DecisionTree {
    func rewrite(subject: CoreExpr, returnTy: Ty) -> CoreExpr {
        func aux(_ dt: DecisionTree, subject: CoreExpr) -> CoreExpr {
            switch dt {
            case .fail:
                return .Raw(js: "(() => { throw new Error('Pattern matching failed'); })()", ty: .unit)
            case let .leaf(action):
                return action
            case let .switch_(path, cases, defaultCase):
                let proj = subject.at(path: path)
                var tests = [(CoreExpr, CoreExpr)]()
                
                for (ctor, dt) in cases {
                    if case .literal(let lit) = ctor {
                        tests.append((.Literal(lit, ty: lit.ty), dt.rewrite(subject: subject, returnTy: returnTy)))
                    }
                }
                
                if tests.isEmpty {
                    if !cases.isEmpty {
                        return cases[0].dt.rewrite(subject: subject, returnTy: returnTy)
                    } else if defaultCase != nil {
                        return defaultCase!.rewrite(subject: subject, returnTy: returnTy)
                    }
                }
                
                return .Switch(
                    proj,
                    cases: tests,
                    defaultCase: defaultCase?.rewrite(subject: subject, returnTy: returnTy),
                    ty: returnTy
                )
            }
        }
        
        return aux(self, subject: subject)
    }
}

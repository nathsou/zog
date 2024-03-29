//
//  CodeGen.swift
//  
//
//  Created by nathan on 20/11/2022.
//

import Foundation

let reservedIdentifiers = Set(["eval", "throw", "Infinity"])

class CoreContext {
    let env: TypeEnv
    let parent: CoreContext?
    var statements = [JSStmt]()
    var rootVariables: Ref<[String:Int]>
    var localVariables = [String:String]()
    
    init(parent: CoreContext? = nil, env: TypeEnv) {
        self.parent = parent
        self.rootVariables = parent?.rootVariables ?? Ref([:])
        self.env = env
    }
    
    func child() -> CoreContext {
        return .init(parent: self, env: env)
    }
    
    func declareMeta(_ name: String) -> String {
        let uniqueName = "$" + name + (rootVariables.ref[name].map({ String($0)} ) ?? "")
        return declare(uniqueName)
    }

    func declare(_ name: String) -> String {
        if reservedIdentifiers.contains(name) {
            return declareMeta(name)
        }

        if localVariables.keys.contains(name) {
            fatalError("Variable \(name) already declared")
        }

        if let count = rootVariables.ref[name] {
            rootVariables.ref[name] = count + 1
        } else {
            rootVariables.ref[name] = 1
        }

        let linearName = formatVarName(name, count: rootVariables.ref[name]!)
        localVariables[name] = linearName
        return linearName
    }
    
    func lookup(_ name: String) throws -> String {
        if reservedIdentifiers.contains(name) {
            return try lookup("$\(name)")
        }

        if let linearName = localVariables[name] {
            return linearName
        } else if let parent {
            return try parent.lookup(name)
        } else {
            throw TypeError.unknownVariable(name)
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
    func codegen(_ ctx: CoreContext) throws -> JSExpr {
        switch self {
        case let .Literal(lit, _): return lit.codegen()
        case let .UnaryOp(op, expr, _): return .unaryOperation(op, try expr.codegen(ctx))
        case let .BinaryOp(lhs, op, rhs, _):
            return .binaryOperation(try lhs.codegen(ctx), op, try rhs.codegen(ctx))
        case let .Parens(expr, _): return .parens(try expr.codegen(ctx))
        case let .Var(name, _): return .variable(try ctx.lookup(name))
        case let .Fun(modifier, args, _, body, _):
            let funCtx = ctx.child()
            let newArgs = try args.map({ (arg, _) in try arg.codegen(funCtx) })
            let ret = try body.codegen(funCtx)
            
            if case .undefined = ret {
            } else if case .unit = body.ty.deref() {
                funCtx.statements.append(.expr(ret))
            } else {
                funCtx.statements.append(.return_(ret))
            }

            switch modifier {
                case .iterator:
                return .generator(args: newArgs, stmts: funCtx.statements)
                case .fun:
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
        case let .If(cond, then, else_, ty):
            if case let .Expr(retThen) = then.last, case let .Expr(retElse) = else_.last {
                let resultName = "$result"
                let resultVar = CoreExpr.Var(resultName, ty: ty)
                
                let thenStmts: [CoreStmt] = then.dropLast(1) + [
                    .Expr(.Assignment(resultVar, .eq, retThen, ty: ty))
                ]
                
                let elseStmts: [CoreStmt] = else_.dropLast(1) + [
                    .Expr(.Assignment(resultVar, .eq, retElse, ty: ty))
                ]
                
                return try CoreExpr.Block([
                    .Let(mut: true, pat: .variable(resultName), ty: ty, val: .Literal(.unit, ty: .unit)),
                    .If(cond: cond, then: thenStmts, else_: elseStmts)
                ], ret: resultVar, ty: ty).codegen(ctx)
            }
            
            ctx.statements.append(try CoreStmt.If(cond: cond, then: then, else_: else_).codegen(ctx))
            
            return .undefined
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
            let dt = try DecisionTree.from(
                exprTy: expr.ty,
                cases: cases,
                env: ctx.env
            )
            
            if case .Var(_, _) = expr {
                return try dt.rewrite(
                    subject: expr,
                    returnTy: ty,
                    patterns: cases.map({ $0.pattern }),
                    ctx: ctx
                ).codegen(ctx)
            } else {
                let subjectVarName = "$subject" 
                return try CoreExpr.Block(
                    [.Let(mut: false, pat: .variable(subjectVarName), ty: expr.ty, val: expr)],
                    ret: dt.rewrite(
                        subject: .Var(subjectVarName, ty: expr.ty),
                        returnTy: ty,
                        patterns: cases.map({ $0.pattern }),
                        ctx: ctx
                    ),
                    ty: ty
                ).codegen(ctx)
            }
        case let .Switch(expr, cases, defaultCase, ty):
           if cases.count == 1 {
               let (test, action) = cases[0]

                if let defaultCase {
                    return try CoreExpr.If(
                        cond: .BinaryOp(expr, .equ, test, ty: .bool),
                        then: [.Expr(action)],
                        else_: [.Expr(defaultCase)],
                        ty: ty
                    ).codegen(ctx)
                }

                return try action.codegen(ctx)
           } else {
                let resultName = ctx.declare("$result")
                ctx.statements.append(.varDecl(
                    export: false,
                    const: false,
                    .variable(resultName),
                    .undefined
                ))
                
                let codegenBody = { (body: CoreExpr) throws -> [JSStmt] in
                    let bodyCtx = ctx.child()
                    let rhs = try body.codegen(bodyCtx)
                    return bodyCtx.statements + [
                        .expr(.assignment(.variable(resultName), .eq, rhs)),
                        .break_
                    ]
                }
                
                ctx.statements.append(.switch_(
                    subject: try expr.codegen(ctx),
                    cases: try cases.map({ (val, body) in (try val.codegen(ctx), try codegenBody(body))}),
                    defaultCase: try defaultCase.map({ try codegenBody($0) })
                ))
                
                return .variable(resultName)
           }
        case let .Variant(enumName, variantName, val, _):
            let enum_ = ctx.env.enums[enumName.ref!]!
            let tag = JSExpr.number(Float64(enum_.variants.mapping[variantName]!.id))
            
            if let val {
                return .object([
                    ("variant", tag),
                    ("value", try val.codegen(ctx))
                ])
            } else {
                return tag
            }
        case let .BuiltInCall(name, args, _):
            return try BuiltIn.codegen(name: name, args: args, ctx: ctx)
        }
    }
}

extension CoreStmt {
    func codegen(_ ctx: CoreContext) throws -> JSStmt {
        switch self {
        case let .Expr(.If(cond, then, else_, _)):
            return try CoreStmt.If(cond: cond, then: then, else_: else_).codegen(ctx)
        case let .Expr(expr):
            return .expr(try expr.codegen(ctx))
        case let .Let(_, pat: .any, _, val):
            return try .expr(val.codegen(ctx))
        case let .Let(mut, pat, ty: _, val):
            return .varDecl(export: false, const: !mut, try pat.codegen(ctx), try val.codegen(ctx))
        case let .If(cond, then, else_):
            let thenCtx = ctx.child()
            try then.forEach({ stmt in thenCtx.statements.append(try stmt.codegen(thenCtx)) })
            
            var elseStmts: [JSStmt]? = nil
            
            if let else_ {
                let elseCtx = ctx.child()
                try else_.forEach({ stmt in elseCtx.statements.append(try stmt.codegen(elseCtx)) })
                elseStmts = elseCtx.statements
            }
            
            return .if_(
                cond: try cond.codegen(ctx),
                then: thenCtx.statements,
                else_: elseStmts
            )
        case let .While(cond, body):
            let bodyCtx = ctx.child()
            try body.forEach({ stmt in bodyCtx.statements.append(try stmt.codegen(bodyCtx)) })
            return .whileLoop(cond: try cond.codegen(ctx), body: bodyCtx.statements)
        case let .For(pat, iterator, body):
            let bodyCtx = ctx.child()
            let corePat = try pat.codegen(ctx)
            try body.forEach({ stmt in bodyCtx.statements.append(try stmt.codegen(bodyCtx)) })
            
            if case .const("Array", _) = iterator.ty.deref(), case .variable(let name) = pat {
                let iVar = ctx.declareMeta("i");
                let iterVal: JSExpr = try iterator.codegen(ctx)
                let iterVar: JSExpr
                
                if case .variable(let name) = iterVal {
                    iterVar = .variable(name)
                } else {
                    iterVar = .variable(ctx.declareMeta("elems"))
                    ctx.statements.append(.varDecl(export: false, const: true, iterVar, iterVal))
                }
                
                let len = JSExpr.objectAccess(iterVar, field: "length")
                let lenVar = JSExpr.variable(ctx.declareMeta("len"))
                ctx.statements.append(.varDecl(export: false, const: true, lenVar, len))
                bodyCtx.statements.insert(
                    .varDecl(export: false, const: false, .variable(name), .arraySubscript(iterVar, .variable(iVar))),
                    at: 0
                )
                
                return .forLoop(
                    name: iVar,
                    start: .number(0),
                    end: lenVar,
                    step: .number(1),
                    body: bodyCtx.statements
                )
            }
            
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

extension CoreDecl {
    func codegen(_ ctx: CoreContext) throws -> [JSStmt] {
        switch self {
        case let .Let(pub, mut, pat, _, val):
            return [.varDecl(export: pub, const: !mut, try pat.codegen(ctx), try val.codegen(ctx))]
        case let .Stmt(stmt):
            return [try stmt.codegen(ctx)]
        case .TypeAlias(_, _, _, _):
            return []
        case .Enum(_, _, _, _):
            return []
        case let .Declare(_, name, _):
            _ = ctx.declare(name)
            return []
        case let .Import(path, members):
            let variables = members?.filter({ name in ctx.env.vars.keys.contains(name) }) ?? []
            for member in variables {
                _ = ctx.declare(member)
            }

            assert(path.suffix(4) == ".zog")
            let zogPath = String(path.dropLast(4) + ".mjs") 
            
            return [
                .importDecl(
                    members: variables, 
                    path: zogPath
                )
            ]
        }
    }
}

extension CorePattern {
    func codegen(_ ctx: CoreContext) throws -> JSExpr {
        switch self {
        case .any:
            return .variable(ctx.declare("_"))
        case let .variable(name):
            return .variable(ctx.declare(name))
        case .literal(_), .variant(_, _, _):
            fatalError("pattern is not infallible")
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

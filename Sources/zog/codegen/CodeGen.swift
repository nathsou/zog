//
//  CodeGen.swift
//  
//
//  Created by nathan on 20/11/2022.
//

import Foundation

class CoreContext {
    let env: TypeEnv
    let parent: CoreContext?
    var statements = [JSStmt]()
    var variables = Set<String>()
    let isLinear: Bool
    
    init(parent: CoreContext? = nil, linear: Bool, env: TypeEnv) {
        self.parent = parent
        self.isLinear = linear
        self.env = env
    }
    
    func child(linear: Bool) -> CoreContext {
        return .init(parent: self, linear: linear, env: env)
    }
    
    private func countVars(name: String) -> Int {
        if let parent {
            return parent.countVars(name: name) + (variables.contains(name) ? 1 : 0)
        }
        
        return variables.contains(name) ? 1 : 0
    }
    
    func declareUnique(_ name: String) -> String {
        var uniqueName = "$" + name
        var index = 0
        
        while variables.contains(uniqueName) {
            index += 1
            uniqueName = "\(name)\(index)"
        }
        
        return declare(uniqueName)
    }
    
    func declare(_ name: String) -> String {
        variables.insert(name)
        
        let count = countVars(name: name)
        return formatVarName(name, count: count)
    }
    
    func lookup(_ name: String) throws -> String {
        let count = countVars(name: name)
        
        if count == 0 {
            throw TypeError.unknownVariable(name)
        }
        
        return formatVarName(name, count: count)
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
        case let .Fun(args, _, body, isIterator, _):
            let funCtx = ctx.child(linear: false)
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
            let blockCtx = ctx.child(linear: true)
            
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
                elseExpr: try elseExpr?.codegen(ctx) ?? .undefined
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
                let subjectVarName = ctx.declare("subject")
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
        case let .Switch(expr, cases, defaultCase, _):
            if let defaultCase, cases.count == 1 {
                let (test, action) = cases[0]
                return .ternary(
                    cond: .binaryOperation(
                        try expr.codegen(ctx),
                        .equ,
                        try test.codegen(ctx)
                    ),
                    thenExpr: try action.codegen(ctx),
                    elseExpr: try defaultCase.codegen(ctx)
                )
            } else {
                let result = JSExpr.variable(ctx.declare("result"))
                ctx.statements.append(.varDecl(mut: true, result, .undefined))
                
                let codegenBody = { (body: CoreExpr) throws -> [JSStmt] in
                    let bodyCtx = ctx.child(linear: false)
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
        case let .Variant(enumName, variantName, val, _):
            let enum_ = ctx.env.enums[enumName.ref!]!
            let tag = JSExpr.number(Float64(enum_.variants.mapping[variantName]!.id))
            
            if let val {
                return .object([
                    ("variant", tag),
                    ("value", try val.codegen(ctx))
                ])
            } else {
                return .object([("variant", tag)])
            }
        case let .BuiltInCall(name, args, _):
            switch name {
            case "type":
                let ZogType = Ty.const("Type", [])
                
                func typeVariant(_ name: String, val: CoreExpr? = nil) -> CoreExpr {
                    return .Variant(
                        enumName: Ref("Type"),
                        variantName: name,
                        val: val,
                        ty: ZogType
                    )
                }
                
                var tyVarIdMapping = [TyVarId:String]()
                
                func tyVarName(_ id: TyVarId) -> String {
                    if let name = tyVarIdMapping[id] {
                        return name
                    }
                    
                    let count = tyVarIdMapping.count
                    let name = TyVar.showTyVarId(UInt(count))
                    tyVarIdMapping[id] = name
                    return name
                }
                
                func typeOf(_ ty: Ty) -> CoreExpr {
                    switch ty {
                    case let .variable(v):
                        switch v.ref {
                        case let .generic(id):
                            return typeVariant(
                                "Variable",
                                val: .Literal(.str(tyVarName(id).lowercased()), ty: .str)
                            )
                        case let .link(to):
                            return typeOf(to)
                        case let .unbound(id, _):
                            return typeVariant(
                                "Variable",
                                val: .Literal(.str(tyVarName(id)), ty: .str)
                            )
                        }
                    case .const("bool", []):
                        return typeVariant("Bool")
                    case .const("str", []):
                        return typeVariant("Str")
                    case .const("num", []):
                        return typeVariant("Num")
                    case let .const("Tuple", args):
                        return typeVariant("Tuple", val: .Array(args.map(typeOf), ty: .array(ZogType)))
                    case let .const("Array", args) where args.count == 1:
                        return typeVariant("Array", val: typeOf(args[0]))
                    case let .const(name, args):
                        return typeVariant(
                            "Alias",
                            val: .Record([
                                ("name", .Literal(.str(name), ty: .str)),
                                ("args", .Tuple(args.map(typeOf), ty: .tuple(args)))
                            ], ty: .record(Row.from(entries: [("name", .str), ("args", .tuple(args))])))
                        )
                    case let .fun(args, ret):
                        let argsTy = Ty.array(ZogType)
                        return typeVariant(
                            "Function",
                            val: .Record([
                                ("args", .Array(args.map(typeOf), ty: argsTy)),
                                ("returns", typeOf(ret))
                            ], ty: .record(Row.from(entries: [("args", argsTy), ("returns", ZogType)])))
                        )
                    case let .record(row):
                        let entries = row.entries(sorted: true)
                        let entryType = Ty.record(Row.from(entries: [("field", .str), ("type", ZogType)]))
                        return typeVariant(
                            "Record",
                            val: .Array(entries.map({ (field, ty) in
                                    .Record([
                                        ("field", .Literal(.str(field), ty: .str)),
                                        ("type", typeOf(ty))
                                    ], ty: .array(entryType))
                            }), ty: ZogType)
                        )
                    }
                }
                
                return try typeOf(args[0].ty).codegen(ctx)
            default:
                fatalError("invalid built in function: impossible -> checked in infer")
            }
        }
    }
}

extension CoreStmt {
    func codegen(_ ctx: CoreContext) throws -> JSStmt {
        switch self {
        case let .Expr(expr):
            return .expr(try expr.codegen(ctx))
        case let .Let(_, pat: .any, _, val):
            return try .expr(val.codegen(ctx))
        case let .Let(mut, pat, ty: _, val):
            return .varDecl(mut: mut, try pat.codegen(ctx), try val.codegen(ctx))
        case let .If(cond, then, else_):
            let thenCtx = ctx.child(linear: false)
            try then.forEach({ stmt in thenCtx.statements.append(try stmt.codegen(thenCtx)) })
            
            var elseStmts: [JSStmt]? = nil
            
            if let else_ {
                let elseCtx = ctx.child(linear: false)
                try else_.forEach({ stmt in elseCtx.statements.append(try stmt.codegen(elseCtx)) })
                elseStmts = elseCtx.statements
            }
            
            return .if_(
                cond: try cond.codegen(ctx),
                then: thenCtx.statements,
                else_: elseStmts
            )
        case let .While(cond, body):
            let bodyCtx = ctx.child(linear: false)
            try body.forEach({ stmt in bodyCtx.statements.append(try stmt.codegen(bodyCtx)) })
            return .whileLoop(cond: try cond.codegen(ctx), body: bodyCtx.statements)
        case let .For(pat, iterator, body):
            let bodyCtx = ctx.child(linear: false)
            let corePat = try pat.codegen(ctx)
            try body.forEach({ stmt in bodyCtx.statements.append(try stmt.codegen(bodyCtx)) })
            
            if case .const("Array", _) = iterator.ty.deref(), case .variable(let name) = pat {
                let iVar = ctx.declareUnique("i");
                let iterVal: JSExpr = try iterator.codegen(ctx)
                let iterVar: JSExpr
                
                if case .variable(let name) = iterVal {
                    iterVar = .variable(name)
                } else {
                    iterVar = .variable(ctx.declareUnique("elems"))
                    ctx.statements.append(.varDecl(mut: false, iterVar, iterVal))
                }
                
                let len = JSExpr.objectAccess(iterVar, field: "length")
                let lenVar = JSExpr.variable(ctx.declareUnique("len"))
                ctx.statements.append(.varDecl(mut: false, lenVar, len))
                bodyCtx.statements.insert(
                    .varDecl(mut: false, .variable(name), .arraySubscript(iterVar, .variable(iVar))),
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
        case let .Stmt(stmt):
            return [try stmt.codegen(ctx)]
        case .TypeAlias(_, _, _):
            return []
        case .Enum(_, _, _):
            return []
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

extension DecisionTree {
    func rewrite(subject: CoreExpr, returnTy: Ty, patterns: [CorePattern], ctx: CoreContext) -> CoreExpr {
        func aux(_ dt: DecisionTree, subject: CoreExpr) -> CoreExpr {
            switch dt {
            case .fail:
                return .Raw(js: "(() => { throw new Error('Pattern matching failed'); })()", ty: .unit)
            case let .leaf(rowIndex, action):
                let vars = patterns[rowIndex].vars()
                return .Block(
                    vars.map({ (name, path) in
                        .Let(
                            mut: false,
                            pat: .variable(ctx.declare(name)),
                            ty: nil,
                            val: subject.at(path: path, env: ctx.env)
                        )
                    }),
                    ret: action,
                    ty: returnTy
                )
            case let .switch_(path, cases, defaultCase):
                var proj = subject.at(path: path, env: ctx.env)
                var tests = [(CoreExpr, CoreExpr)]()
                var isEnum = false
                
                for (ctor, _, dt) in cases {
                    switch ctor {
                    case let .literal(lit):
                        tests.append((
                            .Literal(lit, ty: lit.ty),
                            aux(dt, subject: subject)
                        ))
                    case let .variant(_, _, id):
                        isEnum = true
                        tests.append((
                            .Literal(.num(Float64(id)), ty: .str),
                            aux(dt, subject: subject)
                        ))
                    default:
                        continue
                    }
                }
                
                if tests.isEmpty {
                    if !cases.isEmpty {
                        return aux(cases[0].dt, subject: subject)
                    } else if defaultCase != nil {
                        return aux(defaultCase!, subject: subject)
                    }
                }
                
                if isEnum {
                    proj = .RecordSelect(proj, field: "variant", ty: proj.ty)
                }
                
                return .Switch(
                    proj,
                    cases: tests,
                    defaultCase: defaultCase != nil ? aux(defaultCase!, subject: subject) : nil,
                    ty: returnTy
                )
            }
        }
        
        return aux(self, subject: subject)
    }
}

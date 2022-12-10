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
    
    func declareMeta(_ name: String) -> String {
        var uniqueName = "$" + name
        var index = 0
        
        while variables.contains(uniqueName) {
            index += 1
            uniqueName = "\(name)\(index)"
        }
        
        return declare(uniqueName)
    }
    
    func declare(_ name: String) -> String {
        if reservedIdentifiers.contains(name) {
            return declareMeta(name)
        }

        variables.insert(name)
        
        let count = countVars(name: name)
        return formatVarName(name, count: count)
    }
    
    func lookup(_ name: String) throws -> String {
        if reservedIdentifiers.contains(name) {
            return try lookup("$\(name)")
        }

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
            
            if case .undefined = ret {
            } else if case .unit = body.ty.deref() {
                funCtx.statements.append(.expr(ret))
            } else {
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
        case let .If(cond, then, else_, ty):
            if case let .Expr(retThen) = then.last, case let .Expr(retElse) = else_.last {
                let resultName = ctx.declareMeta("result")
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
                let result = JSExpr.variable(ctx.declareMeta("result"))
                ctx.statements.append(.varDecl(export: false, const: false, result, .undefined))
                
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
                return tag
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
                    case .const("unit", []):
                        return typeVariant("Unit")
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
            case "jsTypeOf":
                return .typeof(try args[0].codegen(ctx))
            default:
                fatalError("invalid built in function: impossible -> checked in infer")
            }
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

extension DecisionTree {
    func rewrite(subject: CoreExpr, returnTy: Ty, patterns: [CorePattern], ctx: CoreContext) throws -> CoreExpr {
        func aux(_ dt: DecisionTree, subject: CoreExpr) throws -> CoreExpr {
            switch dt {
            case .fail:
                return .Raw(js: "(() => { throw new Error('Pattern matching failed'); })()", ty: .unit)
            case let .leaf(rowIndex, action):
                let vars = patterns[rowIndex].vars()
                return .Block(
                    vars.map({ (name, path) in
                        .Let(
                            mut: false,
                            pat: .variable(name),
                            ty: nil,
                            val: subject.at(path: path, env: ctx.env)
                        )
                    }),
                    ret: action,
                    ty: returnTy
                )
            case let .switch_(path, cases, defaultCase):
                let proj = subject.at(path: path, env: ctx.env)
                var literalTests = [(CoreExpr, CoreExpr)]()
                var variantTests = [(CoreExpr, CoreExpr)]()
                
                for (ctor, _, dt) in cases {
                    switch ctor {
                    case let .literal(lit):
                        literalTests.append((
                            .Literal(lit, ty: lit.ty),
                            try aux(dt, subject: subject)
                        ))
                    case let .variant(_, _, id, hasAssociatedValue):
                        let tag = CoreExpr.Literal(.num(Float64(id)), ty: .str)
                        let test = (tag, try aux(dt, subject: subject))
                        
                        if hasAssociatedValue {
                            variantTests.append(test)
                        } else {
                            literalTests.append(test)
                        }
                    default:
                        continue
                    }
                }
                
                if literalTests.isEmpty, variantTests.isEmpty {
                    if !cases.isEmpty {
                        return try aux(cases[0].dt, subject: subject)
                    } else if defaultCase != nil {
                        return try aux(defaultCase!, subject: subject)
                    }
                }
                
                let variantProj = CoreExpr.RecordSelect(proj, field: "variant", ty: proj.ty)
                let defaultCase_ = defaultCase != nil ? try aux(defaultCase!, subject: subject) : nil
                
                if !literalTests.isEmpty && !variantTests.isEmpty {
                    return .If(
                            cond: .BinaryOp(
                                .BuiltInCall("jsTypeOf", [proj], ty: .str),
                                .equ,
                                .Literal(.str("number"),
                                ty: .str
                            ),
                            ty: .bool
                        ),
                        then: [.Expr(.Switch(proj, cases: literalTests, defaultCase: nil, ty: returnTy))],
                        else_: [.Expr(.Switch(variantProj, cases: variantTests, defaultCase: defaultCase_, ty: returnTy))],
                        ty: returnTy
                    )
                }
                
                return .Switch(
                    literalTests.isEmpty ? variantProj : proj,
                    cases: literalTests.isEmpty ? variantTests : literalTests,
                    defaultCase: defaultCase_,
                    ty: returnTy
                )
            }
        }
        
        return try aux(self, subject: subject)
    }
}

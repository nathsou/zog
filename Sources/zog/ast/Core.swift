//
//  Core.swift
//  
//
//  Created by nathan on 20/11/2022.
//

import Foundation

// Type class placeholders
// see https://www.researchgate.net/publication/2683816_Implementing_Type_Classes
enum Placeholder: CustomStringConvertible {
    case Trait(trait: String, ty: Ty)
    case Method(trait: String, method: String, subjectTy: Ty)

    var description: String {
        switch self {
        case let .Trait(trait, ty): return "<\(trait), \(ty)>"
        case let .Method(trait, method, subjectTy): return "<\(trait).\(method), \(subjectTy)>"
        }
    }
}

indirect enum CoreExpr {
    case Literal(Literal, ty: Ty)
    case UnaryOp(UnaryOperator, CoreExpr, ty: Ty)
    case BinaryOp(CoreExpr, BinaryOperator, CoreExpr, ty: Ty)
    case Parens(CoreExpr, ty: Ty)
    case Var(String, placeholders: Ref<[Placeholder]> = Ref([]), ty: Ty)
    case Fun(modifier: FunModifier, args: [(CorePattern, Ty?)], retTy: Ty?, body: CoreExpr, ty: Ty)
    case Call(f: CoreExpr, args: [CoreExpr], ty: Ty)
    case Block([CoreStmt], ret: CoreExpr?, ty: Ty)
    case If(cond: CoreExpr, then: [CoreStmt], else_: [CoreStmt], ty: Ty)
    case Assignment(CoreExpr, AssignmentOperator, CoreExpr, ty: Ty)
    case Tuple([CoreExpr], ty: Ty)
    case Array([CoreExpr], ty: Ty)
    case ArraySubscript(CoreExpr, index: CoreExpr, ty: Ty)
    case Record([(String, CoreExpr)], ty: Ty)
    case RecordSelect(CoreExpr, field: String, ty: Ty)
    case Raw(js: String, ty: Ty)
    case Match(CoreExpr, cases: [(pattern: CorePattern, action: CoreExpr)], ty: Ty)
    case Switch(CoreExpr, cases: [(CoreExpr, CoreExpr)], defaultCase: CoreExpr?, ty: Ty)
    case Variant(enumName: Ref<String?>, variantName: String, val: CoreExpr?, ty: Ty)
    case MethodCall(subject: CoreExpr, method: String, args: [CoreExpr], placeholder: Ref<Placeholder?> = Ref(nil), ty: Ty)
    case BuiltInCall(String, [CoreExpr], ty: Ty)
    
    public var ty: Ty {
        switch self {
        case .Literal(_, let ty): return ty
        case .UnaryOp(_, _, let ty): return ty
        case .BinaryOp(_, _, _, let ty): return ty
        case .Parens(_, let ty): return ty
        case .Var(_, _, let ty): return ty
        case .Fun(_, _, _, _, let ty): return ty
        case .Call(_, _, let ty): return ty
        case .Block(_, _, let ty): return ty
        case .If(_, _, _, let ty): return ty
        case .Assignment(_, _, _, let ty): return ty
        case .Tuple(_, let ty): return ty
        case .Array(_, let ty): return ty
        case .ArraySubscript(_, _, let ty): return ty
        case .Record(_, let ty): return ty
        case .RecordSelect(_, _, let ty): return ty
        case .Raw(_, let ty): return ty
        case .Match(_, _, let ty): return ty
        case .Switch(_, _, _, let ty): return ty
        case .Variant(_, _, _, let ty): return ty
        case .MethodCall(_, _, _, _, let ty): return ty
        case .BuiltInCall(_, _, let ty): return ty
        }
    }
}

extension Expr {
    public func core(_ ctx: TypeContext, _ lvl: UInt) -> CoreExpr {
        let ty = { Ty.fresh(level: lvl) }
        
        switch self {
        case let .Literal(lit):
            return .Literal(lit, ty: ty())
        case let .UnaryOp(op, expr):
            return .UnaryOp(op, expr.core(ctx, lvl), ty: ty());
        case let .BinaryOp(lhs, op, rhs):
            return .BinaryOp(lhs.core(ctx, lvl), op, rhs.core(ctx, lvl), ty: ty())
        case let .Parens(expr):
            let coreExpr = expr.core(ctx, lvl)
            return .Parens(coreExpr, ty: coreExpr.ty)
        case let .Var(name):
            return .Var(name, ty: ty())
        case let .Fun(modifier, args, retTy, body):
            return .Fun(
                modifier: modifier,
                args: args.map({ (pat, ty) in (pat.core(), ty) }),
                retTy: retTy,
                body: body.core(ctx, lvl),
                ty: ty()
            )
        case let .Call(f: .Var(ruleName), args) where ctx.env.containsRule(ruleName):
            let (argNames, rhs, _) = ctx.env.lookupRule(ruleName)!
            let subst = Dictionary(uniqueKeysWithValues: zip(argNames, args))
            return rhs.substitute(mapping: subst).core(ctx, lvl)
        case let .Call(f, args):
            return .Call(f: f.core(ctx, lvl), args: args.map({ $0.core(ctx, lvl) }), ty: ty())
        case let .Block(stmts, ret):
            return .Block(stmts.map({ $0.core(ctx, lvl) }), ret: ret.map({ $0.core(ctx, lvl) }), ty: ty())
        case let .If(cond, then, else_):
            return .If(
                cond: cond.core(ctx, lvl),
                then: then.map({ $0.core(ctx, lvl) }),
                else_: else_.map({ $0.core(ctx, lvl) }),
                ty: ty()
            )
        case let .Assignment(lhs, op, rhs):
            return .Assignment(lhs.core(ctx, lvl), op, rhs.core(ctx, lvl), ty: ty())
        case let .Tuple(elems):
            return .Tuple(elems.map({ $0.core(ctx, lvl) }), ty: ty())
        case let .UseIn(pat, ty, val, rhs):
            return Expr.Block([.Let(mut: false, pat: pat, ty: ty, val: val)], ret: rhs).core(ctx, lvl)
        case let .Array(elems):
            return .Array(elems.map({ $0.core(ctx, lvl) }), ty: ty())
        case let .Record(fields):
            return .Record(fields.map({ (field, val) in (field, val.core(ctx, lvl)) }), ty: ty())
        case let .RecordSelect(record, field):
            return .RecordSelect(record.core(ctx, lvl), field: field, ty: ty())
        case let .Pipeline(arg1, f, remArgs):
            var args = [arg1]
            args.append(contentsOf: remArgs)
            
            return Expr.Call(f: .Var(f), args: args).core(ctx, lvl)
        case let .Raw(js):
            return .Raw(js: js, ty: ty())
        case let .Match(expr, cases):
            return .Match(
                expr.core(ctx, lvl),
                cases: cases.map({ (pat, body) in (pat.core(), body.core(ctx, lvl)) }),
                ty: ty()
            )
        case let .Variant(enumName, variantName, args):
            let val: Expr?
            
            switch args.count {
            case 0:
                val = nil
            case 1:
                val = args[0]
            default:
                val = .Tuple(args)
            }
            
            return .Variant(
                enumName: Ref(enumName),
                variantName: variantName,
                val: val.map({ $0.core(ctx, lvl) }),
                ty: ty()
            )
        case let .MethodCall(subject, method, args):
            return .MethodCall(
                subject: subject.core(ctx, lvl),
                method: method,
                args: args.map({ $0.core(ctx, lvl) }),
                ty: ty()
            )
        case let .BuiltInCall("show", args) where args.count == 1:
            return .Literal(.str("\(args[0])"), ty: .str)
        case let .BuiltInCall(name, args):
            return .BuiltInCall(name, args.map({ arg in arg.core(ctx, lvl) }), ty: ty())
        }
    }
}

enum CoreStmt {
    case Expr(CoreExpr)
    case Let(mut: Bool, pat: CorePattern, ty: Ty?, val: CoreExpr)
    indirect case If(cond: CoreExpr, then: [CoreStmt], else_: [CoreStmt]?)
    indirect case While(cond: CoreExpr, body: [CoreStmt])
    indirect case For(pat: CorePattern, iterator: CoreExpr, body: [CoreStmt])
    case Return(CoreExpr?)
    case Yield(CoreExpr)
    case Break
}

extension Stmt {
    func core(_ ctx: TypeContext, _ lvl: UInt) -> CoreStmt {
        switch self {
        case let .Expr(expr):
            return .Expr(expr.core(ctx, lvl))
        case let .Let(mut, pat, ty, val):
            return .Let(mut: mut, pat: pat.core(), ty: ty, val: val.core(ctx, lvl + 1))
        case let .If(cond, then, else_):
            return .If(
                cond: cond.core(ctx, lvl),
                then: then.map({ $0.core(ctx, lvl) }),
                else_: else_.map({ $0.map({ $0.core(ctx, lvl) }) })
            )
        case let .While(cond, body):
            return .While(cond: cond.core(ctx, lvl), body: body.map({ $0.core(ctx, lvl) }))
        case let .For(pat, iterator, body):
            return .For(
                pat: pat.core(),
                iterator: iterator.core(ctx, lvl),
                body: body.map({ $0.core(ctx, lvl) })
            )
        case let .Return(expr):
            return .Return(expr.map({ $0.core(ctx, lvl) }))
        case let .Yield(expr):
            return .Yield(expr.core(ctx, lvl))
        case .Break:
            return .Break
        case .Error(_, _):
            assertionFailure("Unexpected Error statement in Stmt.core")
            return .Break
        }
    }
}

enum CoreDecl {
    case Let(pub: Bool, mut: Bool, pat: CorePattern, ty: Ty?, val: CoreExpr)
    case Stmt(CoreStmt)
    case TypeAlias(pub: Bool, name: String, args: [TyVarId], ty: Ty)
    case Enum(pub: Bool, name: String, args: [TyVarId], variants: [(name: String, ty: Ty?)])
    case Declare(pub: Bool, name: String, ty: Ty)
    case Import(path: String, members: [String]?)
    case Error(ParserError, span: (Int, Int))
}

extension Decl {
    func core(_ ctx: TypeContext, _ lvl: UInt) throws -> CoreDecl? {
        switch self {
        case let .Let(pub, mut, pat, ty, val):
            return .Let(pub: pub, mut: mut, pat: pat.core(), ty: ty, val: val.core(ctx, lvl + 1))
        case let .Stmt(stmt):
            return .Stmt(stmt.core(ctx, lvl))
        case let .TypeAlias(pub, name, args, ty):
            return .TypeAlias(pub: pub, name: name, args: args, ty: ty)
        case let .Fun(pub, modifier, name, args, retTy, body):
            let funTy = Ty.fun(args.map({ $0.1 ?? .fresh(level: lvl + 1) }), retTy ?? .fresh(level: lvl + 1))
            return .Let(
                pub: pub,
                mut: false,
                pat: .variable(name), ty: funTy,
                val: .Fun(
                    modifier: modifier,
                    args: args.map({ (pat, ty) in (pat.core(), ty) }),
                    retTy: retTy,
                    body: body.core(ctx, lvl + 1),
                    ty: funTy
                )
            )
        case let .Enum(pub, name, args, variants):
            return .Enum(pub: pub, name: name, args: args, variants: variants)
        case let .Rewrite(pub, name, args, rhs):
            ctx.env.declareRule(name: name, args: args, rhs: rhs, pub: pub)
            return nil
        case let .Declare(pub, name, ty):
            return .Declare(pub: pub, name: name, ty: ty)
        case let .Import(path, members):
            if let mod = try ctx.resolver.resolve(path: path, level: lvl) {
                for (ruleName, info) in mod.rewritingRules {
                    ctx.env.declareRule(name: ruleName, args: info.args, rhs: info.rhs, pub: false)
                }
                
                if let members {
                    var newMembers = Array(members)
                    
                    for member in members {
                        if mod.traits.keys.contains(member) {
                            if let impls = mod.traitImpls[member] {
                                for impl in impls {
                                    newMembers.append(impl.dict)
                                }
                            }
                        }
                    }
                    
                    return .Import(path: path, members: newMembers)
                }
            }

            return .Import(path: path, members: members)
        case let .Trait(_, name, members):
            let subjectTyVarId = TyContext.nextTyVarId
            let subjectTy = Ty.variable(Ref(TyVar.unbound(id: subjectTyVarId, level: lvl + 1)))
            let replaceSelf = { (ty: Ty) in ty.rewrite({ t in
                if case .const("Self", []) = t {
                    return subjectTy
                } else {
                    return t
                }
            })}
            
            return ctx.env.declareTrait(
                name: name,
                subjectTy: subjectTy,
                methods: Dictionary(uniqueKeysWithValues: members.map({ method in
                    let ty = replaceSelf(.fun(method.args.map({ $0.1 }), method.ret))
                    return (method.name, (ty, false))
                })),
                pub: false,
                level: lvl
            )
        case let .TraitImpl(trait, _, methods):
            let freshTy = Ty.fresh(level: lvl + 1)
            let implTy = Ty.const(trait, [freshTy])
            let dict = "$impl\(trait)_\(freshTy.tyVarId()!)"
            ctx.env.declareTraitImpl(trait: trait, dict: dict, implementee: freshTy, context: [:])
            
            return .Let(
                pub: true,
                mut: false,
                pat: .variable(dict),
                ty: implTy,
                val: .Record(
                    methods.map({ method in (
                        method.name,
                        .Fun(
                            modifier: method.modifier,
                            args: method.args.map({ (pat, ty) in (pat.core(), ty) }),
                            retTy: method.ret,
                            body: method.body.core(ctx, lvl + 1),
                            ty: .fresh(level: lvl + 1)
                        )
                    ) }),
                    ty: implTy
                )
            )

        case let .Error(err, span):
            return .Error(err, span: span)
        }
    }
}

enum CorePattern: CustomStringConvertible {
    case any
    case variable(String)
    case literal(Literal)
    indirect case tuple([CorePattern])
    indirect case record([(field: String, pattern: CorePattern?)])
    indirect case variant(enumName: Ref<String?>, variant: String, CorePattern?)
    
    func ty(level: UInt, env: TypeEnv, expectedTy: Ty? = nil) throws -> (ty: Ty, vars: [String:Ty]) {
        var vars = [String:Ty]()
        
        func go(_ pat: CorePattern, _ ann: Ty?) throws -> Ty {
            switch pat {
            case .any:
                return .fresh(level: level)
            case let .variable(name):
                if let ty = vars[name] {
                    return ty
                }
                
                let ty = ann ?? Ty.fresh(level: level)
                vars[name] = ty
                return ty
            case let .literal(lit):
                return lit.ty
            case let .tuple(patterns):
                var elemTys: [Ty?]
                
                if case let .const("Tuple", tys) = ann?.deref() {
                    elemTys = tys
                } else {
                    elemTys = patterns.map({ _ in Ty.fresh(level: level) })
                }
                
                return .tuple(try zip(patterns, elemTys).map({ (pat, ty) in try go(pat, ty)}))
            case let .record(entries):
                var rowEntries = [(String, Ty)]()
                let fieldTys: [String:Ty]
                if case let .record(row) = ann?.deref() {
                    fieldTys = row.asDictionary()
                } else {
                    fieldTys = [:]
                }
                
                for (field, pat) in entries {
                    let ty: Ty
                    if let pat {
                        ty = try go(pat, fieldTys[field])
                    } else {
                        ty = fieldTys[field] ?? Ty.fresh(level: level)
                        vars[field] = ty
                    }
                    
                    rowEntries.append((field, ty))
                }
                
                return .record(Row.from(entries: rowEntries, tail: .fresh(level: level)))
            case let .variant(enumName, variantName, pat):
                if enumName.ref == nil, case let .const(actualEnumName, _) = ann?.deref() {
                    enumName.ref = actualEnumName
                }
                
                let enum_: EnumVariants
                if let enumName = enumName.ref {
                    enum_ = env.enums[enumName]!.variants
                } else {
                    enum_ = try env.lookupEnumUnique(variants: [variantName])
                }
                
                let (subst, enumTy) = enum_.instantiate(level: level)
                let associatedTy = enum_.mapping[variantName]!.ty?.substitute(subst)
                
                if pat == nil, associatedTy != nil {
                    throw TypeError.missingVariantArgument(enumName: enum_.name, variant: variantName)
                } else if pat != nil, associatedTy == nil {
                    throw TypeError.extraneousVariantArgument(enumName: enum_.name, variant: variantName)
                }
                
                _ = try pat.map({ try go($0, associatedTy) })
                
                return enumTy
            }
        }
        
        return (try go(self, expectedTy), vars)
    }
    
    func isInfallible() -> Bool {
        switch self {
        case .any, .variable(_): return true
        case .literal(_): return false
        case .tuple(let args): return args.allSatisfy({ $0.isInfallible() })
        case .record(let entries):
            return entries.allSatisfy({ $0.pattern?.isInfallible() ?? true })
        case let .variant(_, _, pat):
            return pat.map({ $0.isInfallible() }) ?? true
        }
    }
    
    public var description: String {
        switch self {
        case .any:
            return "_"
        case let .variable(name):
            return name
        case let .literal(lit):
            return "\(lit)"
        case let .tuple(patterns):
            return "(\(patterns.map({ p in "\(p)" }).joined(separator: ", ")))"
        case let .record(entries):
            return "{ \(entries.map({ (k, p) in p == nil ? k : "\(k): \(p!)" }).joined(separator: ", ")) }"
        case let .variant(_, name, pat):
            return "\(name) \(pat.map({ "\($0)" }) ?? "")"
        }
    }
}

extension Pattern {
    public func core() -> CorePattern {
        switch self {
        case .any:
            return .any
        case let .variable(name):
            return .variable(name)
        case let .literal(lit):
            return .literal(lit)
        case let .tuple(args):
            return .tuple(args.map({ p in p.core() }))
        case let .record(entries):
            return .record(entries.map({ (k, p) in (k, p?.core()) }))
        case let .variant(enumName, variant, args):
            let pat: Pattern?
            
            switch args.count {
            case 0:
                pat = nil
            case 1:
                pat = args[0]
            default:
                pat = .tuple(args)
            }
            
            return .variant(enumName: enumName, variant: variant, pat.map({ $0.core() }))
        }
    }
}

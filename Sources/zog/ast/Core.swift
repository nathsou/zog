//
//  Core.swift
//  
//
//  Created by nathan on 20/11/2022.
//

import Foundation

indirect enum CoreExpr {
    case Literal(Literal, ty: Ty)
    case UnaryOp(UnaryOperator, CoreExpr, ty: Ty)
    case BinaryOp(CoreExpr, BinaryOperator, CoreExpr, ty: Ty)
    case Parens(CoreExpr, ty: Ty)
    case Var(String, ty: Ty)
    case Fun(args: [(CorePattern, Ty?)], retTy: Ty?, body: CoreExpr, isIterator: Bool, ty: Ty)
    case Call(f: CoreExpr, args: [CoreExpr], ty: Ty)
    case Block([CoreStmt], ret: CoreExpr?, ty: Ty)
    case If(cond: CoreExpr, thenExpr: CoreExpr, elseExpr: CoreExpr, ty: Ty)
    case Assignment(CoreExpr, AssignmentOperator, CoreExpr, ty: Ty)
    case Tuple([CoreExpr], ty: Ty)
    case Array([CoreExpr], ty: Ty)
    case ArraySubscript(CoreExpr, index: CoreExpr, ty: Ty)
    case Record([(String, CoreExpr)], ty: Ty)
    case RecordSelect(CoreExpr, field: String, ty: Ty)
    case Raw(js: String, ty: Ty)
    case Match(CoreExpr, cases: [(pattern: CorePattern, action: CoreExpr)], ty: Ty)
    case Switch(CoreExpr, cases: [(CoreExpr, CoreExpr)], defaultCase: CoreExpr?, ty: Ty)
    case Variant(typeName: String?, variantName: String, val: CoreExpr?, ty: Ty)
    
    public var ty: Ty {
        switch self {
        case .Literal(_, let ty): return ty
        case .UnaryOp(_, _, let ty): return ty
        case .BinaryOp(_, _, _, let ty): return ty
        case .Parens(_, let ty): return ty
        case .Var(_, let ty): return ty
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
        }
    }
}

extension Expr {
    public func core(_ lvl: UInt) -> CoreExpr {
        let ty = { Ty.fresh(level: lvl) }
        
        switch self {
        case let .Literal(lit):
            return .Literal(lit, ty: ty())
        case let .UnaryOp(op, expr):
            return .UnaryOp(op, expr.core(lvl), ty: ty());
        case let .BinaryOp(lhs, op, rhs):
            return .BinaryOp(lhs.core(lvl), op, rhs.core(lvl), ty: ty())
        case let .Parens(expr):
            let coreExpr = expr.core(lvl)
            return .Parens(coreExpr, ty: coreExpr.ty)
        case let .Var(name):
            return .Var(name, ty: ty())
        case let .Fun(args, retTy, body, isIterator):
            return .Fun(
                args: args.map({ (pat, ty) in (pat.core(), ty) }),
                retTy: retTy,
                body: body.core(lvl),
                isIterator: isIterator,
                ty: ty()
            )
        case let .Call(f, args):
            return .Call(f: f.core(lvl), args: args.map({ $0.core(lvl) }), ty: ty())
        case let .Block(stmts, ret):
            return .Block(stmts.map({ $0.core(lvl) }), ret: ret.map({ $0.core(lvl) }), ty: ty())
        case let .If(cond, thenExpr, elseExpr):
            return .If(cond: cond.core(lvl), thenExpr: thenExpr.core(lvl), elseExpr: elseExpr.core(lvl), ty: ty())
        case let .Assignment(lhs, op, rhs):
            return .Assignment(lhs.core(lvl), op, rhs.core(lvl), ty: ty())
        case let .Tuple(elems):
            return .Tuple(elems.map({ $0.core(lvl) }), ty: ty())
        case let .UseIn(pat, ty, val, rhs):
            return Expr.Block([.Let(mut: false, pat: pat, ty: ty, val: val)], ret: rhs).core(lvl)
        case let .Array(elems):
            return .Array(elems.map({ $0.core(lvl) }), ty: ty())
        case let .Record(fields):
            return .Record(fields.map({ (field, val) in (field, val.core(lvl)) }), ty: ty())
        case let .RecordSelect(record, field):
            return .RecordSelect(record.core(lvl), field: field, ty: ty())
        case let .Pipeline(arg1, f, remArgs):
            var args = [arg1]
            args.append(contentsOf: remArgs)
            
            return Expr.Call(f: .Var(f), args: args).core(lvl)
        case let .Raw(js):
            return .Raw(js: js, ty: ty())
        case let .Match(expr, cases):
            return .Match(
                expr.core(lvl),
                cases: cases.map({ (pat, body) in (pat.core(), body.core(lvl)) }),
                ty: ty()
            )
        case let .Variant(typeName, variantName, val):
            return .Variant(
                typeName: typeName,
                variantName: variantName,
                val: val.map({ $0.core(lvl) }),
                ty: ty()
            )
        }
    }
}

enum CoreStmt {
    case Expr(CoreExpr)
    case Let(mut: Bool, pat: CorePattern, ty: Ty?, val: CoreExpr)
    indirect case While(cond: CoreExpr, body: [CoreStmt])
    indirect case For(pat: CorePattern, iterator: CoreExpr, body: [CoreStmt])
    indirect case If(cond: CoreExpr, then: [CoreStmt], else_: [CoreStmt]?)
    case Return(CoreExpr?)
    case Yield(CoreExpr)
    case Break
}

extension Stmt {
    func core(_ lvl: UInt) -> CoreStmt {
        switch self {
        case let .Expr(expr):
            return .Expr(expr.core(lvl))
        case let .Let(mut, pat, ty, val):
            return .Let(mut: mut, pat: pat.core(), ty: ty, val: val.core(lvl + 1))
        case let .While(cond, body):
            return .While(cond: cond.core(lvl), body: body.map({ $0.core(lvl) }))
        case let .For(pat, iterator, body):
            return .For(
                pat: pat.core(),
                iterator: iterator.core(lvl),
                body: body.map({ $0.core(lvl) })
            )
        case let .If(cond, then, else_):
            return .If(
                cond: cond.core(lvl),
                then: then.map({ $0.core(lvl) }),
                else_: else_?.map({ $0.core(lvl) })
            )
        case let .Return(expr):
            return .Return(expr.map({ $0.core(lvl) }))
        case let .Yield(expr):
            return .Yield(expr.core(lvl))
        case .Break:
            return .Break
        case .Error(_, _):
            assertionFailure("Unexpected Error statement in CoreStmt.from")
            return .Break
        }
    }
}

enum CoreDecl {
    case Stmt(CoreStmt)
    case TypeAlias(name: String, args: [TyVarId], ty: Ty)
    case Enum(name: String, args: [TyVarId], variants: [(name: String, ty: Ty?)])
}

extension Decl {
    func core(_ lvl: UInt) -> CoreDecl {
        switch self {
        case let .Stmt(stmt):
            return .Stmt(stmt.core(lvl))
        case let .TypeAlias(name, args, ty):
            return .TypeAlias(name: name, args: args, ty: ty)
        case let .Enum(name, args, variants):
            return .Enum(name: name, args: args, variants: variants)
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
                
                let ty = Ty.fresh(level: level)
                vars[name] = ty
                return ty
            case let .literal(lit):
                return lit.ty
            case let .tuple(patterns):
                var elemTys: [Ty?]
                
                if case let .const("Tuple", tys) = ann?.deref() {
                    elemTys = tys
                } else {
                    elemTys = patterns.map({ _ in nil })
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
                
                let enum_: Enum
                if let enumName = enumName.ref {
                    enum_ = env.enums[enumName]!.variants
                } else {
                    enum_ = try env.lookupEnumUnique(variants: [variantName])
                }
                
                _ = try pat.map({ try go($0, enum_.mapping[variantName]!) })
                
                return .const(enum_.name, [])
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
        case let .variant(enumName, variant, pat):
            return .variant(enumName: enumName, variant: variant, pat.map({ $0.core() }))
        }
    }
}

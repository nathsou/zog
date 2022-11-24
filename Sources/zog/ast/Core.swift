//
//  Core.swift
//  
//
//  Created by nathan on 20/11/2022.
//

import Foundation

public indirect enum CoreExpr {
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
    case Record([(String, CoreExpr)], ty: Ty)
    case RecordSelect(CoreExpr, field: String, ty: Ty)
    case Raw(js: String, ty: Ty)
    
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
        case .Record(_, let ty): return ty
        case .RecordSelect(_, _, let ty): return ty
        case .Raw(_, let ty): return ty
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
        }
    }
}

public enum CoreStmt {
    case Expr(CoreExpr)
    case Let(mut: Bool, pat: CorePattern, ty: Ty?, val: CoreExpr)
    indirect case While(cond: CoreExpr, body: [CoreStmt])
    indirect case For(pat: CorePattern, iterator: CoreExpr, body: [CoreStmt])
    indirect case IfThen(cond: CoreExpr, then: [CoreStmt])
    case Return(CoreExpr?)
    case Yield(CoreExpr)
    case Break
}

extension Stmt {
    public func core(_ lvl: UInt) -> CoreStmt {
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
        case let .IfThen(cond, then):
            return .IfThen(cond: cond.core(lvl), then: then.map({ $0.core(lvl) }))
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

public enum CorePattern {
    case any
    case variable(String)
    indirect case tuple([CorePattern])
    indirect case record([(String, CorePattern?)])
    
    public func ty(level: UInt) -> (ty: Ty, vars: [String:Ty]) {
        var vars = [String:Ty]()
        
        func go(_ pat: CorePattern) -> Ty {
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
            case let .tuple(patterns):
                return .tuple(patterns.map(go))
            case let .record(entries):
                var rowEntries = [(String, Ty)]()
                
                for (key, pat) in entries {
                    let ty: Ty
                    if let pat {
                        ty = go(pat)
                    } else {
                        ty = Ty.fresh(level: level)
                        vars[key] = ty
                    }
                    
                    rowEntries.append((key, ty))
                }
                
                return .record(Row.from(entries: rowEntries, tail: .fresh(level: level)))
            }
        }
        
        return (go(self), vars)
    }
}

extension Pattern {
    public func core() -> CorePattern {
        switch self {
        case .any:
            return .any
        case let .variable(name):
            return .variable(name)
        case let .tuple(args):
            return .tuple(args.map({ p in p.core() }))
        case let .record(entries):
            return .record(entries.map({ (k, p) in (k, p?.core()) }))
        }
    }
}

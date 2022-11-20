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
    case Fun(args: [String], body: CoreExpr, isIterator: Bool, ty: Ty)
    case Call(f: CoreExpr, args: [CoreExpr], ty: Ty)
    case Block([CoreStmt], ret: CoreExpr?, ty: Ty)
    case If(cond: CoreExpr, thenExpr: CoreExpr, elseExpr: CoreExpr?, ty: Ty)
    case Assignment(CoreExpr, AssignmentOperator, CoreExpr, ty: Ty)
    case Tuple([CoreExpr], ty: Ty)
    
    public func ty() -> Ty {
        switch self {
        case .Literal(_, let ty): return ty
        case .UnaryOp(_, _, let ty): return ty
        case .BinaryOp(_, _, _, let ty): return ty
        case .Parens(_, let ty): return ty
        case .Var(_, let ty): return ty
        case .Fun(_, _, _, let ty): return ty
        case .Call(_, _, let ty): return ty
        case .Block(_, _, let ty): return ty
        case .If(_, _, _, let ty): return ty
        case .Assignment(_, _, _, let ty): return ty
        case .Tuple(_, let ty): return ty
        }
    }

    public static func from(_ expr: Expr, _ lvl: UInt) -> CoreExpr {
        let ty = Ty.freshVar(level: lvl);
        let go = { (_ expr: Expr) in from(expr, lvl) }

        switch expr {
        case let .Literal(lit):
            return .Literal(lit, ty: ty)
        case let .UnaryOp(op, expr):
            return .UnaryOp(op, go(expr), ty: ty);
        case let .BinaryOp(lhs, op, rhs):
            return .BinaryOp(go(lhs), op, go(rhs), ty: ty)
        case let .Parens(expr):
            return .Parens(go(expr), ty: ty)
        case let .Var(name):
            return .Var(name, ty: ty)
        case let .Fun(args, body, isIterator):
            return .Fun(args: args, body: go(body), isIterator: isIterator, ty: ty)
        case let .Call(f, args):
            return .Call(f: go(f), args: args.map({ go($0) }), ty: ty)
        case let .Block(stmts, ret):
            return .Block(stmts.map({ CoreStmt.from($0, lvl) }), ret: ret.map(go), ty: ty)
        case let .If(cond, thenExpr, elseExpr):
            return .If(cond: go(cond), thenExpr: go(thenExpr), elseExpr: elseExpr.map(go), ty: ty)
        case let .Assignment(lhs, op, rhs):
            return .Assignment(go(lhs), op, go(rhs), ty: ty)
        case let .Tuple(elems):
            return .Tuple(elems.map(go), ty: ty)
        }
    }
}

enum CoreStmt {
    case Expr(CoreExpr)
    case Let(mut: Bool, name: String, val: CoreExpr)
    indirect case While(cond: CoreExpr, body: [CoreStmt])
    indirect case For(name: String, iterator: CoreExpr, body: [CoreStmt])
    case Return(CoreExpr?)
    case Yield(CoreExpr)
    case Break

    public static func from(_ stmt: Stmt, _ lvl: UInt) -> CoreStmt {
        let go = { (_ stmt: Stmt) in from(stmt, lvl) }
        let goExpr = { (_ expr: Expr) in CoreExpr.from(expr, lvl) }

        switch stmt {
        case let .Expr(expr):
            return .Expr(goExpr(expr))
        case let .Let(mut, name, val):
            return .Let(mut: mut, name: name, val: CoreExpr.from(val, lvl + 1))
        case let .While(cond, body):
            return .While(cond: goExpr(cond), body: body.map(go))
        case let .For(name, iterator, body):
            return .For(name: name, iterator: goExpr(iterator), body: body.map(go))
        case let .Return(expr):
            return .Return(expr.map(goExpr))
        case let .Yield(expr):
            return .Yield(goExpr(expr))
        case .Break:
            return .Break
        case .Error(_, _):
            assertionFailure("Unexpected Error statement in CoreStmt.from")
            return .Break
        }
    }
}

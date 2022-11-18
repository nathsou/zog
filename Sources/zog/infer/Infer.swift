//
//  Infer.swift
//  
//
//  Created by nathan on 18/11/2022.
//

import Foundation

extension Expr {
    public func infer(_ env: Env, _ level: UInt) throws -> Ty {
        switch self {
        case let .Literal(lit):
            switch lit {
            case .bool(_):
                return .bool
            case .num(_):
                return .num
            case .str(_):
                return .str
            case .unit:
                return .unit
            }
        case let .Var(name):
            if let ty = env.lookup(varName: name) {
                return ty.instantiate(level: level)
            } else {
                throw TypeError.unknownVariable(name)
            }
        case let .Parens(expr):
            return try expr.infer(env, level)
        case let .Fun(args, body, isIterator):
            let argsTy = args.map({ _ in Ty.freshVar(level: level) })
            let bodyEnv = env.child()
            
            for (arg, ty) in zip(args, argsTy) {
                bodyEnv.declare(varName: arg, ty: ty)
            }
            
            let retTy = isIterator ? Ty.iterator(Ty.freshVar(level: level)) : Ty.freshVar(level: level)
            Env.pushFunc(retTy: retTy)
            var actualRetTy = try body.infer(bodyEnv, level)
            if isIterator {
                actualRetTy = .iterator(.freshVar(level: level))
                try unify(retTy, actualRetTy)
            } else {
                try unify(retTy, actualRetTy)
            }
            Env.popFunc()
            
            return .fun(argsTy, actualRetTy)
        case let .Call(f, args):
            let argsTy = try args.map({ arg in try arg.infer(env, level) })
            let funTy = try f.infer(env, level)
            let retTy = Ty.freshVar(level: level)
            let expectedTy = Ty.fun(argsTy, retTy)
            try unify(expectedTy, funTy)
            
            return retTy
        case let .If(cond, thenExpr, elseExpr):
            let condTy = try cond.infer(env, level)
            try unify(condTy, .bool)
            
            let thenTy = try thenExpr.infer(env, level)
            
            if let elseExpr {
                let elseTy = try elseExpr.infer(env, level)
                try unify(thenTy, elseTy)
                return thenTy
            }
            
            return .unit
        case let .UnaryOp(op, expr):
            let exprTy = try expr.infer(env, level)
            let ty: Ty
            
            switch op {
            case .arithmeticNegation: ty = .num
            case .logicalNegation: ty = .bool
            }
            
            try unify(exprTy, ty)
            
            return ty
        case let .BinaryOp(lhs, op, rhs):
            let lhsTy = try lhs.infer(env, level)
            let rhsTy = try rhs.infer(env, level)
            let argTy: Ty
            let retTy: Ty
            
            switch op {
            case .add, .sub, .mul, .div, .mod, .pow:
                argTy = .num
                retTy = .num
            case .equ, .neq, .gtr, .geq, .lss, .leq:
                argTy = .num
                retTy = .bool
            case .and, .or:
                argTy = .bool
                retTy = .bool
            }
            
            try unify(lhsTy, argTy)
            try unify(rhsTy, argTy)
            
            return retTy
        case let .Assignment(lhs, op, rhs):
            let lhsTy = try lhs.infer(env, level)
            let rhsTy = try rhs.infer(env, level)
            
            switch op {
            case .plusEq, .minusEq, .timesEq, .divideEq:
                try unify(lhsTy, .num)
                try unify(rhsTy, .num)
                return .num
            case .eq:
                try unify(lhsTy, rhsTy)
                return lhsTy
            }
        case let .Block(stmts, ret):
            let blockEnv = env.child()
            
            for stmt in stmts {
                try stmt.infer(blockEnv, level)
            }
            
            return try ret?.infer(blockEnv, level) ?? .unit
        case let .Tuple(elems):
            let elemTys = try elems.map({ elem in try elem.infer(env, level) })
            return .tuple(elemTys)
        }
    }
}

extension Stmt {
    public func infer(_ env: Env, _ level: UInt) throws {
        switch self {
        case let .Expr(expr):
            _ = try expr.infer(env, level)
        case let .Let(_, name, val):
            let valTy = try val.infer(env, level + 1)
            let genValTy = valTy.generalize(level: level)
            env.declare(varName: name, ty: genValTy)
        case let .For(name, iterator, body):
            let iterTy = try iterator.infer(env, level)
            let iterItemTy = Ty.freshVar(level: level)
            try unify(iterTy, .iterator(iterItemTy))
            let bodyEnv = env.child()
            bodyEnv.declare(varName: name, ty: iterItemTy)
    
            for stmt in body {
                try stmt.infer(bodyEnv, level)
            }
        case let .While(cond, body):
            let condTy = try cond.infer(env, level)
            try unify(condTy, .bool)
            let bodyEnv = env.child()
            
            for stmt in body {
                try stmt.infer(bodyEnv, level)
            }
        case let .Return(expr):
            let exprTy = try expr?.infer(env, level) ?? .unit
            if let funcRetTy = Env.funcReturnTy() {
                try unify(exprTy, funcRetTy)
            } else {
                throw TypeError.cannotReturnOutsideFunctionBody
            }
        case .Break:
            break
        case let .Yield(expr):
            let exprTy = try expr.infer(env, level)
            if case let .const("iter", args) = Env.funcReturnTy(), args.count == 1 {
                try unify(exprTy, args[0])
            } else {
                throw TypeError.cannotYieldOutsideIteratorBody
            }
        case .Error(_, _):
            break
        }
    }
}

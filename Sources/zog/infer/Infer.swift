//
//  Infer.swift
//  
//
//  Created by nathan on 18/11/2022.
//

import Foundation

extension CoreExpr {
    public func infer(_ env: Env, _ level: UInt) throws -> Ty {
        let tau: Ty
        
        switch self {
        case let .Literal(lit, _):
            switch lit {
            case .bool(_):
                tau = .bool
            case .num(_):
                tau = .num
            case .str(_):
                tau = .str
            case .unit:
                tau = .unit
            }
        case let .Var(name, _):
            if let ty = env.lookup(varName: name) {
                tau = ty.instantiate(level: level)
            } else {
                throw TypeError.unknownVariable(name)
            }
        case let .Parens(expr, _):
            tau = try expr.infer(env, level)
        case let .Fun(args, body, isIterator, _):
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
            
            tau = .fun(argsTy, actualRetTy)
        case let .Call(f, args, retTy):
            let argsTy = try args.map({ arg in try arg.infer(env, level) })
            let funTy = try f.infer(env, level)
            let expectedTy = Ty.fun(argsTy, retTy)
            try unify(expectedTy, funTy)
            
            tau = retTy
        case let .If(cond, thenExpr, elseExpr, _):
            let condTy = try cond.infer(env, level)
            try unify(condTy, .bool)
            
            let thenTy = try thenExpr.infer(env, level)
            
            if let elseExpr {
                let elseTy = try elseExpr.infer(env, level)
                try unify(thenTy, elseTy)
                return thenTy
            }
            
            tau = .unit
        case let .UnaryOp(op, expr, _):
            let exprTy = try expr.infer(env, level)
            
            switch op {
            case .arithmeticNegation: tau = .num
            case .logicalNegation: tau = .bool
            }
            
            try unify(exprTy, tau)
        case let .BinaryOp(lhs, op, rhs, _):
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
            
            tau = retTy
        case let .Assignment(lhs, op, rhs, _):
            let lhsTy = try lhs.infer(env, level)
            let rhsTy = try rhs.infer(env, level)
            
            switch op {
            case .plusEq, .minusEq, .timesEq, .divideEq:
                try unify(lhsTy, .num)
                try unify(rhsTy, .num)
                tau = .num
            case .eq:
                try unify(lhsTy, rhsTy)
                tau = lhsTy
            }
        case let .Block(stmts, ret, _):
            let blockEnv = env.child()
            
            for stmt in stmts {
                try stmt.infer(blockEnv, level)
            }
            
            tau = try ret?.infer(blockEnv, level) ?? .unit
        case let .Tuple(elems, _):
            let elemTys = try elems.map({ elem in try elem.infer(env, level) })
            tau = .tuple(elemTys)
        }
        
        let ty = self.ty()
        try unify(tau, ty)
        
        return ty
    }
}

extension CoreStmt {
    public func infer(_ env: Env, _ level: UInt) throws {
        switch self {
        case let .Expr(expr):
            _ = try expr.infer(env, level)
        case let .Let(_, name, val):
            let valTy: Ty
            
            if case .Fun(_, _, _, let funTy) = val {
                // include `name` in the rhs environment
                // to support recursive closures
                let rhsEnv = env.child()
                rhsEnv.declare(varName: name, ty: funTy)
                valTy = try val.infer(rhsEnv, level + 1)
                try unify(valTy, funTy)
            } else {
                valTy = try val.infer(env, level + 1)
            }
            
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
        }
    }
}

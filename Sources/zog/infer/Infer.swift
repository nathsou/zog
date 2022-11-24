//
//  Infer.swift
//  
//
//  Created by nathan on 18/11/2022.
//

import Foundation

extension CoreExpr {
    public func infer(_ env: TypeEnv, _ level: UInt) throws -> Ty {
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
            if let ty = env.lookup(name) {
                tau = ty.instantiate(level: level)
            } else {
                throw TypeError.unknownVariable(name)
            }
        case let .Parens(expr, _):
            tau = try expr.infer(env, level)
        case let .Fun(args, retTyAnn, body, isIterator, _):
            let bodyEnv = env.child()
            var argsInfo = [(pat: CorePattern, ty: Ty, vars: [String:Ty], ann: Ty?)]()
            
            for (pat, ann) in args {
                let (ty, vars) = pat.ty(level: level)
                argsInfo.append((pat, ty, vars, ann))
            }
            
            let argTys = argsInfo.map({ $0.ty })
            
            for (_, ty: patTy, vars, ann) in argsInfo {
                for (variable, ty) in vars {
                    try bodyEnv.declare(variable, ty: ty)
                }
                
                if let ann {
                    try unify(patTy, ann)
                }
            }
            
            let innerRetTy = retTyAnn ?? .fresh(level: level)
            let retTy = isIterator ? Ty.iterator(innerRetTy, level: level) : innerRetTy
            TypeEnv.pushFunctionInfo(retTy: retTy, isIterator: isIterator)
            var actualRetTy = try body.infer(bodyEnv, level)
            if isIterator {
                actualRetTy = .iterator(.fresh(level: level), level: level)
                try unify(retTy, actualRetTy)
            } else {
                try unify(retTy, actualRetTy)
            }
            TypeEnv.popFunctionInfo()
            
            tau = .fun(argTys, actualRetTy)
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
            let elseTy = try elseExpr.infer(env, level)
            try unify(thenTy, elseTy)
            tau = thenTy
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
        case let .Block(stmts, ret, ty):
            let blockEnv = env.child()
            
            for stmt in stmts {
                try stmt.infer(blockEnv, level)
                
                if case let .Return(expr) = stmt, expr != nil {
                    try unify(expr!.ty, ty)
                }
            }
            
            if let ret {
                let retTy = try ret.infer(blockEnv, level)
                try unify(retTy, ty)
            }
            
            tau = ty
        case let .Tuple(elems, _):
            let elemTys = try elems.map({ elem in try elem.infer(env, level) })
            tau = .tuple(elemTys)
        case let .Array(elems, _):
            let elemTy = Ty.fresh(level: level)
            
            for elem in elems {
                let ty = try elem.infer(env, level)
                try unify(ty, elemTy)
            }
            
            tau = .array(elemTy)
        case let .Record(entries, _):
            let fields = try entries.map({ (field, val) in (field, try val.infer(env, level)) })
            tau = .record(Row.from(entries: fields))
        case let .RecordSelect(record, field, fieldTy):
            let tail = Ty.fresh(level: level)
            let partialRecordTy = Ty.record(Row.extend(field: field, ty: fieldTy, tail: tail))
            let recordTy = try record.infer(env, level)
            try unify(recordTy, partialRecordTy)
            tau = fieldTy
        case let .Raw(_, ty):
            tau = ty
        }
        
        let ty = self.ty
        try unify(tau, ty)
        
        return ty
    }
}

extension CoreStmt {
    public func infer(_ env: TypeEnv, _ level: UInt) throws {
        switch self {
        case let .Expr(expr):
            _ = try expr.infer(env, level)
        case let .Let(_, pat, ann, val):
            let (patternTy, patternVars) = pat.ty(level: level + 1)
            let rhsEnv = env.child()
            
            for (variable, ty) in patternVars {
                try rhsEnv.declare(variable, ty: ty)
            }
            
            let valTy = try val.infer(rhsEnv, level + 1)
            
            if let ann {
                try unify(ann, valTy)
            }
            
            try unify(patternTy, valTy)
            
            for (name, ty) in patternVars {
                try env.declare(name, ty: ty.generalize(level: level))
            }
        case let .For(pat, iterator, body):
            let iterTy = try iterator.infer(env, level)
            let iterItemTy = Ty.fresh(level: level)
            try unify(iterTy, .iterator(iterItemTy, level: level))
            let bodyEnv = env.child()
            let (patternTy, patternVars) = pat.ty(level: level)
            try unify(patternTy, iterItemTy)
            
            for (name, ty) in patternVars {
                try bodyEnv.declare(name, ty: ty)
            }
    
            for stmt in body {
                try stmt.infer(bodyEnv, level)
            }
        case let .While(cond, body), let .IfThen(cond, body):
            let condTy = try cond.infer(env, level)
            try unify(condTy, .bool)
            let bodyEnv = env.child()
            
            for stmt in body {
                try stmt.infer(bodyEnv, level)
            }
        case let .Return(expr):
            let exprTy = try expr?.infer(env, level) ?? .unit
            if let (funcRetTy, _) = TypeEnv.peekFunctionInfo() {
                try unify(exprTy, funcRetTy)
            } else {
                throw TypeError.cannotReturnOutsideFunctionBody
            }
        case .Break:
            break
        case let .Yield(expr):
            _ = try expr.infer(env, level)
            if case let funcInfo? = TypeEnv.peekFunctionInfo() {
                if !funcInfo.isIterator {
                    throw TypeError.cannotYieldOutsideIteratorBody
                }
            }
        }
    }
}

//
//  Infer.swift
//  
//
//  Created by nathan on 18/11/2022.
//

import Foundation

extension CorePattern {
    func checkIfInfallible() throws {
        if !self.isInfallible() {
            throw TypeError.patternDestructuringCanFail(self)
        }
    }
}

extension CoreExpr {
    func infer(_ env: TypeEnv, _ level: UInt) throws -> Ty {
        let tau: Ty
        
        switch self {
        case let .Literal(lit, _):
            tau = lit.ty
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
                try pat.checkIfInfallible()
                let (ty, vars) = try pat.ty(level: level, env: env)
                argsInfo.append((pat, ty, vars, ann))
            }
            
            let argTys = argsInfo.map({ $0.ty })
            
            for (_, ty: patTy, vars, ann) in argsInfo {
                for (variable, ty) in vars {
                    try bodyEnv.declare(variable, ty: ty)
                }
                
                if let ann {
                    try env.unify(patTy, ann)
                }
            }
            
            let innerRetTy = retTyAnn ?? .fresh(level: level)
            let retTy = isIterator ? Ty.iterator(innerRetTy) : innerRetTy
            TypeEnv.pushFunctionInfo(retTy: retTy, isIterator: isIterator)
            var actualRetTy = try body.infer(bodyEnv, level)
            if isIterator {
                actualRetTy = .iterator(.fresh(level: level))
                try env.unify(retTy, actualRetTy)
            } else {
                try env.unify(retTy, actualRetTy)
            }
            TypeEnv.popFunctionInfo()
            
            tau = .fun(argTys, actualRetTy)
        case let .Call(f, args, retTy):
            let argsTy = try args.map({ arg in try arg.infer(env, level) })
            let funTy = try f.infer(env, level)
            let expectedTy = Ty.fun(argsTy, retTy)
            try env.unify(expectedTy, funTy)
            
            tau = retTy
        case let .If(cond, thenExpr, elseExpr, _):
            let condTy = try cond.infer(env, level)
            try env.unify(condTy, .bool)
            let thenTy = try thenExpr.infer(env, level)
            let elseTy = try elseExpr.infer(env, level)
            try env.unify(thenTy, elseTy)
            tau = thenTy
        case let .UnaryOp(op, expr, _):
            let exprTy = try expr.infer(env, level)
            
            switch op {
            case .arithmeticNegation: tau = .num
            case .logicalNegation: tau = .bool
            }
            
            try env.unify(exprTy, tau)
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
                argTy = Ty.fresh(level: level)
                retTy = .bool
            case .and, .or:
                argTy = .bool
                retTy = .bool
            }
            
            try env.unify(lhsTy, argTy)
            try env.unify(rhsTy, argTy)
            
            tau = retTy
        case let .Assignment(lhs, op, rhs, _):
            let lhsTy = try lhs.infer(env, level)
            let rhsTy = try rhs.infer(env, level)
            
            switch op {
            case .plusEq, .minusEq, .timesEq, .divideEq:
                try env.unify(lhsTy, .num)
                try env.unify(rhsTy, .num)
                tau = .num
            case .eq:
                try env.unify(lhsTy, rhsTy)
                tau = rhsTy
            }
        case let .Block(stmts, ret, ty):
            let blockEnv = env.child()
            
            for stmt in stmts {
                try stmt.infer(blockEnv, level)
                
                if case let .Return(expr) = stmt, expr != nil {
                    try env.unify(expr!.ty, ty)
                }
            }
            
            if let ret {
                let retTy = try ret.infer(blockEnv, level)
                try env.unify(retTy, ty)
            }
            
            tau = ty
        case let .Tuple(elems, _):
            let elemTys = try elems.map({ elem in try elem.infer(env, level) })
            tau = .tuple(elemTys)
        case let .Array(elems, _):
            let elemTy = Ty.fresh(level: level)
            
            for elem in elems {
                let ty = try elem.infer(env, level)
                try env.unify(ty, elemTy)
            }
            
            tau = .array(elemTy)
        case let .ArraySubscript(elems, index, _):
            let itemTy = Ty.fresh(level: level)
            let arrayTy = try elems.infer(env, level)
            try env.unify(arrayTy, .array(itemTy))
            let indexTy = try index.infer(env, level)
            try env.unify(indexTy, .num)
            tau = itemTy
        case let .Record(entries, _):
            let fields = try entries.map({ (field, val) in (field, try val.infer(env, level)) })
            tau = .record(Row.from(entries: fields))
        case let .RecordSelect(record, field, fieldTy):
            let tail = Ty.fresh(level: level)
            let partialRecordTy = Ty.record(Row.extend(field: field, ty: fieldTy, tail: tail))
            let recordTy = try record.infer(env, level)
            try env.unify(recordTy, partialRecordTy)
            tau = fieldTy
        case let .Raw(_, ty):
            tau = ty
        case let .Match(expr, cases, ty):
            let exprTy = try expr.infer(env, level)
            
            for (pat, body) in cases {
                let (patTy, vars) = try pat.ty(level: level, env: env)
                let bodyEnv = env.child()
                for (name, ty) in vars {
                    try bodyEnv.declare(name, ty: ty)
                }
                
                try env.unify(patTy, exprTy)
                let bodyTy = try body.infer(bodyEnv, level)
                try env.unify(bodyTy, ty)
            }
            
            if cases.isEmpty {
                try env.unify(ty, .unit)
            }
            
            tau = ty
        case let .Switch(expr, cases, defaultCase, ty):
            let exprTy = try expr.infer(env, level)
            
            for (val, body) in cases {
                let valTy = try val.infer(env, level)
                try env.unify(exprTy, valTy)
                let bodyTy = try body.infer(env, level)
                try env.unify(bodyTy, ty)
            }
            
            if let defaultCase {
                let bodyTy = try defaultCase.infer(env, level)
                try env.unify(bodyTy, ty)
            }
            
            tau = ty
        case let .Variant(enumName, variantName, val, ty):
            let enum_: Enum
            if let enumName {
                enum_ = env.enums[enumName]!.variants
            } else {
                enum_ = try env.lookupEnumUnique(variants: [variantName])
            }
            
            let enumTy = Ty.const(enum_.name, [])
            let associatedTy = enum_.mapping[variantName]! ?? .unit
            let valTy = try val?.infer(env, level) ?? .unit
            try env.unify(valTy, associatedTy)
            try env.unify(enumTy, ty)
            tau = ty
        }
        
        let ty = self.ty
        try env.unify(tau, ty)
        
        return ty
    }
}

extension CoreStmt {
    func infer(_ env: TypeEnv, _ level: UInt) throws {
        switch self {
        case let .Expr(expr):
            _ = try expr.infer(env, level)
        case let .Let(isMut, pat, ann, val):
            try pat.checkIfInfallible()
            let (patternTy, patternVars) = try pat.ty(level: level + 1, env: env)
            let rhsEnv = env.child()
            
            for (variable, ty) in patternVars {
                try rhsEnv.declare(variable, ty: ty)
            }
            
            let valTy = try val.infer(rhsEnv, level + 1)
            
            if let ann {
                try env.unify(ann, valTy)
            }
            
            try env.unify(patternTy, valTy)
            
            for (name, ty) in patternVars {
                // https://en.wikipedia.org/wiki/Value_restriction
                try env.declare(
                    name,
                    ty: !isMut ? ty.generalize(level: level) : ty
                )
            }
        case let .For(pat, iterator, body):
            let iterTy = try iterator.infer(env, level)
            let iterItemTy = Ty.fresh(level: level)
            try env.unify(iterTy, .iterator(iterItemTy))
            let bodyEnv = env.child()
            let (patternTy, patternVars) = try pat.ty(level: level, env: env)
            try env.unify(patternTy, iterItemTy)
            
            for (name, ty) in patternVars {
                try bodyEnv.declare(name, ty: ty)
            }
    
            for stmt in body {
                try stmt.infer(bodyEnv, level)
            }
        case let .While(cond, body):
            let condTy = try cond.infer(env, level)
            try env.unify(condTy, .bool)
            let bodyEnv = env.child()
            
            for stmt in body {
                try stmt.infer(bodyEnv, level)
            }
        case let .If(cond, then, else_):
            let condTy = try cond.infer(env, level)
            try env.unify(condTy, .bool)
            let thenEnv = env.child()
            
            for stmt in then {
                try stmt.infer(thenEnv, level)
            }
            
            if let else_ {
                let elseEnv = env.child()
                
                for stmt in else_ {
                    try stmt.infer(elseEnv, level)
                }
            }
        case let .Return(expr):
            let exprTy = try expr?.infer(env, level) ?? .unit
            if let (funcRetTy, _) = TypeEnv.peekFunctionInfo() {
                try env.unify(exprTy, funcRetTy)
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
                } else {
                    try env.unify(funcInfo.returnTy, .iterator(expr.ty))
                }
            }
        }
    }
}

extension CoreDecl {
    func infer(_ env: TypeEnv, _ level: UInt) throws {
        switch self {
        case let .Stmt(stmt):
            try stmt.infer(env, level)
        case let .TypeAlias(name, args, ty):
            env.declareAlias(name: name, args: args, ty: ty)
        case let .Enum(name, args, variants):
            env.declareEnum(name: name, args: args, variants: variants)
        }
    }
}

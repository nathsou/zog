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

class TypeContext {
    let env: TypeEnv
    let resolver: Resolver

    init() {
        env = TypeEnv()
        resolver = Resolver()
    }

    init(env: TypeEnv, resolver: Resolver) {
        self.env = env
        self.resolver = resolver
    }

    func child() -> TypeContext {
        return .init(env: env.child(), resolver: resolver)
    }
}

extension CoreExpr {
    func infer(_ ctx: TypeContext, _ level: UInt, expectedTy: Ty? = nil) throws -> Ty {
        let tau: Ty
        
        if let expectedTy {
            try ctx.env.unify(self.ty, expectedTy)
        }
        
        switch self {
        case let .Literal(lit, _):
            tau = lit.ty
        case let .Var(name, _):
            if let info = ctx.env.lookup(name) {
                tau = info.ty.instantiate(level: level)
            } else {
                throw TypeError.unknownVariable(name)
            }
        case let .Parens(expr, _):
            tau = try expr.infer(ctx, level)
        case let .Fun(args, retTyAnn, body, isIterator, _):
            let bodyCtx = ctx.child()
            var argsInfo = [(pat: CorePattern, ty: Ty, vars: [String:Ty], ann: Ty?)]()
            
            for (pat, ann) in args {
                try pat.checkIfInfallible()
                let (ty, vars) = try pat.ty(level: level, env: ctx.env, expectedTy: ann)
                argsInfo.append((pat, ty, vars, ann))
            }
            
            let argTys = argsInfo.map({ $0.ty })
            
            for (_, ty: patTy, vars, ann) in argsInfo {
                for (variable, ty) in vars {
                    try bodyCtx.env.declare(variable, ty: ty)
                }
                
                if let ann {
                    try ctx.env.unify(patTy, ann)
                }
            }
            
            let innerRetTy = retTyAnn ?? .fresh(level: level)
            let retTy = isIterator ? Ty.iterator(innerRetTy) : innerRetTy
            TypeEnv.pushFunctionInfo(retTy: retTy, isIterator: isIterator)
            var actualRetTy = try body.infer(bodyCtx, level, expectedTy: retTyAnn)
            if isIterator {
                actualRetTy = .iterator(.fresh(level: level))
                try ctx.env.unify(retTy, actualRetTy)
            } else {
                try ctx.env.unify(retTy, actualRetTy)
            }
            TypeEnv.popFunctionInfo()
            
            tau = .fun(argTys, actualRetTy)
        case let .Call(f, args, retTy):
            let argsTy = try args.map({ arg in try arg.infer(ctx, level) })
            let funTy = try f.infer(ctx, level)
            let expectedTy = Ty.fun(argsTy, retTy)
            try ctx.env.unify(expectedTy, funTy)
            
            tau = retTy
        case let .If(cond, then, else_, _):
            _ = try cond.infer(ctx, level, expectedTy: .bool)
            
            var thenTy = Ty.unit
            var elseTy = Ty.unit
            let thenCtx = ctx.child()
            let elseCtx = ctx.child()
            
            for stmt in then {
                try stmt.infer(thenCtx, level)
            }
            
            for stmt in else_ {
                try stmt.infer(elseCtx, level)
            }
            
            if case let .Expr(lastThenExpr) = then.last {
                thenTy = lastThenExpr.ty
            }
            
            if case let .Expr(lastElseExpr) = else_.last {
                elseTy = lastElseExpr.ty
            }
            
            try ctx.env.unify(thenTy, elseTy)
            
            tau = thenTy
        case let .UnaryOp(op, expr, _):
            let exprTy = try expr.infer(ctx, level)
            
            switch op {
            case .arithmeticNegation: tau = .num
            case .logicalNegation: tau = .bool
            }
            
            try ctx.env.unify(exprTy, tau)
        case let .BinaryOp(lhs, op, rhs, _):
            let lhsTy = try lhs.infer(ctx, level)
            let rhsTy = try rhs.infer(ctx, level)
            let argTy: Ty
            let retTy: Ty
            
            switch op {
            case .add, .sub, .mul, .div, .mod, .pow:
                argTy = .num
                retTy = .num
            case .lss, .gtr, .leq, .geq:
                argTy = .num
                retTy = .bool
            case .equ, .neq:
                argTy = Ty.fresh(level: level)
                retTy = .bool
            case .and, .or:
                argTy = .bool
                retTy = .bool
            case .concat:
                argTy = .str
                retTy = .str
            }
            
            try ctx.env.unify(lhsTy, argTy)
            try ctx.env.unify(rhsTy, argTy)
            
            tau = retTy
        case let .Assignment(lhs, op, rhs, _):
            let lhsTy = try lhs.infer(ctx, level)
            let rhsTy = try rhs.infer(ctx, level)
            
            switch op {
            case .plusEq, .minusEq, .timesEq, .divideEq:
                try ctx.env.unify(lhsTy, .num)
                try ctx.env.unify(rhsTy, .num)
            case .eq:
                try ctx.env.unify(lhsTy, rhsTy)
            }
            
            tau = .unit
        case let .Block(stmts, ret, ty):
            let blockCtx = ctx.child()
            
            for stmt in stmts {
                if case let .Return(expr) = stmt, expr != nil {
                    try blockCtx.env.unify(expr!.ty, ty)
                }
                
                try stmt.infer(blockCtx, level)
            }
            
            if let ret {
                _ = try ret.infer(blockCtx, level, expectedTy: ty)
            }
            
            tau = ty
        case let .Tuple(elems, ty):
            let tupleTy = Ty.tuple(elems.map({ $0.ty }))
            try ctx.env.unify(tupleTy, ty)
            
            for elem in elems {
                _ = try elem.infer(ctx, level)
            }
            
            tau = tupleTy
        case let .Array(elems, _):
            let elemTy = Ty.fresh(level: level)
            
            for elem in elems {
                _ = try elem.infer(ctx, level, expectedTy: elemTy)
            }
            
            tau = .array(elemTy)
        case let .ArraySubscript(elems, index, _):
            let itemTy = Ty.fresh(level: level)
            let arrayTy = try elems.infer(ctx, level)
            try ctx.env.unify(arrayTy, .array(itemTy))
            _ = try index.infer(ctx, level, expectedTy: .num)
            tau = itemTy
        case let .Record(entries, ty):
            let recordTy = Ty.record(Row.from(entries: entries.map({ ($0.0, $0.1.ty) })))
            try ctx.env.unify(ty, recordTy)
            
            for (_, val) in entries {
                _ = try val.infer(ctx, level)
            }
            
            tau = recordTy
        case let .RecordSelect(record, field, fieldTy):
            let tail = Ty.fresh(level: level)
            let partialRecordTy = Ty.record(Row.extend(field: field, ty: fieldTy, tail: tail))
            _ = try record.infer(ctx, level, expectedTy: partialRecordTy)
            tau = fieldTy
        case let .Raw(_, ty):
            tau = ty
        case let .Match(expr, cases, ty):
            let exprTy = try expr.infer(ctx, level)
            
            for (pat, body) in cases {
                let (patTy, vars) = try pat.ty(level: level, env: ctx.env, expectedTy: exprTy)
                let bodyCtx = ctx.child()
                for (name, ty) in vars {
                    try bodyCtx.env.declare(name, ty: ty)
                }
                
                try bodyCtx.env.unify(patTy, exprTy)
                _ = try body.infer(bodyCtx, level, expectedTy: ty)
            }
            
            if cases.isEmpty {
                try ctx.env.unify(ty, .unit)
            }
            
            tau = ty
        case let .Switch(expr, cases, defaultCase, ty):
            let exprTy = try expr.infer(ctx, level)
            
            for (val, body) in cases {
                let valTy = try val.infer(ctx, level)
                try ctx.env.unify(exprTy, valTy)
                let bodyTy = try body.infer(ctx, level)
                try ctx.env.unify(bodyTy, ty)
            }
            
            if let defaultCase {
                let bodyTy = try defaultCase.infer(ctx, level)
                try ctx.env.unify(bodyTy, ty)
            }
            
            tau = ty
        case let .Variant(enumName, variantName, val, ty):
            let enum_: EnumVariants
            if let enumName = enumName.ref {
                enum_ = ctx.env.enums[enumName]!.variants
            } else {
                if case let .const(enumName, _) = ty.deref() {
                    enum_ = ctx.env.enums[enumName]!.variants
                } else {
                    enum_ = try ctx.env.lookupEnumUnique(variants: [variantName])
                }
            }
            
            enumName.ref = enum_.name
            let (subst, enumTy) = enum_.instantiate(level: level)
            let associatedTy = enum_.mapping[variantName]!.ty?.substitute(subst)
            _ = try val?.infer(ctx, level, expectedTy: associatedTy)
            
            if val == nil, associatedTy != nil {
                throw TypeError.missingVariantArgument(enumName: enum_.name, variant: variantName)
            } else if val != nil, associatedTy == nil {
                throw TypeError.extraneousVariantArgument(enumName: enum_.name, variant: variantName)
            }
            
            try ctx.env.unify(enumTy, ty)
            tau = ty
        case let .BuiltInCall(name, args, _):
            for arg in args {
                _ = try arg.infer(ctx, level)
            }
            
            switch name {
            case "type":
                guard args.count == 1 else {
                    throw TypeError.wrongNumberOfArguments(
                        name: "@type",
                        expected: 1,
                        got: args.count
                    )
                }
                
                tau = .const("Type", [])
            default:
                throw TypeError.invalidBuiltInCall(name)
            }
}
        
        let ty = self.ty
        try ctx.env.unify(tau, ty)
        
        return ty
    }
}

func inferLet(
    pub: Bool,
    mut: Bool,
    pattern: CorePattern,
    annotation: Ty?,
    value: CoreExpr,
    ctx: TypeContext,
    level: UInt
) throws {
    try pattern.checkIfInfallible()
    let (patternTy, patternVars) = try pattern.ty(
        level: level + 1,
        env: ctx.env,
        expectedTy: annotation
    )
    
    let rhsCtx = ctx.child()
    
    for (variable, ty) in patternVars {
        try rhsCtx.env.declare(variable, ty: ty, pub: pub)
    }
    
    let valTy = try value.infer(rhsCtx, level + 1, expectedTy: annotation)
    
    try ctx.env.unify(patternTy, valTy)
    
    for (name, ty) in patternVars {
        // https://en.wikipedia.org/wiki/Value_restriction
        let genTy = !mut ? ty.generalize(level: level) : ty
        try ctx.env.declare(
            name,
            ty: genTy,
            pub: pub
        )
    }
}

extension CoreStmt {
    func infer(_ ctx: TypeContext, _ level: UInt) throws {
        switch self {
        case let .Expr(expr):
            _ = try expr.infer(ctx, level)
        case let .Let(mut, pat, ann, val):
            try inferLet(
                pub: false,
                mut: mut,
                pattern: pat,
                annotation: ann,
                value: val,
                ctx: ctx,
                level: level
            )
        case let .For(pat, iterator, body):
            let iterTy = try iterator.infer(ctx, level)
            let iterItemTy: Ty
            
            // Arrays are iterators
            if case let .const("Array", args) = iterTy.deref(), args.count == 1 {
                iterItemTy = args[0]
            } else {
                iterItemTy = Ty.fresh(level: level)
                try ctx.env.unify(iterTy, .iterator(iterItemTy))
            }
            
            let bodyCtx = ctx.child()
            let (patternTy, patternVars) = try pat.ty(
                level: level,
                env: ctx.env,
                expectedTy: iterItemTy
            )
            try ctx.env.unify(patternTy, iterItemTy)
            
            for (name, ty) in patternVars {
                try bodyCtx.env.declare(name, ty: ty)
            }
    
            for stmt in body {
                try stmt.infer(bodyCtx, level)
            }
        case let .While(cond, body):
            let condTy = try cond.infer(ctx, level)
            try ctx.env.unify(condTy, .bool)
            let bodyCtx = ctx.child()
            
            for stmt in body {
                try stmt.infer(bodyCtx, level)
            }
        case let .If(cond, then, else_):
            let condTy = try cond.infer(ctx, level)
            try ctx.env.unify(condTy, .bool)
            let thenCtx = ctx.child()
            
            for stmt in then {
                try stmt.infer(thenCtx, level)
            }
            
            if let else_ {
                let elseCtx = ctx.child()
                
                for stmt in else_ {
                    try stmt.infer(elseCtx, level)
                }
            }
        case let .Return(expr):
            let exprTy = try expr?.infer(ctx, level) ?? .unit
            if let (funcRetTy, _) = TypeEnv.peekFunctionInfo() {
                try ctx.env.unify(exprTy, funcRetTy)
            } else {
                throw TypeError.cannotReturnOutsideFunctionBody
            }
        case .Break:
            break
        case let .Yield(expr):
            _ = try expr.infer(ctx, level)
            if case let funcInfo? = TypeEnv.peekFunctionInfo() {
                if !funcInfo.isIterator {
                    throw TypeError.cannotYieldOutsideIteratorBody
                } else {
                    try ctx.env.unify(funcInfo.returnTy, .iterator(expr.ty))
                }
            }
        }
    }
}

extension CoreDecl {
    func infer(_ ctx: TypeContext, _ level: UInt) throws {
        switch self {
        case let .Let(pub, mut, pat, ann, val):
            try inferLet(
                pub: pub,
                mut: mut,
                pattern: pat,
                annotation: ann,
                value: val,
                ctx: ctx,
                level: level
            )
        case let .Stmt(stmt):
            try stmt.infer(ctx, level)
        case let .TypeAlias(pub, name, args, ty):
            ctx.env.declareAlias(name: name, args: args, ty: ty, pub: pub)
        case let .Enum(pub, name, args, variants):
            ctx.env.declareEnum(name: name, args: args, variants: variants, pub: pub)
        case let .Declare(pub, name, ty):
            try ctx.env.declare(name, ty: ty.generalize(level: level), pub: pub)
        case let .Import(path, members):
            if let mod = try ctx.resolver.resolve(path: path, level: level) {
                for member in members! {
                    if let info = mod.members[member] {
                        try ctx.env.declare(member, ty: info.ty, pub: false)
                    } else if let info = mod.enums[member] {
                        ctx.env.declareEnum(
                            name: member,
                            args: info.args,
                            variants: info.variants.variants,
                            pub: false
                        )
                    } else if let info = mod.typeAliases[member] {
                        ctx.env.declareAlias(name: member, args: info.args, ty: info.ty, pub: false)
                    } else {
                        throw TypeError.couldNotResolveMember(modulePath: path, member: member)
                    }
                }
            }
        }
    }
}

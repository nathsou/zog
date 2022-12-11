//
//  Type.swift
//
//
//  Created by Nathan on 14/11/2022.
//

import Foundation

public typealias TyVarId = UInt

class TyContext {
    static var nextTyVarId: TyVarId = 0

    static func freshTyVarId() -> TyVarId {
        let id = nextTyVarId
        nextTyVarId += 1
        return id
    }
}

class EnumVariants {
    let name: String
    let args: [TyVarId]
    let variants: [(name: String, ty: Ty?)]
    let mapping: [String:(id: Int, ty: Ty?)]
    
    init(name: String, args: [TyVarId], variants: [(name: String, ty: Ty?)]) {
        self.name = name
        self.variants = variants
        self.args = args
        var mapping = [String:(Int, Ty?)]()
        
        for (id, (name, ty)) in variants.enumerated() {
            mapping[name] = (id, ty)
        }
        
        self.mapping = mapping
    }
    
    func instantiate(level: UInt) -> (subst: [TyVarId:Ty], ty: Ty) {
        var subst = [TyVarId:Ty]()
        
        for arg in args {
            subst[arg] = .fresh(level: level)
        }
        
        let argTys = args.map({ id in Ty.variable(Ref(.unbound(id: id, level: level))) })
        
        return (subst, .const(name, argTys).substitute(subst))
    }
}

class TypeEnv: CustomStringConvertible {
    typealias VarInfo = (ty: Ty, pub: Bool)
    typealias AliasInfo = (args: [TyVarId], ty: Ty, pub: Bool)
    typealias EnumInfo = (args: [TyVarId], variants: EnumVariants, pub: Bool)
    typealias RewritingRuleInfo = (args: [String], rhs: Expr, pub: Bool)
    
    let parent: TypeEnv?
    var vars = [String:VarInfo]()
    var aliases = [String:AliasInfo]()
    var enums = [String:EnumInfo]()
    var rewritingRules = [String:RewritingRuleInfo]()
    typealias FunctionInfo = (returnTy: Ty, isIterator: Bool)
    static var functionInfoStack = [FunctionInfo]()

    init() {
        parent = nil
    }
    
    init(parent: TypeEnv? = nil) {
        self.parent = parent
    }
    
    func lookup(_ name: String) -> VarInfo? {
        if let info = vars[name] {
            return info
        }
        
        return parent?.lookup(name)
    }
    
    func contains(_ name: String) -> Bool {
        return lookup(name) != nil
    }
    
    func declare(_ name: String, ty: Ty, pub: Bool = false) throws {
        guard vars[name] == nil else {
            throw TypeError.cannotRedeclareVariable(name)
        }
        
        vars[name] = (ty, pub)
    }
    
    func declareAlias(name: String, args: [TyVarId], ty: Ty, pub: Bool) {
        aliases[name] = (args, ty, pub)
    }
    
    func lookupAlias(name: String) throws -> AliasInfo {
        if let alias = aliases[name] {
            return alias
        }
        
        if let parent {
            return try parent.lookupAlias(name: name)
        }
        
        throw TypeError.couldNotResolveType(.const(name, []))
    }
    
    func declareEnum(name: String, args: [TyVarId], variants: [(name: String, ty: Ty?)], pub: Bool) {
        enums[name] = (args, .init(name: name, args: args, variants: variants), pub: pub)
    }
    
    func lookupEnums(variants: [String]) -> [EnumVariants] {
        return enums.filter({ (name, enum_) in variants.allSatisfy({ variant in enum_.variants.mapping.keys.contains(variant) })
        }).map({ $0.value.variants })
    }
    
    func lookupEnumUnique(variants: [String]) throws -> EnumVariants {
        let matchingEnums = lookupEnums(variants: variants)
        
        if matchingEnums.isEmpty {
            throw TypeError.noEnumMatchesVariants(variants: variants)
        }
        
        if matchingEnums.count > 1 {
            throw TypeError.ambiguousEnumForVariants(
                variants: variants,
                candidates: matchingEnums.map({ $0.name })
            )
        }
        
        return matchingEnums[0]
    }
    
    func declareRule(name: String, args: [String], rhs: Expr, pub: Bool) {
        rewritingRules[name] = (args, rhs, pub)
    }
    
    func lookupRule(_ name: String) -> RewritingRuleInfo? {
        return rewritingRules[name]
    }
    
    func containsRule(_ name: String) -> Bool {
        return rewritingRules.keys.contains(name)
    }

    func child() -> TypeEnv {
        let child = TypeEnv.init(parent: self)
        child.aliases = aliases
        child.enums = enums
        return child
    }
    
    static func pushFunctionInfo(retTy: Ty, isIterator: Bool) {
        TypeEnv.functionInfoStack.append((retTy, isIterator))
    }
    
    static func popFunctionInfo() {
        _ = TypeEnv.functionInfoStack.popLast()
    }
    
    static func peekFunctionInfo() -> FunctionInfo? {
        return TypeEnv.functionInfoStack.last
    }
    
    public var description: String {
        let vars = self.vars.map({ (v, info) in indent("pub ".when(info.pub) + "\(v): \(info.ty.canonical)") })
        return "{\n\(vars.joined(separator: ",\n"))\n}"
    }
}

enum TyVar: Equatable, CustomStringConvertible {
    case unbound(id: TyVarId, level: UInt)
    case link(Ty)
    case generic(TyVarId)

    static func showTyVarId(_ id: TyVarId) -> String {
        let char = UnicodeScalar(65 + Int(id % 26))!

        if id > 26 {
            return "\(char)\(id / 26)"
        } else {
            return "\(char)"
        }
    }

    public var description: String {
        switch self {
        case let .unbound(id, _):
            return TyVar.showTyVarId(id)
        case let .link(ty):
            return "\(ty)"
        case let .generic(id):
            return "'" + TyVar.showTyVarId(id)
        }
    }

    static func fresh(level: UInt) -> TyVar {
        return .unbound(id: TyContext.freshTyVarId(), level: level)
    }
    
    public static func == (lhs: TyVar, rhs: TyVar) -> Bool {
        switch (lhs, rhs) {
        case let (.unbound(id: id1, _), .unbound(id: id2, _)):
            return id1 == id2
        case let (.link(to1), .link(to2)):
            return to1.deref() == to2.deref()
        default:
            return false
        }
    }
}

class Ref<T> {
    var ref: T

    init(_ ref: T) {
        self.ref = ref
    }
}

enum Row: Equatable {
    case empty
    case extend(field: String, ty: Ty, tail: Ty)
    
    public static func from(entries: [(name: String, ty: Ty)], tail: Ty = .record(.empty)) -> Row {
        var row = Row.empty
        
        for (index, (field, ty)) in entries.sorted(by: { $0.name < $1.name  }).enumerated() {
            let rest = index == 0 ? tail : .record(row)
            row = .extend(field: field, ty: ty, tail: rest)
        }
        
        return row
    }
    
    func entries(sorted: Bool) -> [(key: String, ty: Ty)] {
        var entries = [(key: String, ty: Ty)]()
        
        func aux(_ row: Row) {
            switch row {
            case .empty:
                break
            case let .extend(field, ty, tail):
                entries.append((field, ty))
                switch tail.deref() {
                case let .record(tailRow):
                    aux(tailRow)
                default:
                    break
                }
            }
        }
        
        aux(self)
        
        if sorted {
            return entries.sorted(by: { $0.key < $1.key })
        }
        
        return entries
    }
    
    func asDictionary() -> [String:Ty] {
        return .init(uniqueKeysWithValues: entries(sorted: false))
    }
    
    func map(types f: (_ ty: Ty) -> Ty) -> Row {
        switch self {
        case .empty:
            return .empty
        case let .extend(field, ty, tail):
            switch tail {
            case let .record(row):
                return .extend(field: field, ty: f(ty), tail: .record(row.map(types: f)))
            default:
                return .extend(field: field, ty: f(ty), tail: tail)
            }
        }
    }
    
    func indexOf(field: String) -> Int? {
        return entries(sorted: true).firstIndex(where: { $0.key == field })
    }
}

indirect enum Ty: Equatable, CustomStringConvertible {
    case variable(Ref<TyVar>)
    case const(String, [Ty])
    case fun([Ty], Ty)
    case record(Row)

    static var num: Ty {
        return .const("num", [])
    }

    static var str: Ty {
        return .const("str", [])
    }

    static var bool: Ty {
        return .const("bool", [])
    }
    
    static var unit: Ty {
        return .const("unit", [])
    }

    public static func tuple(_ elems: [Ty]) -> Ty {
        return .const("Tuple", elems)
    }
    
    public static func array(_ elemTy: Ty) -> Ty {
        return .const("Array", [elemTy])
    }
    
    public static func iterator(_ ty: Ty) -> Ty {
        return .const("Iterator", [ty])
    }
    
    public static func fresh(level l: UInt) -> Ty {
        return .variable(Ref(TyVar.fresh(level: l)))
    }
    
    public var description: String {
        return show(canonical: false)
    }
    
    public var canonical: String {
        return show(canonical: true)
    }
    
    public func show(canonical: Bool) -> String {
        var generics = Set<TyVarId>()
        var tyVarNames = [TyVarId:String]()
        
        func canonicalized(_ id: TyVarId) -> String {
            if !canonical {
                return TyVar.showTyVarId(id)
            }
            
            if let v = tyVarNames[id] {
                return v
            }
            
            let v = TyVar.showTyVarId(UInt(tyVarNames.count))
            tyVarNames[id] = v
            return v
        }
        
        func go(_ ty: Ty) -> String {
            switch ty {
            case let .variable(tyVar):
                switch tyVar.ref {
                case let .unbound(id, _):
                    return canonicalized(id)
                case let .link(to):
                    return go(to)
                case let .generic(id):
                    generics.insert(id)
                    return canonicalized(id).lowercased()
                }
            case .const("unit", []):
                return "()"
            case let .const("Tuple", elems):
                return "(\(elems.map(go).joined(separator: ", ")))"
            case let .const("Array", args) where args.count == 1:
                return "\(go(args[0]))[]"
            case let .const(name, []):
                return name
            case let .const(name, args):
                return "\(name)<\(args.map(go).joined(separator: ", "))>"
            case let .fun(args, ret) where args.count == 1:
                if case .const("Tuple", _) = args[0] {
                    return "(\(go(args[0]))) => \(go(ret))"
                } else {
                    return "\(go(args[0])) => \(go(ret))"
                }
            case let .fun(args, ret):
                return "(\(args.map(go).joined(separator: ", "))) => \(go(ret))"
            case let .record(row):
                let entries = row.entries(sorted: true)
                if entries.isEmpty {
                    return "{}"
                } else {
                    let fields = row
                        .entries(sorted: true)
                        .map({ (k, v) in "\(k): \(go(v))" })
                        .joined(separator: ", ")
                    
                    return "{ \(fields) }"
                }
            }
        }
        
        return go(self)
    }

    func deref() -> Ty {
        if case let .variable(tyVar) = self, case let .link(ty) = tyVar.ref {
            let res = ty.deref()
            tyVar.ref = .link(res)
            return res
        }

        return self
    }
    
    func substitute(mappingFunc: (TyVarId) -> Ty?) -> Ty {
        func aux(_ ty: Ty) -> Ty {
            switch ty {
            case let .variable(v):
                switch v.ref {
                case let .unbound(id, level):
                    return mappingFunc(id) ?? .variable(Ref(.unbound(id: id, level: level)))
                case let .link(to):
                    return aux(to)
                case let .generic(id):
                    return mappingFunc(id) ?? .variable(Ref(.generic(id)))
                }
            case let .const(name, args):
                return .const(name, args.map(aux))
            case let .fun(args, ret):
                return .fun(args.map(aux), aux(ret))
            case let .record(row):
                return .record(row.map(types: aux))
            }
        }
        
        return aux(self)
    }

    func substitute(_ mapping: [TyVarId:Ty]) -> Ty {
        return substitute(mappingFunc: { id in mapping[id] })
    }
    
    static func == (s: Ty, t: Ty) -> Bool {
        return "\(s)" == "\(t)"
    }
}

func occursCheckAdjustLevels(id: UInt, level: UInt, ty: Ty) throws {
    func go(_ t: Ty) throws {
        switch t {
        case let .variable(v):
            switch v.ref {
            case let .link(t):
                try go(t)
            case let .unbound(id: otherId , level: otherLvl):
                if otherId == id {
                    throw TypeError.recursiveType(.variable(Ref(.unbound(id: id, level: level))), ty)
                }

                if otherLvl > level {
                    v.ref = .unbound(id: otherId, level: level)
                }
            case .generic(_):
                fatalError("generic ty")
         }
        case let .const(_, args):
            for arg in args {
                try go(arg)
            }
        case let .fun(args, ret):
            for arg in args {
                try go(arg)
            }

            try go(ret)
        case let .record(row):
            for (_, ty) in row.entries(sorted: false) {
                try go(ty)
            }
        }
    }
    
    try go(ty)
}

let primitiveTypes = Set(["num", "str", "bool", "unit", "Tuple", "Array", "Iterator"])

extension TypeEnv {
        // see https://github.com/tomprimozic/type-systems
    func unify(_ s: Ty, _ t: Ty) throws {
        let startingEq = (s, t)
        var eqs = [(s, t)]
        
        while let (s, t) = eqs.popLast() {
            if s != t {
                switch (s, t) {
                case let (.const(f, args1), .const(g, args2)) where f == g && args1.count == args2.count:
                    eqs.append(contentsOf: zip(args1, args2))
                case let (.const(f, _), other) where !primitiveTypes.contains(f) && !enums.keys.contains(f),
                     let (other, .const(f, _)) where !primitiveTypes.contains(f) && !enums.keys.contains(f):
                    let (_, alias, _) = try self.lookupAlias(name: f)
                    eqs.append((alias, other))
                case let (.fun(args1, ret1), .fun(args2, ret2)) where args1.count == args2.count:
                    eqs.append(contentsOf: zip(args1, args2))
                    eqs.append((ret1, ret2))
                case let (.variable(v), ty), let (ty, .variable(v)):
                    switch v.ref {
                    case let .unbound(id, lvl):
                        if case let .variable(v2) = ty, case let .unbound(id2, _) = v2.ref, id2 == id {
                            assertionFailure("There should only be one instance of a particular type variable.")
                        }
                        
                        try occursCheckAdjustLevels(id: id, level: lvl, ty: ty)
                        
                        v.ref = .link(ty)
                    case let .link(to):
                        eqs.append((to, ty))
                    case .generic(_):
                        throw TypeError.cannotUnify(startingEq, failedWith: (s, t))
                    }
                case let (.record(r1), .record(r2)):
                    switch (r1, r2) {
                    case (.empty, .empty):
                        break
                    case let (.extend(field1, ty1, tail1), .extend(_, _, _)):
                        let isTailUnbound: Bool
                        if case let .variable(v) = tail1, case .unbound(_, _) = v.ref {
                            isTailUnbound = true
                        } else {
                            isTailUnbound = false
                        }
                        
                        let tail2 = try rewriteRow(t, field: field1, ty: ty1)
                        
                        if isTailUnbound, case let .variable(v) = tail1, case .link(_) = v.ref {
                            throw TypeError.recursiveType(.record(r1), .record(r2))
                        }
                        
                        eqs.append((tail1, tail2))
                    default:
                        throw TypeError.cannotUnify(startingEq, failedWith: (.record(r1), .record(r2)))
                    }
                default:
                    throw TypeError.cannotUnify(startingEq, failedWith: (s, t))
                }
            }
        }
    }
    
    fileprivate func rewriteRow(_ row2: Ty, field: String, ty: Ty) throws -> Ty {
        switch row2 {
        case .record(let row):
            switch row {
            case .empty:
                throw TypeError.recordDoesNotContainField(row2, field)
            case let .extend(field: field2, ty: ty2, tail: tail2):
                if field2 == field {
                    try unify(ty2, ty)
                    return tail2
                }
                
                return .record(
                    .extend(
                        field: field2,
                        ty: ty2,
                        tail: try rewriteRow(tail2, field: field, ty: ty)
                    )
                )
            }
        case .variable(let v):
            switch v.ref {
            case let .unbound(_, level):
                let tail2 = Ty.fresh(level: level)
                let row2 = Row.extend(field: field, ty: ty, tail: tail2)
                v.ref = .link(.record(row2))
                return tail2
            case let .link(to):
                return try rewriteRow(to, field: field, ty: ty)
            default:
                break
            }
        default:
            break
        }
        
        throw TypeError.expectedRecordType(row2)
    }
}

extension Ty {
    func generalize(level: UInt) -> Ty {
        switch self {
        case let .variable(v):
            switch v.ref {
            case let .unbound(id, lvl) where lvl > level:
                return .variable(Ref(.generic(id)))
            case let .link(to):
                return to.generalize(level: level)
            default:
                return self
            }
        case let .const(name, args):
            return .const(name, args.map({ $0.generalize(level: level) }))
        case let .fun(args, ret):
            return .fun(args.map({ $0.generalize(level: level) }), ret.generalize(level: level))
        case let .record(row):
            return .record(row.map(types: { ty in ty.generalize(level: level) }))
        }
    }
    
    func instantiate(level: UInt) -> Ty {
        var idVarMap = [TyVarId:Ty]()
        
        func go(_ ty: Ty) -> Ty {
            switch ty {
            case let .const(name, args):
                return .const(name, args.map(go))
            case let .variable(v):
                switch v.ref {
                case let .link(to):
                    return go(to)
                case .unbound(_, _):
                    return ty
                case let .generic(id):
                    if let inst = idVarMap[id] {
                        return inst
                    } else {
                        let inst = Ty.fresh(level: level)
                        idVarMap[id] = inst
                        return inst
                    }
                }
            case let .fun(args, ret):
                return .fun(args.map(go), go(ret))
            case let .record(row):
                return .record(row.map(types: go))
            }
        }
        
        return go(self)
    }
}

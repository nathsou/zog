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
    typealias FunctionInfo = (returnTy: Ty, isIterator: Bool)
    typealias AliasInfo = (args: [TyVarId], ty: Ty, pub: Bool)
    typealias EnumInfo = (args: [TyVarId], variants: EnumVariants, pub: Bool)
    typealias RewritingRuleInfo = (args: [String], rhs: Expr, pub: Bool)
    typealias TraitInfo = (subjectTy: Ty, methods: [String:(ty: Ty, isStatic: Bool)], pub: Bool)
    typealias TraitImplInfo = (implementee: Ty, context: [TyVarId:TyVar.Context])
    
    let parent: TypeEnv?
    let depth: Int
    var vars = [String:VarInfo]()
    var aliases = [String:AliasInfo]()
    var enums = [String:EnumInfo]()
    var rewritingRules = [String:RewritingRuleInfo]()
    var traits = [String:TraitInfo]()
    var traitImpls = [String:[TraitImplInfo]]()
    var traitMethods: [String:[String]] = [:]
    static var functionInfoStack = [FunctionInfo]()

    init() {
        parent = nil
        depth = 0
    }
    
    init(parent: TypeEnv? = nil) {
        self.parent = parent
        depth = (parent?.depth ?? -1) + 1
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
    
    func declareAlias(name: String, args: [TyVarId], ty: Ty, pub: Bool, level: UInt) {
        let subst = ty.substitute(
            Dictionary(uniqueKeysWithValues: args.map({ id in (id, Ty.variable(Ref(TyVar.generic(id)))) }))
        )

        aliases[name] = (args, subst.generalize(level: level + 1), pub)
    }
    
    func lookupAlias(name: String, args: [Ty]) throws -> Ty {
        if let alias = aliases[name] {
            let subst = Dictionary(uniqueKeysWithValues: zip(alias.args, args))
            return try alias.ty.substitute(subst).instantiate(level: 0, env: self).ty
        }
        
        if let parent {
            return try parent.lookupAlias(name: name, args: args)
        }
        
        throw TypeError.couldNotResolveType(.const(name, args))
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

    func declareTrait(name: String, subjectTy: Ty, methods: [String:(ty: Ty, isStatic: Bool)], pub: Bool) {
        traits[name] = (subjectTy, methods, pub)

        for method in methods.keys {
            if traitMethods[method] == nil {
                traitMethods[method] = [name]
            } else {
                traitMethods[method]!.append(name)
            }
        }
    }

    func lookupTrait(_ name: String) -> TraitInfo? {
        return traits[name]
    }

    func declareTraitImpl(trait: String, implementee: Ty, context: [TyVarId:TyVar.Context]) {
        if traitImpls[trait] == nil {
            traitImpls[trait] = [TraitImplInfo(implementee: implementee, context: context)]
        } else {
            traitImpls[trait]!.append(TraitImplInfo(implementee: implementee, context: context))
        }
    }

    func child() -> TypeEnv {
        let child = TypeEnv.init(parent: self)
        child.aliases = aliases
        child.enums = enums
        child.traits = traits
        child.traitMethods = traitMethods
        child.traitImpls = traitImpls
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
    typealias Context = [String]
    
    case unbound(id: TyVarId, level: UInt, context: Ref<Context> = Ref([]))
    case link(Ty)
    case generic(TyVarId, context: Ref<Context> = Ref([]))

    static func showId(_ id: TyVarId) -> String {
        let char = UnicodeScalar(65 + Int(id % 26))!

        if id > 26 {
            return "\(char)\(id / 26)"
        } else {
            return "\(char)"
        }
    }

    public var description: String {
        switch self {
        case let .unbound(id, _, context):
            if !context.ref.isEmpty {
                return TyVar.showId(id) + ": " + context.ref.joined(separator: " + ")
            } 
            
            return TyVar.showId(id)
        case let .link(ty):
            return "\(ty)"
        case let .generic(id, context):
            return TyVar.showId(id).lowercased() + ": " + context.ref.joined(separator: " + ")
        }
    }

    static func fresh(level: UInt) -> TyVar {
        return .unbound(id: TyContext.freshTyVarId(), level: level)
    }
    
    public static func == (lhs: TyVar, rhs: TyVar) -> Bool {
        switch (lhs, rhs) {
        case let (.unbound(id: id1, _, _), .unbound(id: id2, _, _)):
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
    
    func map(types f: (_ ty: Ty) throws -> Ty) rethrows -> Row {
        switch self {
        case .empty:
            return .empty
        case let .extend(field, ty, tail):
            switch tail {
            case let .record(row):
                return .extend(field: field, ty: try f(ty), tail: .record(try row.map(types: f)))
            default:
                return .extend(field: field, ty: try f(ty), tail: tail)
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

    public static func map(_ keyTy: Ty, _ valueTy: Ty) -> Ty {
        return .const("Map", [keyTy, valueTy])
    }

    public static func iterator(_ ty: Ty) -> Ty {
        return .const("Iterator", [ty])
    }
    
    public static func fresh(level l: UInt) -> Ty {
        return .variable(Ref(TyVar.fresh(level: l)))
    }
    
    public var description: String {
        return show(canonical: false, traits: true)
    }
    
    public var canonical: String {
        return show(canonical: true, traits: true)
    }
    
    public func show(canonical: Bool, traits: Bool) -> String {
        var generics = Set<TyVarId>()
        var tyVarNames = [TyVarId:String]()
        var tyVarTraits = [TyVarId:Set<String>]()
        
        func canonicalized(_ id: TyVarId) -> String {
            if !canonical {
                return TyVar.showId(id)
            }
            
            if let v = tyVarNames[id] {
                return v
            }
            
            let v = TyVar.showId(UInt(tyVarNames.count))
            tyVarNames[id] = v
            return v
        }
        
        func go(_ ty: Ty) -> String {
            switch ty {
            case let .variable(tyVar):
                switch tyVar.ref {
                case let .unbound(id, level, traits):
                    if !traits.ref.isEmpty {
                        if tyVarTraits.keys.contains(id) {
                            for trait in traits.ref {
                                tyVarTraits[id]!.insert(trait)
                            }
                        } else {
                            tyVarTraits[id] = Set(traits.ref)
                        }
                    }

                    return "\(canonicalized(id))_\(level)"
                case let .link(to):
                    return go(to)
                case let .generic(id, traits):
                    if !traits.ref.isEmpty {
                        if tyVarTraits.keys.contains(id) {
                            for trait in traits.ref {
                                tyVarTraits[id]!.insert(trait)
                            }
                        } else {
                            tyVarTraits[id] = Set(traits.ref)
                        }
                    }

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

        let rhs = go(self)
        
        if traits && !tyVarTraits.isEmpty {        
            let contextFmt = tyVarTraits.map({ (id, traits) in 
                var name = canonicalized(id)
                if generics.contains(id) {
                    name = name.lowercased()
                }
                let traits = traits.joined(separator: " + ")
                return "\(name): \(traits)"
            }).joined(separator: ", ")

            return ("\(rhs) where \(contextFmt)")
        }

        return rhs
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
                case let .unbound(id, level, traits):
                    return mappingFunc(id) ?? .variable(Ref(.unbound(id: id, level: level, context: Ref(Array(traits.ref)))))
                case let .link(to):
                    return aux(to)
                case let .generic(id, context):
                    return mappingFunc(id) ?? .variable(Ref(.generic(id, context: Ref(Array(context.ref)))))
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

    func subTypes() -> [Ty] {
        switch self {
        case let .variable(v):
            switch v.ref {
            case let .link(t):
                return t.subTypes()
            case .unbound(_, _, _), .generic(_, _):
                return []
            }
        case let .const(_, args):
            return args
        case let .fun(args, ret):
            return args + [ret]
        case let .record(row):
            return row.entries(sorted: true).map({ $0.1 })
        }
    }

    func unboundVars() -> Set<TyVarId> {
        var vars = Set<TyVarId>()

        func go(_ ty: Ty) {
            switch ty {
            case let .variable(v):
                switch v.ref {
                case let .link(t):
                    go(t)
                case let .unbound(id, _, _):
                    vars.insert(id)
                case .generic(_, _):
                    break
                }
            case let .const(_, args):
                args.forEach(go)
            case let .fun(args, ret):
                args.forEach(go)
                go(ret)
            case let .record(row):
                row.entries(sorted: true).forEach({ (_, ty) in go(ty) })
            }
        }

        go(self)

        return vars
    }

    typealias TraitBounds = [String:[Ty]]

    func traitBounds() -> TraitBounds {
        var bounds = TraitBounds()

        func go(_ ty: Ty) {
            switch ty {
            case let .variable(v):
                switch v.ref {
                case let .link(t):
                    go(t)
                case let .unbound(_, _, traits), let .generic(_, context: traits):
                    traits.ref.forEach({ bounds[$0, default: []].append(ty) })
                }
            case let .const(_, args):
                args.forEach(go)
            case let .fun(args, ret):
                args.forEach(go)
                go(ret)
            case let .record(row):
                row.entries(sorted: true).forEach({ (_, ty) in go(ty) })
            }
        }

        go(self)

        return bounds
    }
    
    func asVariable() -> TyVar? {
        switch self {
        case let .variable(v):
            return v.ref
        default:
            return nil
        }
    }
    
    func tyVarId() -> TyVarId? {
        switch self {
        case let .variable(v):
            switch v.ref {
            case let .link(t):
                return t.tyVarId()
            case let .unbound(id, _, _):
                return id
            case let .generic(id, _):
                return id
            }
        default:
            return nil
        }
    }
    
    func rewrite(_ f: (Ty) -> Ty) -> Ty {
        func go(_ ty: Ty) -> Ty {
            switch ty {
            case let .variable(v):
                switch v.ref {
                case let .link(t):
                    return go(t)
                case .unbound(_, _, _), .generic(_, _):
                    return f(ty)
                }
            case let .const(name, args):
                return f(.const(name, args.map(go)))
            case let .fun(args, ret):
                return f(.fun(args.map(go), go(ret)))
            case let .record(row):
                return f(.record(row.map(types: go)))
            }
        }
        
        return go(self)
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
            case let .unbound(id: otherId , level: otherLvl, traits):
                if otherId == id {
                    throw TypeError.recursiveType(.variable(Ref(.unbound(id: id, level: level))), ty)
                }

                if otherLvl > level {
                    v.ref = .unbound(id: otherId, level: level, context: traits)
                }
            case .generic(_, _):
                fatalError("generic type")
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

extension Ty.TraitBounds {
    func merging(_ other: Ty.TraitBounds) -> Ty.TraitBounds {
        var bounds = self

        for (trait, tys) in other {
            bounds[trait, default: []].append(contentsOf: tys)
        }

        return bounds
    }

    func tys() -> [Ty] {
        return self.values.flatMap({ $0 })
    }
}

let primitiveTypes = Set(["num", "str", "bool", "unit", "Tuple", "Array", "Map", "Iterator"])

typealias Subst = Ref<[TyVarId:Ty]>

extension Ref<TyVar> {
    func linkTo(_ ty: Ty, _ subst: Subst? = nil) {
        switch self.ref {
        case let .unbound(id, _, _):
            if let subst = subst {
                subst.ref[id] = ty
            } else {
                self.ref = .link(ty) 
            }
        case .link(_):
            fatalError("linking to a link")
        case .generic(_, _):
            fatalError("linking to a generic")
        }
    }
}

extension TypeEnv {
    // see https://github.com/tomprimozic/type-systems
    func unify(_ s: Ty, _ t: Ty, subst: Subst? = nil) throws {
        let startingEq = (s, t)
        var eqs = [(s, t)]
        
        while let (s, t) = eqs.popLast() {
            if globalParameters.showUnification {
                print("unify \(s) with \(t)")
            }

            if s != t {
                switch (s, t) {
                case let (.const(f, args1), .const(g, args2)) where f == g && args1.count == args2.count:
                    eqs.append(contentsOf: zip(args1, args2))
                case let (.const(f, args), other) where !primitiveTypes.contains(f) && !enums.keys.contains(f),
                     let (other, .const(f, args)) where !primitiveTypes.contains(f) && !enums.keys.contains(f):
                    let inst = try self.lookupAlias(name: f, args: args)
                    eqs.append((inst, other))
                case let (.fun(args1, ret1), .fun(args2, ret2)) where args1.count == args2.count:
                    eqs.append(contentsOf: zip(args1, args2))
                    eqs.append((ret1, ret2))
                case let (.variable(v), ty), let (ty, .variable(v)):
                    switch v.ref {
                    case let .unbound(id, lvl, traits):
                        if case let .variable(v2) = ty, case let .unbound(id2, _, _) = v2.ref, id2 == id {
                            assertionFailure("There should only be one instance of a particular type variable.")
                        }
                        
                        try occursCheckAdjustLevels(id: id, level: lvl, ty: ty)
                        try propagateTraits(traits: traits.ref, ty: ty, level: lvl) 

                        v.linkTo(ty, subst)
                    case let .link(to):
                        eqs.append((to, ty))
                    case .generic(_, _):
                        throw TypeError.cannotUnify(startingEq, failedWith: (s, t))
                    }
                case let (.record(r1), .record(r2)):
                    switch (r1, r2) {
                    case (.empty, .empty):
                        break
                    case let (.extend(field1, ty1, tail1), .extend(_, _, _)):
                        let isTailUnbound: Bool
                        if case let .variable(v) = tail1, case .unbound(_, _, _) = v.ref {
                            isTailUnbound = true
                        } else {
                            isTailUnbound = false
                        }
                        
                        let tail2 = try rewriteRow(t, field: field1, ty: ty1, subst: subst)
                        
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
    
    fileprivate func rewriteRow(_ row2: Ty, field: String, ty: Ty, subst: Subst?) throws -> Ty {
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
                        tail: try rewriteRow(tail2, field: field, ty: ty, subst: subst)
                    )
                )
            }
        case .variable(let v):
            switch v.ref {
            case let .unbound(_, level, _):
                let tail2 = Ty.fresh(level: level)
                let row2 = Row.extend(field: field, ty: ty, tail: tail2)
                v.linkTo(.record(row2), subst)
                return tail2
            case let .link(to):
                return try rewriteRow(to, field: field, ty: ty, subst: subst)
            default:
                break
            }
        default:
            break
        }
        
        throw TypeError.expectedRecordType(row2)
    }

    // from "Implementing Type Classes" by John Peterson and Mark Jones
    // trait Show { fun show(self: Self): str }
    // impl Show for a[] where a: Show { fun show(elems): str { ... } }
    // impl Show for num { fun show(n): str { ... } }
    // [1, 2, 3].show()
    // instantiateTyVar(a Show, num[]) -> num: Show 
    // propagateTraits(traits: [Show], ty: num[])
    // propagateTraitTyContext(trait: Show, ty: num[])
    // findTraitImplContext(trait: Show, ty: num[])
    // propagateTraits(traits: [Show], ty: num)
    // propagateTraitTyContext(trait: Show, ty: num)
    // findTraitImplContext(trait: Show, ty: num)

    func findTraitImplContext(trait: String, ty: Ty, level: UInt) throws -> [TyVar.Context]? {
        if let impls = traitImpls[trait] {
            for (implementee, context) in impls {
                let inst = try implementee.instantiate(level: level, env: self)
                let inst2 = try ty.instantiate(level: level, env: self)
                let subst = unifyPure(inst.ty, inst2.ty)
                if subst != nil {
                    return ty.subTypes().map({ subTy in
                        let subTyVars = subTy.unboundVars()
                        let subTyTraits = context
                            .filter({ subTyVars.contains($0.key) })
                            .values
                            .flatMap({ $0 })

                        return subTyTraits
                    }) 
                }
            }
        }

        return nil
    }

    func propagateTraits(traits: TyVar.Context, ty: Ty, level: UInt) throws {
        if case let .variable(v) = ty.deref(), case let .unbound(_, _, context) = v.ref {
            for trait in traits {
                if !context.ref.contains(trait) {
                    context.ref.append(trait)
                } 
            }
        } else {
            for trait in traits { 
                try propagateTraitTyContext(trait: trait, ty: ty, level: level)
            }
        }
    }

    func propagateTraitTyContext(trait: String, ty: Ty, level: UInt) throws {
        if let s = try findTraitImplContext(trait: trait, ty: ty, level: level) {
            for (k, subTy) in zip(s, ty.subTypes()) {
                try propagateTraits(traits: k, ty: subTy, level: level)
            }
        } else {
            throw TypeError.noTraitImplForType(trait: trait, ty: ty)
        }
    }

    func unifyPure(_ s: Ty, _ t: Ty) -> Subst? {
        do {
            let subst: Subst = Ref([:])
            try unify(s, t, subst: subst)
            return subst
        } catch {
            return nil
        }
    }
}

extension Ty {
    func generalize(level: UInt) -> Ty {
        switch self {
        case let .variable(v):
            switch v.ref {
            case let .unbound(id, lvl, traits) where lvl > level:
                return .variable(Ref(.generic(id, context: traits)))

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
    
    func instantiate(level: UInt, env: TypeEnv) throws -> (ty: Ty, subst: [TyVarId:Ty]) {
        var subst = [TyVarId:Ty]()
        
        func go(_ ty: Ty) throws -> Ty {
            switch ty {
            case let .const(name, args):
                return .const(name, try args.map(go))
            case let .variable(v):
                switch v.ref {
                case let .link(to):
                    return try go(to)
                case let .unbound(_, _, traits):
                    try env.propagateTraits(traits: traits.ref, ty: ty, level: level)
                    return ty
                case let .generic(id, traits):
                    if let inst = subst[id] {
                        try env.propagateTraits(traits: traits.ref, ty: inst, level: level)
                        return inst
                    } else {
                        let inst = Ty.fresh(level: level)
                        try env.propagateTraits(traits: traits.ref, ty: inst, level: level)
                        subst[id] = inst
                        return inst
                    }
                }
            case let .fun(args, ret):
                return .fun(try args.map(go), try go(ret))
            case let .record(row):
                return .record(try row.map(types: go))
            }
        }
        
        return (try go(self), subst)
    }
}

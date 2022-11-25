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

    public static func freshTyVarId() -> TyVarId {
        let id = nextTyVarId
        nextTyVarId += 1
        return id
    }
}

public class TypeEnv: CustomStringConvertible {
    let parent: TypeEnv?
    var vars = [String:Ty]()
    public typealias FunctionInfo = (returnTy: Ty, isIterator: Bool)
    static var functionInfoStack = [FunctionInfo]()
    
    init() {
        parent = nil
    }
    
    init(parent: TypeEnv? = nil) {
        self.parent = parent
    }
    
    public func lookup(_ name: String) -> Ty? {
        if let ty = vars[name] {
            return ty
        }
        
        return parent?.lookup(name)
    }
    
    public func contains(_ name: String) -> Bool {
        return lookup(name) != nil
    }
    
    public func declare(_ name: String, ty: Ty) throws {
        guard vars[name] == nil else {
            throw TypeError.cannotRedeclareVariable(name)
        }
        
        vars[name] = ty
    }
    
    public func child() -> TypeEnv {
        return .init(parent: self)
    }
    
    public static func pushFunctionInfo(retTy: Ty, isIterator: Bool) {
        TypeEnv.functionInfoStack.append((retTy, isIterator))
    }
    
    public static func popFunctionInfo() {
        _ = TypeEnv.functionInfoStack.popLast()
    }
    
    public static func peekFunctionInfo() -> FunctionInfo? {
        return TypeEnv.functionInfoStack.last
    }
    
    public var description: String {
        let vars = self.vars.map({ (v, ty) in "    \(v): \(ty.canonical)" })
        return "{\n\(vars.joined(separator: ",\n"))\n}"
    }
}

public enum TyVar: Equatable, CustomStringConvertible {
    case unbound(id: TyVarId, level: UInt)
    case link(Ty)
    case generic(TyVarId)

    public static func showTyVarId(_ id: TyVarId) -> String {
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

    public static func fresh(level: UInt) -> TyVar {
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

public class Ref<T: Equatable>: Equatable {
    var ref: T

    init(_ ref: T) {
        self.ref = ref
    }

    public static func == (lhs: Ref<T>, rhs: Ref<T>) -> Bool {
        return lhs.ref == rhs.ref
    }
}

public enum Row: Equatable {
    case empty
    case extend(field: String, ty: Ty, tail: Ty)
    
    public static func from(entries: [(String, Ty)], tail: Ty = .record(.empty)) -> Row {
        var row = Row.empty
        
        for (index, (field, ty)) in entries.reversed().enumerated() {
            let rest = index == 0 ? tail : .record(row)
            row = .extend(field: field, ty: ty, tail: rest)
        }
        
        
        return row
    }
    
    public func entries() -> [(String, Ty)] {
        var entries = [(String, Ty)]()
        
        func go(_ row: Row) {
            switch row {
            case .empty:
                break
            case let .extend(field, ty, tail):
                entries.append((field, ty))
                if case let .record(tailRow) = tail.deref() {
                    go(tailRow)
                }
            }
        }
        
        go(self)
        
        return entries
    }
    
    public func map(types f: (_ ty: Ty) -> Ty) -> Row {
        switch self {
        case .empty:
            return .empty
        case let .extend(field, ty, tail):
            if case .record(let row) = tail {
                return .extend(field: field, ty: f(ty), tail: .record(row.map(types: f)))
            }
            
            return .extend(field: field, ty: f(ty), tail: tail)
        }
    }
}

public indirect enum Ty: Equatable, CustomStringConvertible {
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
                    return "'" + canonicalized(id)
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
                return "\(go(args[0])) => \(go(ret))"
            case let .fun(args, ret):
                return "(\(args.map(go).joined(separator: ", "))) => \(go(ret))"
            case let .record(row):
                let entries = row.entries()
                if entries.isEmpty {
                    return "{}"
                } else {
                    let fields = row.entries().map({ (k, v) in "\(k): \(go(v))" }).joined(separator: ", ")
                    
                    return "{ \(fields) }"
                }
            }
        }
        
        return go(self)
    }

    public func deref() -> Ty {
        if case let .variable(tyVar) = self, case let .link(ty) = tyVar.ref {
            let res = ty.deref()
            tyVar.ref = .link(res)
            return res
        }

        return self
    }
    
    public static func == (s: Ty, t: Ty) -> Bool {
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
                assertionFailure()
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
            for (_, ty) in row.entries() {
                try go(ty)
            }
        }
    }
    
    try go(ty)
}

// see https://github.com/tomprimozic/type-systems
public func unify(_ s: Ty, _ t: Ty) throws {
    let startingEq = (s, t)
    var eqs = [(s, t)]
    
    while let (s, t) = eqs.popLast() {
        if s != t {
            switch (s, t) {
            case let (.const(f, args1), .const(g, args2)) where f == g && args1.count == args2.count:
                eqs.append(contentsOf: zip(args1, args2))
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

func rewriteRow(_ row2: Ty, field: String, ty: Ty) throws -> Ty {
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
            
            return .record(.extend(field: field2, ty: ty2, tail: try rewriteRow(tail2, field: field, ty: ty)))
        }
    case .variable(let v):
        switch v.ref {
        case let .unbound(_, level):
            let tail2 = Ty.fresh(level: level)
            let ty2 = Ty.record(.extend(field: field, ty: ty, tail: tail2))
            v.ref = .link(ty2)
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

extension Ty {
    public func generalize(level: UInt) -> Ty {
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
    
    public func instantiate(level: UInt) -> Ty {
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

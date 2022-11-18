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

public class Env: CustomStringConvertible {
    let parent: Env?
    var vars = [String:Ty]()
    static var functionReturnTyStack = [Ty]()
    
    init() {
        parent = nil
    }
    
    init(parent: Env) {
        self.parent = parent
    }
    
    public func lookup(varName: String) -> Ty? {
        if let ty = vars[varName] {
            return ty
        }
        
        return parent?.lookup(varName: varName)
    }
    
    public func declare(varName: String, ty: Ty) {
        vars[varName] = ty
    }
    
    public func child() -> Env {
        return Env.init(parent: self)
    }
    
    public static func pushFunc(retTy: Ty) {
        Env.functionReturnTyStack.append(retTy)
    }
    
    public static func popFunc() {
        _ = Env.functionReturnTyStack.popLast()
    }
    
    public static func funcReturnTy() -> Ty? {
        return Env.functionReturnTyStack.last
    }
    
    public var description: String {
        let vars = self.vars.map({ (v, ty) in "    \(v): \(ty)" })
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

public indirect enum Ty: Equatable, CustomStringConvertible {
    case variable(Ref<TyVar>)
    case const(String, [Ty])
    case fun([Ty], Ty)

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
        return .const("tuple", elems)
    }
    
    public static func iterator(_ ty: Ty) -> Ty {
        return .const("iter", [ty])
    }
    
    public static func freshVar(level l: UInt) -> Ty {
        return .variable(Ref(TyVar.fresh(level: l)))
    }

    public var description: String {
        switch self {
        case let .variable(tyVar):
            return "\(tyVar.ref)"
        case let .const("tuple", elems):
            return "(\(elems.map({ "\($0)" }).joined(separator: ", ")))"
        case let .const(name, args) where args.isEmpty:
            return "\(name)"
        case let .const(name, args):
            return "\(name)<\(args.map({ "\($0)" }).joined(separator: ", "))>"
        case let .fun(args, ret) where args.count == 1:
            return "\(args[0]) -> \(ret)"
        case let .fun(args, ret):
            return "(\(args.map({ "\($0)" }).joined(separator: ", "))) -> \(ret)"
        }
    }

    public func deref() -> Ty {
        if case let .variable(tyVar) = self, case let .link(ty) = tyVar.ref {
            let res = ty.deref()
            tyVar.ref = .link(res)
            return res
        }

        return self
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
                    throw TypeError.recursiveType(id, ty)
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
        }
    }
    
    try go(ty)
}

// see https://github.com/tomprimozic/type-systems
public func unify(_ s: Ty, _ t: Ty) throws {
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
                    throw TypeError.cannotUnify(s, t)
                }
            default:
                throw TypeError.cannotUnify(s, t)
            }
        }
    }
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
                        let inst = Ty.freshVar(level: level)
                        idVarMap[id] = inst
                        return inst
                    }
                }
            case let .fun(args, ret):
                return .fun(args.map(go), go(ret))
            }
        }
        
        return go(self)
    }
}

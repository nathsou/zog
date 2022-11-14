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

public enum TyVar: Equatable, CustomStringConvertible {
    case unbound(id: TyVarId, level: UInt)
    case link(Ty)
    
    public var description: String {
        switch self {
        case let .unbound(id, _):
            return "var(\(id)"
        case let .link(ty):
            return ty.description
        }
    }
    
    public static func fresh(level: UInt) -> TyVar {
        return .unbound(id: TyContext.freshTyVarId(), level: level)
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
    
    public static func ~= (lhs: T, rhs: Ref<T>) -> Bool {
        return rhs.ref ~= lhs
    }
    
    public static func ~= (lhs: Ref<T>, rhs: T) -> Bool {
        return lhs.ref ~= rhs
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
    
    public func tuple(_ elems: [Ty]) -> Ty {
        return .const("tuple", elems)
    }
    
    public var description: String {
        switch self {
        case let .variable(tyVar):
            return "\(tyVar.ref)"
        case let .const(name, args):
            return "\(name)<\(args.map({ "\($0)" }).joined(separator: ", "))>"
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

public func unify(_ s: Ty, _ t: Ty) -> TypeError? {
    var eqs = [((s, t))]
    while let (s, t) = eqs.popLast() {
        switch (s.deref(), t.deref()) {
        case let (.const(f, args1), .const(g, args2)) where f == g && args1.count == args2.count:
            eqs.append(contentsOf: zip(args1, args2))
        case let (.fun(args1, ret1), .fun(args2, ret2)) where args1.count == args2.count:
            eqs.append(contentsOf: zip(args1, args2))
            eqs.append((ret1, ret2))
        case let (.variable(v), _):
            switch v.ref {
            case .unbound(_, _):
                // TODO: occurs check
                v.ref = .link(t)
            case .link(let to):
                eqs.append((to, t))
            }
        case (_, .variable(_)):
            eqs.append((t, s))
        default:
            return TypeError.cannotUnify(s, t)
        }
    }
    
    return .none
}

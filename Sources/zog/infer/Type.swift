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

func occursCheckAdjustLevels(id: UInt, level: UInt, ty: Ty) -> TypeError? {
    func go(_ ty: Ty) throws {
        switch ty {
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
    
    do {
        try go(ty)
    } catch let error as TypeError {
        return error
    } catch {}
    
    return nil
}

// see https://github.com/tomprimozic/type-systems
public func unify(_ s: Ty, _ t: Ty) -> TypeError? {
    var eqs = [(s, t)]
    while let (s, t) = eqs.popLast() {
        switch (s, t) {
        case let (.const(f, args1), .const(g, args2)) where f == g && args1.count == args2.count:
            eqs.append(contentsOf: zip(args1, args2))
        case let (.fun(args1, ret1), .fun(args2, ret2)) where args1.count == args2.count:
            eqs.append(contentsOf: zip(args1, args2))
            eqs.append((ret1, ret2))
        case let (.variable(v), ty), let (ty, .variable(v)):
            switch v.ref {
            case let .unbound(id, lvl):
                if let err = occursCheckAdjustLevels(id: id, level: lvl, ty: ty) {
                    return err
                }
                
                v.ref = .link(ty)
            case let .link(to):
                eqs.append((to, ty))
            }
        default:
            return TypeError.cannotUnify(s, t)
        }
    }

    return nil
}

//
//  TypeError.swift
//
//
//  Created by Nathan on 14/11/2022.
//

import Foundation

public enum TypeError: Error, CustomStringConvertible, Equatable {
    case cannotUnify(Ty, Ty)
    case recursiveType(TyVarId, Ty)

    public var description: String {
        switch self {
        case let .cannotUnify(s, t):
            return "Cannot unify \(s) with \(t)"
        case let .recursiveType(id, ty):
            return "Recursive type: \(TyVar.showTyVarId(id)) occurs in \(ty)"
        }
    }
}

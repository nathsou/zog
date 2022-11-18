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
    case unknownVariable(String)
    case cannotReturnOutsideFunctionBody
    case cannotYieldOutsideFunctionBody

    public var description: String {
        switch self {
        case let .cannotUnify(s, t):
            return "Cannot unify \(s) with \(t)"
        case let .recursiveType(id, ty):
            return "Recursive type: \(TyVar.showTyVarId(id)) occurs in \(ty)"
        case let .unknownVariable(v):
            return "Unknown variable \"\(v)\""
        case .cannotReturnOutsideFunctionBody:
            return "Cannot use return outside a function body"
        case .cannotYieldOutsideFunctionBody:
            return "Cannot use yield outside a function body"
        }
    }
}

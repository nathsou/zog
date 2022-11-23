//
//  TypeError.swift
//
//
//  Created by Nathan on 14/11/2022.
//

import Foundation

public enum TypeError: Error, CustomStringConvertible {
    case cannotUnify((Ty, Ty), failedWith: (Ty, Ty)?)
    case recursiveType(Ty, Ty)
    case unknownVariable(String)
    case cannotReturnOutsideFunctionBody
    case cannotYieldOutsideIteratorBody
    case cannotRedeclareVariable(String)
    case recordDoesNotContainField(Ty, String)
    case expectedRecordType(Ty)

    public var description: String {
        switch self {
        case let .cannotUnify((originalS, originalT), failedWith):
            if let failedWith {
                let (s, t) = failedWith
                if !(s == originalS || s == originalT || t == originalS || t == originalT) {
                    return "Type '\(originalS)' is incompatible with '\(originalT)' since '\(s)' cannot be unified with '\(t)'"
                }
            }
            
            return "Type '\(originalS)' is incompatible with '\(originalT)'"
        case let .recursiveType(s, t):
            return "Recursive type: \(s) occurs in \(t)"
        case let .unknownVariable(v):
            return "Unknown variable \"\(v)\""
        case .cannotReturnOutsideFunctionBody:
            return "'return' can only be used inside a function body"
        case .cannotYieldOutsideIteratorBody:
            return "'yield' can only be used inside an iterator declaration"
        case let .cannotRedeclareVariable(name):
            return "Cannot redeclare '\(name)'"
        case let .recordDoesNotContainField(record, field):
            return "record type '\(record)' does not contain field '\(field)'"
        case let .expectedRecordType(ty):
            return "Expected record type, got '\(ty)'"
        }
    }
}

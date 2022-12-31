//
//  TypeError.swift
//
//
//  Created by Nathan on 14/11/2022.
//

import Foundation

enum TypeError: Error, CustomStringConvertible {
    case cannotUnify((Ty, Ty), failedWith: (Ty, Ty)?)
    case recursiveType(Ty, Ty)
    case unknownVariable(String)
    case cannotReturnOutsideFunctionBody
    case cannotYieldOutsideIteratorBody
    case cannotRedeclareVariable(String)
    case recordDoesNotContainField(Ty, String)
    case enumDoesNotContainVariant(Ty, String)
    case expectedRecordType(Ty)
    case patternDestructuringCanFail(CorePattern)
    case couldNotResolveType(Ty)
    case noEnumMatchesVariants(variants: [String])
    case ambiguousEnumForVariants(variants: [String], candidates: [String])
    case invalidBuiltInCall(String)
    case wrongNumberOfArguments(name: String, expected: Int, got: Int)
    case extraneousVariantArgument(enumName: String, variant: String)
    case missingVariantArgument(enumName: String, variant: String)
    case couldNotResolveMember(modulePath: String, member: String)
    case unknownTrait(String)
    case missingTraitImplMethods(trait: String, methods: [String])
    case extraneousTraitImplMethods(trait: String, methods: [String])
    case invalidTraitImplMethodSignature(trait: String, method: String, error: Error)
    case unknownMethodForType(String, Ty)

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
        case let .enumDoesNotContainVariant(enum_, variant):
            return "enum type '\(enum_)' does not contain variant '\(variant)'"
        case let .expectedRecordType(ty):
            return "Expected record type, got '\(ty)'"
        case let .patternDestructuringCanFail(pattern):
            return "Cannot use '\(pattern)' as a destructuring pattern since it can fail"
        case let .couldNotResolveType(ty):
            return "Could not resolve type '\(ty)'"
        case let .noEnumMatchesVariants(variants):
            return "No enum matches variants: '\(variants.joined(separator: ", "))'"
        case let .ambiguousEnumForVariants(variants, candidates):
            return "Variants [\(variants.joined(separator: ", "))] are present in multiple enums: [\(candidates.joined(separator: ", "))]"
        case let .invalidBuiltInCall(name):
            return "@\(name) is not a built in function"
        case let .wrongNumberOfArguments(name, expected, got):
            return "\(name) expects \(expected) argument(s), received \(got)"
        case let .extraneousVariantArgument(enumName, variant):
            return "Extraneous argument in \(enumName).\(variant) variant constructor"
        case let .missingVariantArgument(enumName, variant):
            return "Missing argument in \(enumName).\(variant) variant constructor"
        case let .couldNotResolveMember(modulePath, member):
            return "Could not resolve member '\(member)' from module '\(modulePath)'"
        case let .unknownTrait(trait):
            return "Unknown trait '\(trait)'"
        case let .missingTraitImplMethods(trait, methods):
            return "Implementation of trait '\(trait)' is missing methods: \(methods.joined(separator: ", "))"
        case let .extraneousTraitImplMethods(trait, methods):
            return "Implementation of trait '\(trait)' has extraneous methods: \(methods.joined(separator: ", "))"
        case let .invalidTraitImplMethodSignature(trait, method, error):
            return "Implementation of trait '\(trait)' has an invalid signature for method '\(method)':\n\t\(error)"
        case let .unknownMethodForType(method, ty):
            return "No implementation of method '\(method)' found for type '\(ty)'"
        }
    }
}

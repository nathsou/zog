//
//  ParserError.swift
//
//
//  Created by Nathan on 13/11/2022.
//

import Foundation

enum ParserError: Error, CustomStringConvertible, Equatable {
    case unexepectedToken(Token)
    case expectedExpression
    case expectedStatement
    case expectedDeclaration
    case expectedIdentifier
    case expectedUppercaseIdentifier
    case expectedLowercaseIdentifier
    case expectedType
    case expectedPattern
    case invalidAssignmentTarget
    case expected(Token)
    case expectedPathInImportDecl
    case invalidFunctionSignatureInTrait
    case illegalDeclinImpl(Decl.Kind)
    case expectedTypeAnnotationInTraitMethodSig

    public var description: String {
        switch self {
        case .unexepectedToken(let t):
            return "Unexpected token '\(t)'"
        case .expected(let t):
            return "Expected '\(t)'"
        case .expectedExpression:
            return "Expected an expression"
        case .expectedIdentifier:
            return "Expected an identifier"
        case .expectedUppercaseIdentifier:
            return "Expected an identifier starting with an uppercase letter"
        case .expectedLowercaseIdentifier:
            return "Expected an identifier starting with a lowercase letter"
        case .expectedType:
            return "Expected a type"
        case .invalidAssignmentTarget:
            return "Invalid assignment target"
        case .expectedStatement:
            return "Expected a statement"
        case .expectedPattern:
            return "Expected a pattern"
        case .expectedDeclaration:
            return "Expected a declaration"
        case .expectedPathInImportDecl:
            return "Expected a path in import decl"
        case .invalidFunctionSignatureInTrait:
            return "Invalid function signature in trait"
        case .illegalDeclinImpl(let kind):
            return "Illegal declaration in impl block: \(kind)"
        case .expectedTypeAnnotationInTraitMethodSig:
            return "Type annotations are mendatory in trait method signature"
        }
    }
}

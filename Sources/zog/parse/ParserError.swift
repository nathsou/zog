//
//  ParserError.swift
//
//
//  Created by Nathan on 13/11/2022.
//

import Foundation

public enum ParserError: Error, CustomStringConvertible, Equatable {
  case unexepectedToken(Token)
  case expectedExpression
  case expectedStatement
  case expectedIdentifier
  case invalidAssignmentTarget
  case expected(Token)

  public var description: String {
    switch self {
    case .unexepectedToken(let t):
      return "Unexpected token '\(t)'"
    case .expected(let t):
      return "Expected '\(t)'"
    case .expectedExpression:
      return "Expected expression"
    case .expectedIdentifier:
      return "Expected identifier"
    case .invalidAssignmentTarget:
      return "Invalid assignment target"
    case .expectedStatement:
      return "Expected statement"
    }
  }
}

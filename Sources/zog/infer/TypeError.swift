//
//  TypeError.swift
//
//
//  Created by Nathan on 14/11/2022.
//

import Foundation

public enum TypeError: Error, CustomStringConvertible, Equatable {
  case cannotUnify(Ty, Ty)

  public var description: String {
    switch self {
    case let .cannotUnify(s, t):
      return "Cannot unify \(s) with \(t)"
    }
  }
}

//
//  Token.swift
//  
//
//  Created by Nathan on 12/11/2022.
//

import Foundation

public enum Symbol: CustomStringConvertible {
    case lparen, rparen, lbracket, rbracket, lcurlybracket, rcurlybracket, comma, semicolon, colon, eq,
         arrow, plus, minus, star, slash, percent, eqeq, bangeq, lss, leq, gtr, geq, bang, dot, dotdot,
         dotdotdot, dotdoteq, pipe

    public var description: String { 
        switch self {
            case .lparen:
                return "("
            case .rparen:
                return ")"
            case .lbracket:
                return "["
            case .rbracket:
                return "]"
            case .lcurlybracket:
                return "{"
            case .rcurlybracket:
                return "}"
            case .comma:
                return ","
            case .semicolon:
                return ";"
            case .colon:
                return ":"
            case .eq:
                return "="
            case .arrow:
                return "->"
            case .plus:
                return "+"
            case .minus:
                return "-"
            case .star:
                return "*"
            case .slash:
                return "/"
            case .percent:
                return "%"
            case .eqeq:
                return "=="
            case .bangeq:
                return "!="
            case .lss:
                return "<"
            case .leq:
                return "<="
            case .gtr:
                return ">"
            case .geq:
                return ">="
            case .bang:
                return "!"
            case .dot:
                return "."
            case .dotdot:
                return ".."
            case .dotdotdot:
                return "..."
            case .dotdoteq:
                return "..="
            case .pipe:
                return "|"
        }
    }
}

public enum Keyword: String {
    case Let, Mut, If, Else, For, In, Return, Yield, Break, While, Check, And, Or, True, False

    public static func parse(_ str: String) -> Keyword? {
        switch str {
        case "let":
            return .Let
        case "mut":
            return .Mut
        case "if":
            return .If
        case "else":
            return .Else
        case "for":
            return .For
        case "in":
            return .In
        case "return":
            return .Return
        case "yield":
            return .Yield
        case "break":
            return .Break
        case "while":
            return .While
        case "check":
            return .Check
        case "and":
            return .And
        case "or":
            return .Or
        case "true":
            return .True
        case "false":
            return .False
        default:
            return .none
        }
    }
}

public enum Token: Equatable, CustomStringConvertible {
    case num(Float64)
    case bool(Bool)
    case str(String)
    case symbol(Symbol)
    case identifier(String)
    case keyword(Keyword)
    indirect case error(ParserError)
    
    public var description: String {
        switch self {
        case .num(let x):
            return Literal.num(x).description
        case .bool(let q):
            return q.description
        case .str(let s):
            return "\"\(s)\""
        case .symbol(let s):
            return s.description
        case .identifier(let id):
            return id
        case .keyword(let kw):
            return "\(kw)"
        case .error(let error):
            return error.description
        }
    }
}

public struct TokenWithPos {
    let token: Token
    let start: Int
    let end: Int
    
    public var description: String {
        return "\(self.token): (start: \(self.start), end: \(self.end)"
    }
}

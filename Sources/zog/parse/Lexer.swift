//
//  Lexer.swift
//
//
//  Created by Nathan on 12/11/2022.
//

import Foundation

extension Character {
    var isDigit: Bool {
        return self >= "0" && self <= "9"
    }

    var isAlphaNum: Bool {
        return self.isLetter || self.isDigit
    }
}

struct Lexer {
    var index: Int
    var startIndex: Int
    let chars: [Character]
    var tokens: [TokenWithPos]

    init(source: String) {
        index = 0
        startIndex = 0
        chars = Array(source) + ["\n"]
        tokens = []
    }

    mutating func push(_ token: Token) {
        tokens.append(TokenWithPos(token: token, start: startIndex, end: index))
    }

    func peek() -> Character? {
        guard index >= 0, index < chars.count else {
            return nil
        }

        return chars[index]
    }

    func shouldInsertSemicolon() -> Bool {
        switch tokens.last?.token {
        case .num(_): return true
        case .bool(_): return true
        case .identifier(_): return true
        case .str(_): return true
        case .symbol(.rparen): return true
        case .symbol(.rbracket): return true
        case .symbol(.rcurlybracket): return true
        case .symbol(.gtr): return true
        case .keyword(.Return): return true
        case .keyword(.Break): return true
        case .keyword(.Yield): return true
        case .keyword(.True): return true
        case .keyword(.False): return true
        case .raw(_): return true
        default: return false
        }
    }

    mutating func advance() -> Character? {
        if let c = peek() {
            if c.isNewline && shouldInsertSemicolon() {
                push(.symbol(.semicolon))
            }

            index += 1

            return c
        }

        return nil
    }

    mutating func match(_ char: Character) -> Bool {
        if let c = peek(), c == char {
            _ = advance()
            return true
        }

        return false
    }

    mutating func parseNum() -> Token {
        while let c = peek(), c.isDigit {
            _ = advance()
        }

        if match(".") {
            while let c = peek(), c.isDigit {
                _ = advance()
            }
        }

        return .num(Float64(String(chars[startIndex...(index - 1)]))!)
    }

    mutating func parseIdentOrKeyword() -> Token {
        while let c = peek(), c.isAlphaNum || c == "_" {
            _ = advance()
        }

        let lexeme = String(chars[startIndex...(index - 1)])

        if let kw = Keyword.parse(lexeme) {
            return .keyword(kw)
        } else {
            return .identifier(lexeme)
        }
    }

    
    mutating func parseBacktickIdentifier() -> Token {
        var chars = [Character]()

        while let c = peek() {
            _ = advance()

            if c == "`" {
                break
            }

            chars.append(c)
        }

        return .identifier(String(chars))
    }

    mutating func parseString() -> Token {
        var chars = [Character]()

        while let c = peek() {
            _ = advance()

            if c == "\\" {
                if let nextChar = advance() {
                    switch nextChar {
                    case "n": chars.append("\n")
                    case "t": chars.append("\t")
                    case "r": chars.append("\r")
                    case "\\": chars.append("\\")
                    case "\"": chars.append("\"")
                    default:
                        chars.append(nextChar)
                    }
                }
            } else if c == "\"" {
                break
            } else {
                chars.append(c)
            }
        }

        return .str(String(chars))
    }
    
    mutating func parseRawBlock() -> Token {
        // remove 'raw' token
        _ = tokens.popLast()
        var chars = [Character]()
        var scopeLevel = 1
        
        while let c = peek() {
            _ = advance()
            
            if c == "}" {
                scopeLevel -= 1
                if scopeLevel == 0 {
                    break
                }
            } else if c == "{" {
                scopeLevel += 1
            }
            
            chars.append(c)
        }
        
        return .raw(String(chars).trimmingCharacters(in: [" "]))
    }

    mutating func skipSpaces() {
        while let c = peek(), c.isWhitespace {
            _ = advance()
        }
    }

    mutating func skipLine() {
        while let c = peek(), !c.isNewline {
            _ = advance()
        }
    }

    mutating func next() -> Token? {
        if index == chars.count {
            return nil
        }

        skipSpaces()
        startIndex = index

        if let c = advance() {
            switch c {
            case "(": return .symbol(.lparen)
            case ")": return .symbol(.rparen)
            case "[": return .symbol(.lbracket)
            case "]": return .symbol(.rbracket)
            case "{":
                if tokens.last?.token == .identifier("raw") {
                    return parseRawBlock()
                } else {
                    return .symbol(.lcurlybracket)
                }
            case "}": return .symbol(.rcurlybracket)
            case ",": return .symbol(.comma)
            case ";": return .symbol(.semicolon)
            case ":": return .symbol(.colon)
            case "|": return .symbol(.pipe)
            case "_": return .symbol(.underscore)
            case "'": return .symbol(.singlequote)
            case "@": return .symbol(.at)
            case "=": return .symbol(match(">") ? .thickArrow : match("=") ? .eqeq : .eq)
            case "!": return .symbol(match("=") ? .bangeq : .bang)
            case "<": return .symbol(match("=") ? .leq : .lss)
            case ">": return .symbol(match("=") ? .geq : .gtr)
            case "-":
                if match(">") {
                    // enable pipeline operator chaining
                    if case .symbol(.semicolon) = tokens.last?.token {
                        tokens.removeLast()
                    }
                    
                    return .symbol(.thinArrow)
                } else if match("=") {
                    return .symbol(.minuseq)
                } else {
                    return .symbol(.minus)
                }
            case ".": return .symbol(match(".") ? match(".") ? .dotdotdot : .dotdot : .dot)
            case "+": return .symbol(match("=") ? .pluseq : match("+") ? .plusplus : .plus)
            case "*": return .symbol(match("=") ? .stareq : .star)
            case "%": return .symbol(.percent)
            case "`": return parseBacktickIdentifier()
            case "\"": return parseString()
            case "/":
                if match("/") {
                    skipLine()
                    return next()
                }

                return .symbol(match("=") ? .slasheq : .slash)
            default:
                if c.isDigit {
                    return parseNum()
                }

                if c.isLetter {
                    return parseIdentOrKeyword()
                }

                return nil
            }
        }

        return nil
    }

    mutating func lex() -> [TokenWithPos] {
        while let token = next() {
            push(token)
        }

        return tokens
    }
}

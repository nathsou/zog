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

public struct Lexer {
    var index: Int
    var startIndex: Int
    let chars: [Character]
    var tokens: [TokenWithPos]
    
    init(source: String) {
        index = 0
        startIndex = 0
        chars = Array(source)
        tokens = []
    }
    
    mutating func push(_ token: Token) {
        tokens.append(TokenWithPos(token: token, start: startIndex, end: index))
    }
    
    func peek() -> Character? {
        guard index >= 0, index < chars.count else {
            return .none
        }

        return chars[index]
    }
    
    func shouldInsertSemicolon() -> Bool {
        if tokens.isEmpty {
            return false
        }
        
        switch tokens.last?.token {
            case .num(_):
                return true
            case .bool(_):
                return true
            case .identifier(_):
                return true
            case .symbol(.rparen):
                return true
            case .symbol(.rbracket):
                return true
            case .symbol(.rcurlybracket):
                return true
            case .keyword(.Return):
                return true
            default:
                return false
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
        
        return .none
    }

    mutating func match(_ char: Character) -> Bool {
        if let c = peek() {
            if c == char {
                _ = advance()
                return true
            }
        }

        return false
    }

    mutating func parseNum() -> Token {
        while let c = peek() {
            if c.isDigit {
                _ = advance()
            } else {
                break
            }
        }

        if match(".") {
            while let c = peek() {
                if c.isDigit {
                    _ = advance()
                } else {
                    break
                }
            }
        }

        return .num(Float64(String(chars[startIndex...(index - 1)]))!)
    }

    mutating func parseIdentOrKeyword() -> Token {
        while let c = peek() {
            if c.isAlphaNum {
                _ = advance()
            } else {
                break
            }
        }

        let lexeme = String(chars[startIndex...(index - 1)])
        
        if let kw = Keyword.parse(lexeme) {
            return .keyword(kw)
        } else {
            return .identifier(lexeme)
        }
    }

    mutating func parseString() -> Token {
        var chars: [Character] = []

        while let c = peek() {
            _ = advance()

            if c == "\\" {
                if let nextChar = advance() {
                    switch nextChar {
                        case "n":
                            chars.append("\n")
                        case "t":
                            chars.append("\t")
                        case "r":
                            chars.append("\r")
                        case "\\":
                            chars.append("\\")
                        case "\"":
                            chars.append("\"")
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

    mutating func skipSpaces() {
        while let c = peek() {
            if c.isWhitespace {
                _ = advance()
            } else {
                break
            }
        }

        startIndex = index
    }

    mutating func skipLine() {
        while let c = peek() {
            if !c.isNewline {
                _ = advance()
            } else {
                break
            }
        }
    }
    
    mutating func next() -> Token? {
        if index == chars.count {
            return .none
        }

        skipSpaces()
        
        if let c = advance() {
            switch c {
                case "(":
                    return .symbol(.lparen)
                case ")":
                    return .symbol(.rparen)
                case "[":
                    return .symbol(.lbracket)
                case "]":
                    return .symbol(.rbracket)
                case "{":
                    return .symbol(.lcurlybracket)
                case "}":
                    return .symbol(.rcurlybracket)
                case ",":
                    return .symbol(.comma)
                case ";":
                    return .symbol(.semicolon)
                case ":":
                    return .symbol(.colon)
                case "|":
                    return .symbol(.pipe)
                case "=":
                    return .symbol(match("=") ? .eqeq : .eq)
                case "!":
                    return .symbol(match("=") ? .bangeq : .bang)
                case "<":
                    return .symbol(match("=") ? .leq : .lss)
                case ">":
                    return .symbol(match("=") ? .geq : .gtr)
                case "-":
                    return .symbol(match(">") ? .arrow : .minus)
                case ".":
                    switch peek() {
                        case ".":
                            return .symbol(match(".") ? .dotdotdot : .dotdot)
                        default:
                            return .symbol(.dot)
                    }
                case "+":
                    return .symbol(.plus)
                case "*":
                    return .symbol(.star)
                case "/":
                    if match("/") {
                        return next()
                    } else {
                        return .symbol(.slash)
                    }
                case "%":
                    return .symbol(.percent)
                case "\"":
                    return parseString()
                default:
                    if c.isDigit {
                        return parseNum()
                    }

                    if c.isLetter {
                        return parseIdentOrKeyword()
                    }

                return .none
            }
        }

        return .none
    }

    public mutating func lex() -> [TokenWithPos] {
        while let token = next() {
            push(token)
        }

        return tokens
    }
}

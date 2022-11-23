//
//  Parser.swift
//
//
//  Created by Nathan on 13/11/2022.
//

import Foundation

public class Parser {
    let tokens: [TokenWithPos]
    var index: Int
    var statementStartIndex: Int
    var errors: [(ParserError, start: Int, end: Int)]

    init(tokens: [TokenWithPos]) {
        self.tokens = tokens
        errors = []
        index = 0
        statementStartIndex = 0
    }
    
    // ------ meta ------

    func peek(_ n: Int = 0) -> Token? {
        guard index < tokens.count else {
            return nil
        }

        return tokens[index + n].token
    }

    func previous() -> Token {
        return tokens[index - 1].token
    }

    func check(_ token: Token, lookahead: Int = 0) -> Bool {
        if let t = peek(lookahead) {
            return t == token
        }

        return false
    }

    func match(_ tokens: Token...) -> Bool {
        for token in tokens {
            if check(token) {
                advance()
                return true
            }
        }

        return false
    }

    func advance() {
        if index < tokens.count {
            index += 1
        }
    }

    func consume(_ token: Token, error: ParserError) throws {
        if check(token) {
            advance()
            return
        }

        throw error
    }

    func consume(_ token: Token) throws {
        try consume(token, error: .expected(token))
    }

    func identifier() throws -> String {
        if case .identifier(let n) = peek() {
            advance()
            return n
        }

        throw ParserError.expectedIdentifier
    }

    func attempt<T>(_ f: () throws -> T?) -> T? {
        let initialIndex = index
        let initialStatementIndex = statementStartIndex

        do {
            return try f()
        } catch {
            // backtrack
            index = initialIndex
            statementStartIndex = initialStatementIndex
            return nil
        }
    }
    
    // sepBy(rule, sep) -> (<rule> (sep <rule>)*)?
    func sepBy<T>(_ rule: () throws -> T, separator: Token) throws -> [T] {
        var terms = [T]()
        
        if !check(.symbol(.rparen)) {
            repeat {
                terms.append(try rule())
            } while match(separator)
        }
        
        return terms
    }
    
    // commas(rule) -> (<rule> (',' <rule>)*)?
    func commas<T>(_ rule: () throws -> T) throws -> [T] {
        return try sepBy(rule, separator: .symbol(.comma))
    }
    
    func typeAnnotation() throws -> Ty? {
        if match(.symbol(.colon)) {
            return try type()
        }
        
        return nil
    }

    func synchronize() {
        advance()

        while let token = peek() {
            if case .symbol(.semicolon) = previous() {
                return
            }

            switch token {
            case .keyword(.Let): return
            case .keyword(.Mut): return
            case .keyword(.While): return
            case .keyword(.For): return
            case .keyword(.Return): return
            case .keyword(.Yield): return
            case .keyword(.Break): return
            default: advance()
            }
        }
    }

    public func parse() -> [Stmt] {
        program()
    }

    // prog -> stmt*
    func program() -> [Stmt] {
        var stmts = [Stmt]()

        while let _ = peek() {
            stmts.append(statement())
        }

        return stmts
    }
    
    // ------ statements ------

    func statement() -> Stmt {
        statementStartIndex = index

        do {
            return try statementThrowing()
        } catch let error as ParserError {
            let start = tokens[statementStartIndex].start
            let end = tokens[index < tokens.count ? index : tokens.count - 1].end
            errors.append((error, start: start, end: end))
            synchronize()
            return .Error(error, span: (start, end))
        } catch {
            return .Error(.expectedStatement, span: (statementStartIndex, index))
        }
    }

    // stmt -> letStmt | 'return' expr? | 'break' | exprStmt
    func statementThrowing() throws -> Stmt {
        switch peek() {
        case .keyword(.Let):
            advance()
            return try letStmt(isMut: false)
        case .keyword(.Mut):
            advance()
            return try letStmt(isMut: true)
        case .keyword(.While):
            advance()
            return try whileStmt()
        case .keyword(.For):
            advance()
            return try forStmt()
        case .keyword(.If):
            advance()
            return try ifStmt()
        case .keyword(.Return):
            advance()
            let expr = attempt(expression)
            try consume(.symbol(.semicolon))

            return .Return(expr)
        case .keyword(.Yield):
            advance()
            let expr = try expression()
            try consume(.symbol(.semicolon))

            return .Yield(expr)
        case .keyword(.Break):
            advance()
            try consume(.symbol(.semicolon))
            return .Break
        default:
            return try exprStmt()
        }
    }

    // letStmt -> ('let' | 'mut') identifier '=' expr ';'
    func letStmt(isMut: Bool) throws -> Stmt {
        let name = try identifier()
        let ty = try typeAnnotation()
        try consume(.symbol(.eq))
        let val = try expression()
        try consume(.symbol(.semicolon))

        return .Let(mut: isMut, name: name, ty: ty, val: val)
    }

    // while -> 'while' expr '{' stmt* '}'
    func whileStmt() throws -> Stmt {
        let cond = try expression()
        let body = try statementListBlock()
        try consume(.symbol(.semicolon))

        return .While(cond: cond, body: body)
    }

    // for -> 'for' identifier 'in' expr '{' stmt* '}'
    func forStmt() throws -> Stmt {
        let name = try identifier()
        try consume(.keyword(.In))
        let iterator = try expression()
        let body = try statementListBlock()
        try consume(.symbol(.semicolon))

        return .For(name: name, iterator: iterator, body: body)
    }
    
    // ifStmt -> 'if' expr '{' stmt* '}'
    func ifStmt() throws -> Stmt {
        let cond = try expression()
        let body = try statementListBlock()
        try consume(.symbol(.semicolon))

        return .IfThen(cond: cond, then: body)
    }

    // stmtListBlock -> '{' stmt* '}'
    func statementListBlock() throws -> [Stmt] {
        try consume(.symbol(.lcurlybracket))
        var stmts = [Stmt]()

        while let stmt = attempt(statementThrowing) {
            stmts.append(stmt)
        }

        try consume(.symbol(.rcurlybracket))
        return stmts
    }

    // exprStmt -> expr ';'
    func exprStmt() throws -> Stmt {
        let expr = try expression()
        try consume(.symbol(.semicolon))

        return .Expr(expr)
    }
    
    // ------ expressions ------

    // expr -> useIn
    func expression() throws -> Expr {
        return try useIn()
    }
    
    // useIn -> 'use' ident '=' expr 'in' expr | if
    func useIn() throws -> Expr {
        if match(.identifier("use")) {
            let name = try identifier()
            let ty = try typeAnnotation()
            try consume(.symbol(.eq))
            let val = try expression()
            try consume(.keyword(.In))
            let rhs = try expression()
            
            return Expr.UseIn(name: name, ty: ty, val: val, rhs: rhs)
        }
        
        return try ifExpr()
    }

    // if -> 'if' expr expr 'else' expr | assignment
    func ifExpr() throws -> Expr {
        if match(.keyword(.If)) {
            let cond = try expression()
            let thenExpr = try expression()
            try consume(.keyword(.Else))
            let elseExpr = try expression()

            return .If(cond: cond, thenExpr: thenExpr, elseExpr: elseExpr)
        }

        return try assignment()
    }

    // assignment -> fun ('=' | '+=' | '-=' | '*=' | '/=') assignment | fun
    func assignment() throws -> Expr {
        let lhs = try fun()

        if match(
            .symbol(.eq), .symbol(.pluseq), .symbol(.minuseq), .symbol(.stareq), .symbol(.slasheq))
        {
            let isValidTarget: Bool

            switch lhs {
            case .Var(_):
                isValidTarget = true
            default:
                isValidTarget = false
            }

            if !isValidTarget {
                throw ParserError.invalidAssignmentTarget
            }

            let op: AssignmentOperator

            switch previous() {
            case .symbol(.pluseq):
                op = .plusEq
            case .symbol(.minuseq):
                op = .minusEq
            case .symbol(.stareq):
                op = .timesEq
            case .symbol(.slasheq):
                op = .divideEq
            default:
                op = .eq
            }

            let rhs = try assignment()

            return .Assignment(lhs, op, rhs)
        }

        return lhs
    }

    // fun -> 'iterator'? ('(' (identifier (',' identifier)*)?)? ')' | identifier) '=>' expr | logicalOr
    func fun() throws -> Expr {
        if let f: Expr = attempt({
            var args = [String]()
            let isIterator = match(.keyword(.Iterator))

            if match(.symbol(.lparen)) {
                while case .identifier(let arg) = peek() {
                    advance()
                    args.append(arg)

                    if !match(.symbol(.comma)) {
                        break
                    }
                }

                try consume(.symbol(.rparen))
            } else if case .identifier(let arg) = peek() {
                advance()
                args.append(arg)
            } else {
                return nil
            }

            try consume(.symbol(.thickArrow))

            let body = try expression()

            return Expr.Fun(args: args, body: body, isIterator: isIterator)
        }) {
            return f
        }

        return try logicalOr()
    }

    // or -> and ('or' and)*
    func logicalOr() throws -> Expr {
        var lhs = try logicalAnd()

        while match(.keyword(.Or)) {
            let rhs = try logicalAnd()
            lhs = .BinaryOp(lhs, .and, rhs)
        }

        return lhs
    }

    // and -> equality ('and' equality)*
    func logicalAnd() throws -> Expr {
        var lhs = try equality()

        while match(.keyword(.And)) {
            let rhs = try equality()
            lhs = .BinaryOp(lhs, .and, rhs)
        }

        return lhs
    }

    // equality -> comparison (('==' | '!=') comparison)*
    func equality() throws -> Expr {
        var lhs = try comparison()

        while match(.symbol(.eqeq), .symbol(.bangeq)) {
            var op: BinaryOperator?

            switch previous() {
            case .symbol(.eqeq):
                op = .equ
            case .symbol(.bangeq):
                op = .neq
            default:
                break
            }

            let rhs = try comparison()
            lhs = .BinaryOp(lhs, op!, rhs)
        }

        return lhs
    }

    // comparison -> additive (('<' | '<=' | '>' | '>=') additive)*
    func comparison() throws -> Expr {
        var lhs = try additive()

        while match(.symbol(.lss), .symbol(.leq), .symbol(.gtr), .symbol(.geq)) {
            var op: BinaryOperator?

            switch previous() {
            case .symbol(.lss):
                op = .lss
            case .symbol(.leq):
                op = .leq
            case .symbol(.gtr):
                op = .gtr
            case .symbol(.geq):
                op = .geq
            default:
                break
            }

            let rhs = try additive()
            lhs = .BinaryOp(lhs, op!, rhs)
        }

        return lhs
    }

    // additive -> multiplicative (('+' | '-') multiplicative)*
    func additive() throws -> Expr {
        var lhs = try multiplicative()

        while match(.symbol(.plus), .symbol(.minus)) {
            var op: BinaryOperator?

            switch previous() {
            case .symbol(.plus):
                op = .add
            case .symbol(.minus):
                op = .sub
            default:
                break
            }

            let rhs = try multiplicative()
            lhs = .BinaryOp(lhs, op!, rhs)
        }

        return lhs
    }

    // multiplicative -> unary (('*' | '/' | '%') unary)*
    func multiplicative() throws -> Expr {
        var lhs = try unary()

        while match(.symbol(.star), .symbol(.slash), .symbol(.percent)) {
            var op: BinaryOperator?

            switch previous() {
            case .symbol(.star):
                op = .mul
            case .symbol(.slash):
                op = .div
            case .symbol(.percent):
                op = .mod
            default:
                break
            }

            let rhs = try unary()
            lhs = .BinaryOp(lhs, op!, rhs)
        }

        return lhs
    }

    // unary -> ('-' | '!') unary | primary
    func unary() throws -> Expr {
        switch peek() {
        case .symbol(.minus):
            advance()
            return .UnaryOp(.arithmeticNegation, try unary())
        case .symbol(.bang):
            advance()
            return .UnaryOp(.logicalNegation, try unary())
        default:
            return try call()
        }
    }

    // call -> primary '(' args ')' | ('.' ident) | ('->' ident '(' args ')'))*
    // args -> commas(expr)
    func call() throws -> Expr {
        var lhs = try primary()
        
        while true {
            if match(.symbol(.lparen)) {
                let args = try commas(expression)
                try consume(.symbol(.rparen))
                lhs = .Call(f: lhs, args: args)
            } else if match(.symbol(.dot)) {
                let field = try identifier()
                lhs = .RecordSelect(lhs, field: field)
            } else if match(.symbol(.thinArrow)) {
                let f = try identifier()
                
                let remArgs: [Expr]
                if match(.symbol(.lparen)) {
                    remArgs = try commas(expression)
                    try consume(.symbol(.rparen))
                } else {
                    remArgs = []
                }
                
                lhs = .Pipeline(arg1: lhs, f: f, remArgs: remArgs)
            } else {
                break
            }
        }
        
        return lhs
    }

    // primary -> unit | num | bool | str | identifier | array | tuple | parens
    func primary() throws -> Expr {
        switch peek() {
        case .num(let x):
            advance()
            return .Literal(.num(x))
        case .bool(let q):
            advance()
            return .Literal(.bool(q))
        case .str(let s):
            advance()
            return .Literal(.str(s))
        case .keyword(.True):
            advance()
            return .Literal(.bool(true))
        case .keyword(.False):
            advance()
            return .Literal(.bool(false))
        case .identifier(let n):
            advance()
            return .Var(n)
        case .symbol(.lparen):
            advance()
            return try tupleOrParensOrUnit()
        case .symbol(.lcurlybracket):
            advance()
            return try blockOrRecord()
        case .symbol(.lbracket):
            advance()
            return try array()
        default:
            throw ParserError.expectedExpression
        }
    }
    
    // blockOrRecord -> block | record
    func blockOrRecord() throws -> Expr {
        if match(.symbol(.rcurlybracket)) {
            return .Record([])
        }
        
        if case .identifier(_) = peek(), case .symbol(.colon) = peek(1) {
            return try record()
        } else {
            return try block()
        }
    }
    
    // record -> '{' ((ident ':' expr) (',' ident ':' expr)*)? '}'
    func record() throws -> Expr {
        var entries = [(String, Expr)]()
        
        while case let .identifier(field) = peek() {
            advance()
            try consume(.symbol(.colon))
            let val = try expression()
            entries.append((field, val))
            
            if !match(.symbol(.comma)) {
                break
            }
        }
        
        try consume(.symbol(.rcurlybracket))
        
        return .Record(entries)
    }

    // block -> '{' stmt* expr? '}'
    func block() throws -> Expr {
        var stmts = [Stmt]()

        while let stmt = attempt(statementThrowing) {
            stmts.append(stmt)
        }

        if match(.symbol(.rcurlybracket)) {
            if case let .Expr(ret) = stmts.last {
                return .Block(stmts.dropLast(1), ret: ret)
            }
            
            return .Block(stmts, ret: nil)
        }

        let ret = try expression()
        try consume(.symbol(.rcurlybracket))

        return .Block(stmts, ret: ret)
    }
    
    // array -> '[' (expr (',' expr)*)? ']'
    func array() throws -> Expr {
        var elems = [Expr]()

        while let elem = attempt(expression) {
            elems.append(elem)

            if !match(.symbol(.comma)) {
                break
            }
        }
        
        try consume(.symbol(.rbracket))
        
        return .Array(elems)
    }

    // tupleOrParensOrUnit -> unit | parens | tuple
    // unit -> '(' ')'
    // parens -> '(' expr ')'
    // tuple -> '(' expr (',' expr)+ ')'
    func tupleOrParensOrUnit() throws -> Expr {
        var exprs = [Expr]()

        while let expr = attempt(expression) {
            exprs.append(expr)

            if !match(.symbol(.comma)) {
                break
            }
        }

        try consume(.symbol(.rparen))

        switch exprs.count {
        case 0:
            return .Literal(.unit)
        case 1:
            return .Parens(exprs[0])
        default:
            return .Tuple(exprs)
        }
    }
    
    // ------ types ------
    
    func type() throws -> Ty {
        return try functionOrTupleType()
    }
    
    // fun -> '(' commas(ty) ')' '=>' ty | ty '=>' ty | tuple | () | parens | array
    // tuple -> '(' ty, commas(ty) ')'
    // parens ->
    // unit -> '(' ')'
    func functionOrTupleType() throws -> Ty {
        // '(' commas(ty) ')' '=>' ty
        if match(.symbol(.lparen)) {
            let argTys = try commas(type)
            try consume(.symbol(.rparen))
            
            if match(.symbol(.thickArrow)) {
                let retTy = try type()
                return .fun(argTys, retTy)
            } else { // it's a tuple or unit or parens
                switch argTys.count {
                case 0: return .unit
                case 1: return argTys[0]
                default: return .tuple(argTys)
                }
            }
        }
        
        let lhs = try arrayType()
        
        // ty '=>' ty
        if match(.symbol(.thickArrow)) {
            let retTy = try type()
            return .fun([lhs], retTy)
        }
        
        return lhs
    }
    
    // array -> prim '[' ']' | prim
    func arrayType() throws -> Ty {
        let lhs = try primitiveType()
        
        if match(.symbol(.lbracket)) {
            try consume(.symbol(.rbracket))
            return .array(lhs)
        }
        
        return lhs
    }

    // prim -> unit | num | bool | str | tuple | record | parens
    func primitiveType() throws -> Ty {
        switch peek() {
        case .identifier("num"):
            advance()
            return .num
        case .identifier("bool"):
            advance()
            return .bool
        case .identifier("str"):
            advance()
            return .str
        case .symbol(.lcurlybracket):
            advance()
            return try recordType()
        default:
            throw ParserError.expectedType
        }
    }
    
    // record -> '{' commas(ident ':' ty) '}'
    func recordType() throws -> Ty {
        var entries = [(String, Ty)]()
        
        while case let .identifier(field) = peek() {
            advance()
            try consume(.symbol(.colon))
            let ty = try type()
            
            entries.append((field, ty))
            
            if !match(.symbol(.comma)) {
                break
            }
        }
        
        try consume(.symbol(.rcurlybracket))
        
        return .record(Row.from(entries: entries))
    }
}

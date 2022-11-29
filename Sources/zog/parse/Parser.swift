//
//  Parser.swift
//
//
//  Created by Nathan on 13/11/2022.
//

import Foundation

class Parser {
    let tokens: [TokenWithPos]
    var index: Int
    var statementStartIndex: Int
    var errors: [(ParserError, start: Int, end: Int)]
    var generalizationLevel: UInt
    var tyParamScopes: [[String:Ty]]
    
    init(tokens: [TokenWithPos]) {
        self.tokens = tokens
        errors = []
        index = 0
        statementStartIndex = 0
        generalizationLevel = 0
        tyParamScopes = []
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
        if case .identifier(let ident) = peek() {
            advance()
            return ident
        }

        throw ParserError.expectedIdentifier
    }
    
    func upperIdentifier() throws -> String {
        if case .identifier(let ident) = peek(), ident.first!.isUppercase {
            advance()
            return ident
        }
        
        throw ParserError.expectedUppercaseIdentifier
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
    func sepBy<T>(_ rule: () throws -> T, separator: Token) rethrows -> [T] {
        var terms = [T]()
        
        if !check(.symbol(.rparen)) {
            repeat {
                terms.append(try rule())
            } while match(separator)
        }
        
        return terms
    }
    
    // commas(rule) -> (<rule> (',' <rule>)* ','?)?
    func commas<T>(_ rule: () throws -> T) throws -> [T] {
        return try sepBy(rule, separator: .symbol(.comma))
    }
    
    func typeAnnotation(primitive: Bool = false) throws -> Ty? {
        if match(.symbol(.colon)) {
            return primitive ? try primitiveType() : try type()
        }
        
        return nil
    }
    
    func pushTyParamScope() {
        tyParamScopes.append([String:Ty]())
    }
    
    func declareTyParam(_ name: String) -> Ty {
        if !tyParamScopes.isEmpty {
            let id = TyContext.freshTyVarId()
            let ty = Ty.variable(Ref(.unbound(id: id, level: generalizationLevel)))
            tyParamScopes[tyParamScopes.count - 1][name] = ty
            return ty
        }
        
        assertionFailure("no type parameter scope")
        return .unit
    }
    
    func lookupTyParam(_ name: String) -> Ty? {
        for scope in tyParamScopes.reversed() {
            if let v = scope[name] {
                return v
            }
        }
        
        return nil
    }
    
    func popTyParamScope() {
        _ = tyParamScopes.popLast()
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

    // prog -> decl*
    func program() throws -> [Decl] {
        var decls = [Decl]()

        while let _ = peek() {
            decls.append(try declaration())
        }

        return decls
    }
    
    // ------ declaration ------
    
    func declaration() throws -> Decl {
        switch peek() {
        case .identifier("type") where peek(2) == .symbol(.eq):
            advance()
            return try typeAlias()
        default:
            return .Stmt(statement())
        }
    }
    
        // typeAlias -> 'type' identifier '=' ty
    func typeAlias() throws -> Decl {
        let name = try identifier()
        try consume(.symbol(.eq))
        pushTyParamScope()
        let ty = try type()
        popTyParamScope()
        try consume(.symbol(.semicolon))
        
        return .TypeAlias(name, ty)
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
        let stmt: Stmt
        
        pushTyParamScope()
        
        switch peek() {
        case .keyword(.Let):
            advance()
            stmt = try letStmt(isMut: false)
        case .keyword(.Mut):
            advance()
            stmt = try letStmt(isMut: true)
        case .keyword(.While):
            advance()
            stmt = try whileStmt()
        case .keyword(.For):
            advance()
            stmt = try forStmt()
        case .keyword(.If):
            advance()
            stmt = try ifStmt()
        case .keyword(.Return):
            advance()
            let expr = attempt(expression)
            try consume(.symbol(.semicolon))

            stmt = .Return(expr)
        case .keyword(.Yield):
            advance()
            let expr = try expression()
            try consume(.symbol(.semicolon))

            stmt = .Yield(expr)
        case .keyword(.Break):
            advance()
            try consume(.symbol(.semicolon))
            stmt = .Break
        default:
            stmt = try exprStmt()
        }
        
        popTyParamScope()
        
        return stmt
    }

    // letStmt -> ('let' | 'mut') pattern '=' expr ';'
    func letStmt(isMut: Bool) throws -> Stmt {
        let pat = try pattern()
        generalizationLevel += 1
        let ty = try typeAnnotation()
        try consume(.symbol(.eq))
        let val = try expression()
        generalizationLevel -= 1
        try consume(.symbol(.semicolon))

        return .Let(mut: isMut, pat: pat, ty: ty, val: val)
    }

    // while -> 'while' expr '{' stmt* '}'
    func whileStmt() throws -> Stmt {
        let cond = try expression()
        let body = try statementListBlock()
        try consume(.symbol(.semicolon))

        return .While(cond: cond, body: body)
    }

    // for -> 'for' pattern 'in' expr '{' stmt* '}'
    func forStmt() throws -> Stmt {
        let pat = try pattern()
        try consume(.keyword(.In))
        let iterator = try expression()
        let body = try statementListBlock()
        try consume(.symbol(.semicolon))

        return .For(pat: pat, iterator: iterator, body: body)
    }
    
    // ifStmt -> 'if' expr '{' stmt* '}'
    func ifStmt() throws -> Stmt {
        let cond = try expression()
        let then = try statementListBlock()
        var else_: [Stmt]? = nil
        
        if match(.keyword(.Else)) {
            else_ = try statementListBlock()
        }
        
        try consume(.symbol(.semicolon))

        return .If(cond: cond, then: then, else_: else_)
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
    
    // useIn -> 'use' pattern '=' expr 'in' expr | if
    func useIn() throws -> Expr {
        if match(.identifier("use")) {
            let pat = try pattern()
            let ty = try typeAnnotation()
            try consume(.symbol(.eq))
            let val = try expression()
            try consume(.keyword(.In))
            let rhs = try expression()
            
            return Expr.UseIn(pat: pat, ty: ty, val: val, rhs: rhs)
        }
        
        return try ifExpr()
    }

    // if -> 'if' expr expr 'else' expr | match
    func ifExpr() throws -> Expr {
        if match(.keyword(.If)) {
            let cond = try expression()
            let thenExpr = try expression()
            try consume(.keyword(.Else))
            let elseExpr = try expression()

            return .If(cond: cond, thenExpr: thenExpr, elseExpr: elseExpr)
        }

        return try matchExpr()
    }
    
    // match -> 'match' expr '{' (pattern '=>' expr ';')* '}'
    func matchExpr() throws -> Expr {
        if match(.keyword(.Match)) {
            let subject = try expression()
            var cases = [(Pattern, Expr)]()
            try consume(.symbol(.lcurlybracket))
            
            repeat {
                let pat = try pattern()
                try consume(.symbol(.thickArrow))
                let body = try expression()
                cases.append((pat, body))
                
                if !match(.symbol(.semicolon), .symbol(.comma)) {
                    break
                }
            } while !check(.symbol(.rcurlybracket))
            
            try consume(.symbol(.rcurlybracket))
            
            return .Match(subject, cases: cases)
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

    // fun -> 'iterator'? ('(' commas(pattern (':' type)?) ')' | pattern) '=>' expr | logicalOr
    func fun() throws -> Expr {
        if let f: Expr = attempt({
            var args = [(Pattern, Ty?)]()
            let isIterator = match(.keyword(.Iterator))
            var retTy: Ty? = nil

            if match(.symbol(.lparen)) {
                if !check(.symbol(.rparen)) {
                    repeat {
                        let pat = try pattern()
                        let ty = try typeAnnotation()
                        args.append((pat, ty))
                    } while match(.symbol(.comma))
                }

                try consume(.symbol(.rparen))
                
                retTy = try typeAnnotation(primitive: true)
            } else if let pat = attempt(pattern) {
                args.append((pat, nil))
            }

            try consume(.symbol(.thickArrow))

            let body = try expression()

            return Expr.Fun(args: args, retTy: retTy, body: body, isIterator: isIterator)
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
        case .raw(let js):
            advance()
            return .Raw(js: js)
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
        return try functionType()
    }
    
    // fun -> '(' commas(ty) ')' '=>' ty | ty '=>' ty | array
    func functionType() throws -> Ty {
        // '(' commas(ty) ')' '=>' ty
        if check(.symbol(.lparen)) {
            if let funTy = attempt({
                advance()
                let argTys = try commas(type)
                try consume(.symbol(.rparen))
                
                if match(.symbol(.thickArrow)) {
                    let retTy = try type()
                    return Ty.fun(argTys, retTy)
                } else {
                    throw ParserError.expected(.symbol(.thickArrow))
                }
            }) {
                return funTy
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

    // prim -> unit | num | bool | str | tuple | record | enum | parens
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
        case .identifier(let name) where name.first!.isUppercase:
            advance()
            return try constType(name: name)
        case .symbol(.singlequote):
            advance()
            return try typeParam()
        case .symbol(.lcurlybracket):
            advance()
            return try recordType()
        case .symbol(.lparen):
            advance()
            return try tupleOrParensOrUnitType()
        case .identifier("enum"):
            advance()
            return try enumType()
        default:
            throw ParserError.expectedType
        }
    }
    
    // tuple -> '(' ty, commas(ty) ')'
    // parens -> '(' type ')'
    // unit -> '(' ')'
    func tupleOrParensOrUnitType() throws -> Ty {
        let tys = try commas(type)
        try consume(.symbol(.rparen))
        
        switch tys.count {
        case 0: return .unit
        case 1: return tys[0]
        default: return .tuple(tys)
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
        
        return .record(Row.from(entries: entries, tail: Ty.fresh(level: generalizationLevel)))
    }
    
    // enum -> 'enum' '{' (identifier ty)* '}'
    func enumType() throws -> Ty {
        var variants = [(name: String, ty: Ty?)]()
        
        try consume(.symbol(.lcurlybracket))
        
        while case let .identifier(name) = peek() {
            advance()
            let ty = attempt(type)
            variants.append((name, ty))
            
            if !match(.symbol(.semicolon), .symbol(.comma)) {
                break
            }
        }
        
        try consume(.symbol(.rcurlybracket))
        
        return .enum_(variants)
    }
    
    // var -> upperIdentifier ('<' commas(type) '>')?
    func constType(name: String) throws -> Ty {
        if match(.symbol(.lss)) {
            let args = try commas(type)
            try consume(.symbol(.gtr))
            return .const(name, args)
        }
        
        return .const(name, [])
    }
    
    // param -> '\'' upperIdentifier
    func typeParam() throws -> Ty {
        let name = try upperIdentifier()
        
        if let ty = lookupTyParam(name) {
            return ty
        }
        
        return declareTyParam(name)
    }
    
    // ------ patterns ------
    
    func pattern() throws -> Pattern {
        switch peek() {
        case .symbol(.underscore):
            advance()
            return .any
        case let .identifier(name):
            advance()
            return .variable(name)
        case .symbol(.lparen) where !check(.symbol(.rparen), lookahead: 1):
            advance()
            return try tupleOreParensPattern()
        case .symbol(.lcurlybracket):
            advance()
            return try recordPattern()
        case .num(let x):
            advance()
            return .literal(.num(x))
        case .str(let s):
            advance()
            return .literal(.str(s))
        case .keyword(.True):
            advance()
            return .literal(.bool(true))
        case .keyword(.False):
            advance()
            return .literal(.bool(false))
        default:
            throw ParserError.expectedPattern
        }
    }
    
    func tupleOreParensPattern() throws -> Pattern {
        let patterns = try commas(pattern)
        try consume(.symbol(.rparen))
        
        if patterns.count == 1 {
            return patterns[0]
        }
        
        return .tuple(patterns)
    }
    
    func recordPattern() throws -> Pattern {
        var entries = [(String, Pattern?)]()
        
        while case let .identifier(name) = peek() {
            advance()
            let pat = match(.symbol(.colon)) ? try pattern() : nil
            entries.append((name, pat))
            
            if !match(.symbol(.comma)) {
                break
            }
        }
        
        try consume(.symbol(.rcurlybracket))
        
        return .record(entries)
    }
}

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

  func peek() -> Token? {
    guard index < tokens.count else {
      return .none
    }

    return tokens[index].token
  }

  func previous() -> Token {
    return tokens[index - 1].token
  }

  func check(_ token: Token) -> Bool {
    if let t = peek() {
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
      return .none
    }
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
    var stmts: [Stmt] = []

    while let _ = peek() {
      stmts.append(statement())
    }

    return stmts
  }

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
      let start = tokens[statementStartIndex].start
      let end = tokens[index].end
      errors.append((.expectedStatement, start: start, end: end))
      synchronize()
      return .Error(.expectedStatement, span: (start, end))
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
    try consume(.symbol(.eq))
    let val = try expression()
    try consume(.symbol(.semicolon))

    return .Let(mut: isMut, name: name, val: val)
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

  // stmtListBlock -> '{' stmt* '}'
  func statementListBlock() throws -> [Stmt] {
    try consume(.symbol(.lcurlybracket))
    var stmts: [Stmt] = []

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

  // expr -> ifExpr
  func expression() throws -> Expr {
    return try ifExpr()
  }

  // if -> 'if' expr expr 'else' expr | assignment
  func ifExpr() throws -> Expr {
    if match(.keyword(.If)) {
      let cond = try expression()
      let thenExpr = try expression()
      let elseExpr: Expr? = match(.keyword(.Else)) ? try expression() : .none

      return .If(cond: cond, thenExpr: thenExpr, elseExpr: elseExpr)
    }

    return try assignment()
  }

  // assignment -> fun ('=' | '+=' | '-=' | '*=' | '/=') assignment | fun
  func assignment() throws -> Expr {
    let lhs = try fun()

    if match(.symbol(.eq), .symbol(.pluseq), .symbol(.minuseq), .symbol(.stareq), .symbol(.slasheq))
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
      case .symbol(.pluseq): op = .plusEq
      case .symbol(.minuseq): op = .minusEq
      case .symbol(.stareq): op = .timesEq
      case .symbol(.slasheq): op = .divideEq
      default: op = .eq
      }

      let rhs = try assignment()

      return .Assignment(lhs, op, rhs)
    }

    return lhs
  }

  // fun -> ('(' (identifier (',' identifier)*)? ')' | identifier) '->' expr | logicalOr
  func fun() throws -> Expr {
    if let f: Expr = attempt({
      var args: [String] = []

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
        return .none
      }

      try consume(.symbol(.arrow))

      let body = try expression()

      return Expr.Fun(args: args, body: body)
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
      case .symbol(.eqeq): op = .equ
      case .symbol(.bangeq): op = .neq
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
      case .symbol(.lss): op = .lss
      case .symbol(.leq): op = .leq
      case .symbol(.gtr): op = .gtr
      case .symbol(.geq): op = .geq
      default: break
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
      case .symbol(.plus): op = .add
      case .symbol(.minus): op = .sub
      default: break
      }

      let rhs = try multiplicative()
      lhs = .BinaryOp(lhs, op!, rhs)
    }

    return lhs
  }

  // multiplicative -> unary (('*' | '/') unary)*
  func multiplicative() throws -> Expr {
    var lhs = try unary()

    while match(.symbol(.star), .symbol(.slash)) {
      var op: BinaryOperator?

      switch previous() {
      case .symbol(.star): op = .mul
      case .symbol(.slash): op = .div
      default: break
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
      return .UnaryOp(.arithmeticNegation, try unary())
    case .symbol(.bang):
      return .UnaryOp(.logicalNegation, try unary())
    default:
      return try call()
    }
  }

  // call -> primary '(' (expression (',' expression)*)? ')'
  func call() throws -> Expr {
    let lhs = try primary()

    if match(.symbol(.lparen)) {
      var args: [Expr] = []

      while let expr = attempt(expression) {
        args.append(expr)

        if !match(.symbol(.comma)) {
          break
        }
      }

      try consume(.symbol(.rparen))

      return .Call(f: lhs, args: args)
    }

    return lhs
  }

  // primary -> num | bool | str | identifier | tupleOrParensOrUnit
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
      return try block()
    default:
      throw ParserError.expectedExpression
    }
  }

  func block() throws -> Expr {
    var stmts: [Stmt] = []

    while let stmt = attempt(statementThrowing) {
      stmts.append(stmt)
    }

    if match(.symbol(.rcurlybracket)) {
      return .Block(stmts, ret: .none)
    }

    let ret = try expression()
    try consume(.symbol(.rcurlybracket))

    return .Block(stmts, ret: ret)
  }

  // tupleOrParensOrUnit -> unit | parens | tuple
  func tupleOrParensOrUnit() throws -> Expr {
    var exprs: [Expr] = []

    while let _ = peek() {
      if let expr = attempt(expression) {
        exprs.append(expr)

        if !match(.symbol(.comma)) {
          break
        }
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
}

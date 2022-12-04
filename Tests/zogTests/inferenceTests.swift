//
//  inferenceTests.swift
//  
//
//  Created by nathan on 23/11/2022.
//

import XCTest

@testable import zog

final class inferenceTests: XCTestCase {
    func infer(_ source: String, _ env: TypeEnv = TypeEnv()) throws -> Ty {
        var lexer = Lexer.init(source: source)
        let tokens = lexer.lex()
        let parser = Parser.init(tokens: tokens)
        parser.pushTyParamScope()
        let expr = try parser.expression().core(0)
        return try expr.infer(env, 0)
    }
    
    func infer(statements source: [String]) throws -> TypeEnv {
        var lexer = Lexer.init(source: source.joined(separator: "\n") + "\n")
        let tokens = lexer.lex()
        let parser = Parser.init(tokens: tokens)
        let prog = try parser.program()
        let core = prog.map({ stmt in stmt.core(0) })
        let env = TypeEnv()
        
        for stmt in core {
            try stmt.infer(env, 0)
        }
        
        return env
    }
    
    func testInferPrimitiveTy() throws {
        XCTAssertEqual(try infer("3"), .num)
        XCTAssertEqual(try infer("3.14"), .num)
        XCTAssertEqual(try infer("(0)"), .num)
        XCTAssertEqual(try infer("true"), .bool)
        XCTAssertEqual(try infer("false"), .bool)
        XCTAssertEqual(try infer("\"hello\""), .str)
        XCTAssertEqual(try infer("()"), .unit)
    }
    
    func testInferTupleTy() throws {
        XCTAssertEqual(try infer("(1, 2, 3)"), .tuple([.num, .num, .num]))
        XCTAssertEqual(try infer("(true, \"zog\", 24, ())"), .tuple([.bool, .str, .num, .unit]))
        XCTAssertEqual(
            String(describing: try infer("((1, (2, (3, (4)))), (5, 6))")),
            "((num, (num, (num, num))), (num, num))"
        )
    }
    
    func testInferBinOpTy() throws {
        XCTAssertEqual(try infer("3 + 7"), .num)
        XCTAssertEqual(try infer("3 - 7"), .num)
        XCTAssertEqual(try infer("3 * 7"), .num)
        XCTAssertEqual(try infer("3 / 7"), .num)
        XCTAssertEqual(try infer("3 % 7"), .num)
        XCTAssertEqual(try infer("3 * (7 + 1)"), .num)
        XCTAssertEqual(try infer("3 > 7"), .bool)
        XCTAssertEqual(try infer("3 < 7"), .bool)
        XCTAssertEqual(try infer("3 >= 7"), .bool)
        XCTAssertEqual(try infer("3 <= 7"), .bool)
        XCTAssertEqual(try infer("3 == 7"), .bool)
        XCTAssertEqual(try infer("3 != 7"), .bool)
    }
    
    func testInferFunTy() throws {
        XCTAssertEqual(try infer("x => x").canonical, "A => A")
        XCTAssertEqual(try infer("_ => 1").canonical, "A => num")
        XCTAssertEqual(try infer("(a, b) => a == b").canonical, "(A, A) => bool")
        XCTAssertEqual(try infer("(a, b) => a != b").canonical, "(A, A) => bool")
        XCTAssertEqual(try infer("(a, b) => a + b").canonical, "(num, num) => num")
        XCTAssertEqual(try infer("(a, b) => a < b").canonical, "(num, num) => bool")
        XCTAssertEqual(
            try infer("((a, (b, c))) => { k: a == b and c }").canonical,
            "((A, (A, bool))) => { k: bool }"
        )
    }
    
    func testInferPolyTy() throws {
        let env1 = try infer(statements: [
            "let id = x => x",
            "let a = id(3)",
            "let b = id(\"yo!\")"
        ])
        
        XCTAssertEqual(env1.vars["id"]?.canonical, "a => a")
        XCTAssertEqual(env1.vars["a"], .num)
        XCTAssertEqual(env1.vars["b"], .str)
        
        let env2 = try infer(statements: [
            "let fst = ((a, _)) => a",
        ])
        
        XCTAssertEqual(env2.vars["fst"]?.canonical, "((a, b)) => a")
    }
    
    func testInferArrayTy() throws {
        let env1 = try infer(statements: ["let a1 = []", "mut a2 = []"])
        XCTAssertEqual(env1.vars["a1"]?.canonical, "a[]")
        XCTAssertEqual(env1.vars["a2"]?.canonical, "A[]")
        
        let env2 = try infer(statements: ["let array = [1, 2, 3]"])
        XCTAssertEqual(env2.vars["array"]?.canonical, "num[]")
        
        XCTAssertThrowsError(try infer(statements: ["let array = [1, true, ()]"]))
    }
    
    func testInferRecordTy() throws {
        XCTAssertEqual(try infer("{}"), .record(.empty))
        XCTAssertEqual(
            try infer("{ a: 1, b: true, c: \"zog\" }"),
            .record(Row.from(entries: [("a", .num), ("b", .bool), ("c", .str)]))
        )
        XCTAssertEqual(
            try infer("{ val: { lhs: 1, rhs: 2 } }"),
            .record(Row.from(
                entries: [("val",.record(Row.from(entries: [("lhs", .num), ("rhs", .num)])))]
            ))
        )
    }
    
    func testInferVariantTy() throws {
        let env1 = try infer(statements: [
            "enum BinaryOp { Plus, Minus }",
            "let a = Plus"
        ])
        
        XCTAssertEqual(env1.vars["a"], .const("BinaryOp", []))
        
        let env2 = try infer(statements: [
            "enum List { Nil, Cons (num, List) }",
            "let list = Cons (1, Cons (2, Cons (3, Nil)))"
        ])
        
        XCTAssertEqual(env2.vars["list"]?.canonical, "List")
        
        let env3 = try infer(statements: [
            "enum BinaryOp { Plus, Minus }",
            "enum UnaryOp { Bang, Minus }",
        ])
        
        XCTAssertEqual(try infer(".Plus", env3).canonical, "BinaryOp")
        XCTAssertEqual(try infer(".Bang", env3).canonical, "UnaryOp")
        XCTAssertEqual(try infer("BinaryOp.Minus", env3).canonical, "BinaryOp")
        XCTAssertEqual(try infer("UnaryOp.Minus", env3).canonical, "UnaryOp")
        XCTAssertThrowsError(try infer(".Minus", env3))
        XCTAssertEqual(try infer("use op: UnaryOp = Minus in op", env3).canonical, "UnaryOp")
        XCTAssertEqual(
            try infer("op => match op { Plus => 0, Minus => 1 }", env3).canonical,
            "BinaryOp => num"
        )
        XCTAssertEqual(
            try infer("op => match op { Bang => 0, Minus => 1 }", env3).canonical,
            "UnaryOp => num"
        )
        XCTAssertEqual(
            try infer("(op: UnaryOp) => match op { Minus => 0, _ => 1 }", env3).canonical,
            "UnaryOp => num"
        )
        XCTAssertEqual(
            try infer("(op: BinaryOp) => match op { Minus => 0, _ => 1 }", env3).canonical,
            "BinaryOp => num"
        )
    }
}

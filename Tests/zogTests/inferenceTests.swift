//
//  inferenceTests.swift
//  
//
//  Created by nathan on 23/11/2022.
//

import XCTest

@testable import zog

final class inferenceTests: XCTestCase {
    func infer(_ source: String) throws -> Ty {
        var lexer = Lexer.init(source: source)
        let tokens = lexer.lex()
        let parser = Parser.init(tokens: tokens)
        let expr = try parser.expression().core(0)
        let env = TypeEnv()
        return try expr.infer(env, 0)
    }
    
    func infer(statements source: [String]) throws -> TypeEnv {
        var lexer = Lexer.init(source: source.joined(separator: "\n") + "\n")
        let tokens = lexer.lex()
        let parser = Parser.init(tokens: tokens)
        let prog = parser.program()
        let core = prog.map({ stmt in stmt.core(0) })
        let env = TypeEnv()
        
        for stmt in core {
            try stmt.infer(env, 0)
        }
        
        return env
    }
    
    func testInferPrimitive() throws {
        XCTAssertEqual(try infer("3"), .num)
        XCTAssertEqual(try infer("3.14"), .num)
        XCTAssertEqual(try infer("(0)"), .num)
        XCTAssertEqual(try infer("true"), .bool)
        XCTAssertEqual(try infer("false"), .bool)
        XCTAssertEqual(try infer("\"hello\""), .str)
        XCTAssertEqual(try infer("()"), .unit)
    }
    
    func testInferTuple() throws {
        XCTAssertEqual(try infer("(1, 2, 3)"), .tuple([.num, .num, .num]))
        XCTAssertEqual(try infer("(true, \"zog\", 24, ())"), .tuple([.bool, .str, .num, .unit]))
        XCTAssertEqual(
            String(describing: try infer("((1, (2, (3, (4)))), (5, 6))")),
            "((num, (num, (num, num))), (num, num))"
        )
    }
    
    func testInferMathExpr() throws {
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
    
    func testInferFun() throws {
        XCTAssertEqual(try infer("x => x").canonical, "A => A")
        XCTAssertEqual(try infer("_ => 1").canonical, "A => num")
        XCTAssertEqual(try infer("(a, b) => a + b").canonical, "(num, num) => num")
        XCTAssertEqual(try infer("(a, b) => a < b").canonical, "(num, num) => bool")
        XCTAssertEqual(
            try infer("((a, (b, c))) => { k: a == b and c }").canonical,
            "(num, (num, bool)) => { k: bool }"
        )
    }
    
    func testInferPolyTy() throws {
        let env1 = try infer(statements: [
            "let id = x => x",
            "let a = id(3)",
            "let b = id(\"yo!\")"
        ])
        
        XCTAssertEqual(env1.vars["id"]?.canonical, "'A => 'A")
        XCTAssertEqual(env1.vars["a"], .num)
        XCTAssertEqual(env1.vars["b"], .str)
        
        let env2 = try infer(statements: [
            "let fst = ((a, _)) => a",
        ])
        
        XCTAssertEqual(env2.vars["fst"]?.canonical, "('A, 'B) => 'A")
    }
}

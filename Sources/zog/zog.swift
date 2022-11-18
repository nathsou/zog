@main
public struct zog {
    public static func main() {
        if CommandLine.arguments.count > 1 {
            do {
                let sourceFile = CommandLine.arguments[1]
                let source = try String(contentsOfFile: sourceFile)
                var lexer = Lexer.init(source: source)
                let tokens = lexer.lex()
                let parser = Parser.init(tokens: tokens)
                let prog = parser.parse()
                
                for (error, start, end) in parser.errors {
                    print("\(error) near \"\(String(lexer.chars[start...(end - 1)]))\"")
                }
                
                let env = Env()
                
                env.declare(varName: "print", ty: .fun([.variable(Ref(.generic(0)))], .unit))
                
                for stmt in prog {
                    print(stmt)
                    try stmt.infer(env, 0)
                }
                
                print("\n\(env)")
            } catch let error as ParserError {
                print("Parsing failed: \(error)")
            } catch let error as TypeError {
                print("Type inference failed: \(error)")
            } catch let error {
                print("Error: \(error)")
            }
        } else {
            print("usage: zog file.zog")
        }
    }
}

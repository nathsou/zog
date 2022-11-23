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
                
                if parser.errors.isEmpty {
                    let core = prog.map({ stmt in stmt.core(0) })
                    let env = TypeEnv()
                    try env.declare("print", ty: .fun([.variable(Ref(.generic(0)))], .unit))
                    let context = CoreContext()
                    _ = context.declare("print")
                    
                    func comment(_ text: CustomStringConvertible) -> String {
                        return String(describing: env).split(separator: "\n").map({ "// \($0)" }).joined(separator: "\n")
                    }
                    
                    try core.forEach({ stmt in try stmt.infer(env, 0) })
                    print(comment(env), "\n")
                    
                    for stmt in core {
                        context.statements.append(try stmt.codegen(context))
                    }
                    
                    print("const print = console.log;")
                    for stmt in context.statements {
                        print(stmt)
                    }
                }
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

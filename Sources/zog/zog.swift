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
                let prog = try parser.program()
                
                for (error, start, end) in parser.errors {
                    print("\(error) near \"\(String(lexer.chars[start...(end - 1)]))\"")
                }
                
                for decl in prog {
                    print(decl)
                }
                
                if parser.errors.isEmpty {
                    let core = prog.map({ decl in decl.core(0) })
                    let env = TypeEnv()
                    
                    func comment(_ text: CustomStringConvertible) -> String {
                        return String(describing: env).split(separator: "\n").map({ "// \($0)" }).joined(separator: "\n")
                    }
                    
                    try core.forEach({ decl in try decl.infer(env, 0) })
                    print(comment(env), "\n")
                    
                    let context = CoreContext()
                    
                    for decl in core {
                        context.statements.append(contentsOf: try decl.codegen(context))
                    }
                    
                    for decl in context.statements {
                        print(decl)
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

import Foundation

class Module {
    let name: String
    let members: [String:TypeEnv.VarInfo] 
    let typeAliases: [String:TypeEnv.AliasInfo]
    let enums: [String:TypeEnv.EnumInfo]
    let decls: [CoreDecl]
    let env: TypeEnv
    let ty: Ty

    init(name: String, env: TypeEnv, decls: [CoreDecl], level: UInt) {
       self.name = name
       self.env = env
       members = env.vars.filter({ $0.value.pub }) 
       typeAliases = env.aliases.filter({ $0.value.pub })
       enums = env.enums.filter({ $0.value.pub })
       self.decls = decls
       let mappingFunc = { (id: UInt) in Ty.fresh(level: level) }
       ty = .record(.from(entries: members.map({ (name, info) in
                (name, info.ty.substitute(mappingFunc: mappingFunc))
            })))
    }

    func serialize() throws -> String {
        var contents = ""
        let ctx = CoreContext(linear: false, env: env)

        for decl in decls {
            contents.append(
                contentsOf: String(describing: try decl.codegen(ctx).map({ d in "\(d)\n" }).newlines())
            )
        }

        return contents
    }
}

extension FileManager {
    func createModuleFile(module: Module) throws {
        createFile(atPath: "\(module.name).mjs", contents: (try module.serialize().data(using: .utf8)))
    }
}

class Resolver {
    var fileManager = FileManager()
    var modules = [String:Module]()

    func resolve(path: String, level: UInt) throws -> Module? {
        var dir = URL(string: path)!
        let fileName = dir.lastPathComponent
        dir.deleteLastPathComponent()
        let previousDirectory = fileManager.currentDirectoryPath
        fileManager.changeCurrentDirectoryPath(dir.absoluteString)
        defer { 
            fileManager.changeCurrentDirectoryPath(previousDirectory)
        }

        if let mod = modules[path] {
            return mod
        }

        let source = String(decoding: fileManager.contents(atPath: fileName)!, as: UTF8.self)
        var lexer = Lexer.init(source: source)
        let tokens = lexer.lex()
        let parser = Parser.init(tokens: tokens)
        let prog = try parser.program()
        
        if !parser.errors.isEmpty {
            for (error, start, end) in parser.errors {
                print("\(error) near \"\(String(lexer.chars[start...(end - 1)]))\" in module \"\(path)\"")
            }

            return nil
        }
        
        let rewritingCtx = RewritingContext()
        let core = prog.map({ decl in decl.core(rewritingCtx, 0) }).compactMap({ x in x })
        let ctx = TypeContext()
        try core.forEach({ decl in try decl.infer(ctx, 0) })
        let moduleName = String(fileName.split(separator: ".").first!)
        let mod = Module(name: moduleName, env: ctx.env, decls: core, level: level)
        modules[path] = mod

        try fileManager.createModuleFile(module: mod)

        return mod
    }
}

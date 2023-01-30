import Foundation

enum ResolverError: Error, CustomStringConvertible {
    case fileNotFound(String)
    case circularDependency([String])

    var description: String {
        switch self {
        case .fileNotFound(let path):
            return "File not found: \(path)"
        case .circularDependency(let modules):
            return "Circular dependency: \(modules.joined(separator: " -> "))"
        }
    }
}

class Module {
    let name: String
    let imports: [String:(members: [(name: String, alias: String?)], module: Module)]
    let members: [String:TypeEnv.VarInfo] 
    let typeAliases: [String:TypeEnv.AliasInfo]
    let traits: [String:TypeEnv.TraitInfo]
    let traitImpls: [String:[TypeEnv.TraitImplInfo]]
    let enums: [String:TypeEnv.EnumInfo]
    let decls: [CoreDecl]
    let rewritingRules: [String:TypeEnv.RewritingRuleInfo]
    let env: TypeEnv
    let ty: Ty

    init(name: String, env: TypeEnv, decls: [CoreDecl], level: UInt) {
        self.name = name
        self.env = env
        imports = [:]
        members = env.vars.filter({ $0.value.pub })
        typeAliases = env.aliases.filter({ $0.value.pub })
        traits = env.traits
        traitImpls = env.traitImpls
        enums = env.enums.filter({ $0.value.pub })
        rewritingRules = env.rewritingRules.filter({ $0.value.pub })
        self.decls = decls
        let mappingFunc = { (id: UInt) in Ty.fresh(level: level) }
        ty = .record(.from(entries: members.map({ (name, info) in
            (name, info.ty.substitute(mappingFunc: mappingFunc))
        })))
    }

    func serialize() throws -> String {
        var contents = ""
        let ctx = CoreContext(env: env)
                    
        for decl in decls {
            ctx.statements.append(contentsOf: try decl.codegen(ctx))
        }

        contents.append(
            contentsOf: String(describing: ctx.statements.map({ "\($0)" }).filter({ $0 != "" }).newlines(count: 2))
        )

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
    var visiting: [String] = []

    func resolve(path: String, level: UInt) throws -> Module? {
        var dir = URL(string: path)!
        let fileName = dir.lastPathComponent
        dir.deleteLastPathComponent()
        let previousDirectory = fileManager.currentDirectoryPath
        fileManager.changeCurrentDirectoryPath(dir.absoluteString)
        defer { 
            fileManager.changeCurrentDirectoryPath(previousDirectory)
        }

        let absolutePath = fileManager.currentDirectoryPath + "/" + fileName
        
        if visiting.contains(absolutePath) {
            throw ResolverError.circularDependency(visiting + [absolutePath])
        }

        visiting.append(absolutePath)
        defer { visiting.removeLast() }

        if let mod = modules[absolutePath] {
            return mod
        }

        if !fileManager.fileExists(atPath: fileName) {
            throw ResolverError.fileNotFound(fileName)
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
        
        let ctx = TypeContext(resolver: self)
        let core = try prog.map({ decl in try decl.core(ctx, 0) }).compactMap({ x in x })
        try core.forEach({ decl in try decl.infer(ctx, 0) })

        if globalParameters.showTypes {
            for (name, info) in ctx.env.vars {
                print("\(name): \(info.ty.canonical)")
            }
        }

        let moduleName = String(fileName.split(separator: ".").first!)
        let mod = Module(name: moduleName, env: ctx.env, decls: core, level: level)
        modules[absolutePath] = mod

        try fileManager.createModuleFile(module: mod)

        return mod
    }
}

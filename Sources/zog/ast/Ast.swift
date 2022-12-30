//
//  Ast.swift
//
//
//  Created by Nathan on 12/11/2022.
//

import Foundation

enum Literal: CustomStringConvertible, Hashable {
    case unit
    case bool(Bool)
    case num(Float64)
    case str(String)

    public var description: String {
        switch self {
        case .bool(let q): return "\(q)"
        case .num(let x) where x == floor(x): return "\(Int(x))"
        case .num(let x): return "\(x)"
        case .str(let s): return "\"\(s)\""
        case .unit: return "()"
        }
    }
    
    var ty: Ty {
        switch self {
        case .unit: return .unit
        case .bool(_): return .bool
        case .num(_): return .num
        case .str(_): return .str
        }
    }
}

enum UnaryOperator: CustomStringConvertible {
    case logicalNegation, arithmeticNegation

    public var description: String {
        switch self {
        case .logicalNegation: return "!"
        case .arithmeticNegation: return "-"
        }
    }
}

enum BinaryOperator: CustomStringConvertible {
    case add, sub, mul, div, mod, pow, equ, neq, lss, leq, gtr, geq, and, or, concat

    public var description: String {
        switch self {
        case .add: return "+"
        case .sub: return "-"
        case .mul: return "*"
        case .div: return "/"
        case .mod: return "%"
        case .pow: return "**"
        case .equ: return "=="
        case .neq: return "!="
        case .lss: return "<"
        case .leq: return "<="
        case .gtr: return ">"
        case .geq: return ">="
        case .and: return "&&"
        case .or: return "||"
        case .concat: return "++"
        }
    }
}

enum AssignmentOperator: CustomStringConvertible {
    case eq, plusEq, minusEq, timesEq, divideEq

    public var description: String {
        switch self {
        case .eq: return "="
        case .plusEq: return "+="
        case .minusEq: return "-="
        case .timesEq: return "*="
        case .divideEq: return "/="
        }
    }
}

extension Array where Array.Element: CustomStringConvertible {
    func sepBy(_ separator: String) -> String {
        return map({ String(describing: $0) }).joined(separator: separator)
    }
    
    func commas() -> String {
        return sepBy(", ")
    }
    
    func newlines(count: Int = 1) -> String {
        return sepBy(String(repeating: "\n", count: count))
    }
}

extension String {
    func when(_ condition: Bool) -> String {
        return condition ? self : ""
    }
}

func indent(_ str: CustomStringConvertible) -> String {
    return String(describing: str)
        .split(separator: "\n")
        .map({ s in "    \(s)" })
        .newlines()
}

func ann(_ annotation: Ty?) -> String {
    if let annotation {
        return ": \(annotation)"
    }
    
    return ""
}

enum FunModifier: CustomStringConvertible {
    case fun
    case iterator

    public var description: String {
        switch self {
        case .fun: return "fun"
        case .iterator: return "iterator"
        }
    }
}

indirect enum Expr: CustomStringConvertible {
    case Literal(Literal)
    case UnaryOp(UnaryOperator, Expr)
    case BinaryOp(Expr, BinaryOperator, Expr)
    case Parens(Expr)
    case Var(String)
    case Fun(modifier: FunModifier, args: [(Pattern, Ty?)], retTy: Ty?, body: Expr)
    case Call(f: Expr, args: [Expr])
    case Block([Stmt], ret: Expr?)
    case If(cond: Expr, then: [Stmt], else_: [Stmt])
    case Assignment(Expr, AssignmentOperator, Expr)
    case Tuple([Expr])
    case UseIn(pat: Pattern, ty: Ty?, val: Expr, rhs: Expr)
    case Array([Expr])
    case Record([(String, Expr)])
    case RecordSelect(Expr, field: String)
    case Pipeline(arg1: Expr, f: String, remArgs: [Expr])
    case Raw(js: String)
    case Match(Expr, cases: [(Pattern, Expr)])
    case Variant(typeName: String?, variantName: String, args: [Expr])
    case MethodCall(subject: Expr, method: String, args: [Expr])
    case BuiltInCall(String, [Expr])

    public var description: String {
        switch self {
        case let .Literal(lit):
            return String(describing: lit)
        case let .UnaryOp(op, expr):
            return "\(op)\(expr)"
        case let .BinaryOp(lhs, op, rhs):
            return "\(lhs) \(op) \(rhs)"
        case let .Parens(expr):
            return "(\(expr))"
        case let .Var(v):
            return v
        case let .Fun(modifier, args, retTy, body):
            let res: String
            if args.count == 1, case let (arg, ty) = args[0], ty == nil {
                res = "\(arg)\(ann(retTy)) => \(body)"
            } else {
                res = "(\(args.map({ (arg, ty) in "\(arg)\(ann(ty))" }).commas()))\(ann(retTy)) => \(body)"
            }
            
            return "\(modifier == .fun ? "" : "\(modifier)") \(res)"
        case let .Call(f, args):
            return "\(f)(\(args.commas()))"
        case let .Block(stmts, ret):
            if let ret {
                if stmts.isEmpty {
                    return "{ \(ret) }"
                } else {
                    return "{\n\(stmts.map(indent).joined(separator: "\n"))\n\(indent(ret))\n}"
                }
            } else {
                return "{\n\(stmts.map(indent).joined(separator: "\n"))\n}"
            }
        case let .If(cond, then, else_) where else_.isEmpty:
            return "if \(cond) {\n\(then.newlines())\n}"
        case let .If(cond, then, else_):
            return "if \(cond) {\n\(then.newlines())\n} else {\n\(else_.newlines())\n}"
        case let .Assignment(lhs, op, rhs):
            return "\(lhs) \(op) \(rhs)"
        case let .Tuple(exprs):
            return "(\(exprs.commas()))"
        case let .UseIn(pat, ty, val, rhs):
            return "use \(pat)\(ann(ty)) = \(val) in \(rhs)"
        case let .Array(elems):
            return "[\(elems.commas())]"
        case let .Record(fields):
            let sep = fields.count > 4 ? ",\n" : ", "
            return "{ \(fields.map({ (field, val) in "\(field): \(val)" }).joined(separator: sep)) }"
        case let .RecordSelect(record, field):
            return "\(record).\(field)"
        case let .Pipeline(arg1, f, remArgs):
            return "\(arg1)->\(f)(\(remArgs.commas()))"
        case let .Raw(js):
            return "raw {\n \(indent(js)) \n}"
        case let .Match(expr, cases):
            return "match \(expr) {\n\(cases.map({ (pat, body) in indent("\(pat) => \(body)") }).joined(separator: "\n"))\n}"
        case let .Variant(nil, variantName, args) where args.isEmpty:
            return variantName
        case let .Variant(typeName, variantName, args) where args.isEmpty:
            return "\(typeName ?? "").\(variantName)"
        case let .Variant(nil, variantName, args):
            return "\(variantName)(\(args.commas()))"
        case let .Variant(typeName?, variantName, args):
            return "\(typeName).\(variantName)(\(args.commas()))"
        case let .MethodCall(subject, method, args):
            return "\(subject).\(method)(\(args.commas()))"
        case let .BuiltInCall(name, args):
            return "@\(name)(\(args.commas()))"
        }
    }
    
    func rewrite(with f: @escaping (Expr) -> Expr) -> Expr {
        func aux(_ expr: Expr) -> Expr {
            let res: Expr
            switch expr {
            case let .Literal(lit):
                res = .Literal(lit)
            case let .UnaryOp(op, expr):
                res = .UnaryOp(op, aux(expr))
            case let .BinaryOp(lhs, op, rhs):
                res = .BinaryOp(aux(lhs), op, aux(rhs))
            case let .Parens(expr):
                res = .Parens(aux(expr))
            case let .Var(name):
                res = .Var(name)
            case let .Fun(modifier, args, retTy, body):
                res = .Fun(modifier: modifier, args: args, retTy: retTy, body: aux(body))
            case let .Call(fun, args):
                res = .Call(f: aux(fun), args: args.map(aux))
            case let .Block(stmts, ret):
                res = .Block(stmts.map({ $0.rewrite(with: f) }), ret: ret.map(aux))
            case let .If(cond, then, else_):
                res = .If(
                    cond: aux(cond),
                    then: then.map({ $0.rewrite(with: f) }),
                    else_: else_.map({ $0.rewrite(with: f) })
                )
            case let .Assignment(lhs, op, rhs):
                res = .Assignment(aux(lhs), op, aux(rhs))
            case let .Tuple(elems):
                res = .Tuple(elems.map(aux))
            case let .UseIn(pat, ty, val, rhs):
                res = .UseIn(pat: pat, ty: ty, val: aux(val), rhs: aux(rhs))
            case let .Array(elems):
                res = .Array(elems.map(aux))
            case let .Record(entries):
                res = .Record(entries.map({ (field, val) in (field, aux(val)) }))
            case let .RecordSelect(lhs, field):
                res = .RecordSelect(aux(lhs), field: field)
            case let .Pipeline(arg1, fun, remArgs):
                res = .Pipeline(arg1: aux(arg1), f: fun, remArgs: remArgs.map(aux))
            case let .Raw(js):
                res = .Raw(js: js)
            case let .Match(subject, cases):
                res = .Match(aux(subject), cases: cases.map({ (pat, body) in (pat, aux(body)) }))
            case let .Variant(typeName, variantName, args):
                res = .Variant(typeName: typeName, variantName: variantName, args: args.map(aux))
            case let .MethodCall(subject, method, args):
                res = .MethodCall(subject: aux(subject), method: method, args: args.map(aux))
            case let .BuiltInCall(name, args):
                res = .BuiltInCall(name, args.map(aux))
            }
            
            return f(res)
        }
        
        return aux(self)
    }
    
    func substitute(mapping: [String:Expr]) -> Expr {
        return rewrite(with: { expr in
            if case let .Var(name) = expr, mapping.keys.contains(name) {
                return mapping[name]!
            }
            
            return expr
        })
    }
}

enum Stmt: CustomStringConvertible {
    case Expr(Expr)
    case Let(mut: Bool, pat: Pattern, ty: Ty?, val: Expr)
    indirect case If(cond: Expr, then: [Stmt], else_: [Stmt]?)
    indirect case While(cond: Expr, body: [Stmt])
    indirect case For(pat: Pattern, iterator: Expr, body: [Stmt])
    case Return(Expr?)
    case Yield(Expr)
    case Break
    case Error(ParserError, span: (Int, Int))

    public var description: String {
        switch self {
        case let .Expr(expr):
            return "\(expr)"
        case let .Let(mut: false, pat, ty, val):
            return "let \(pat)\(ann(ty)) = \(val)"
        case let .Let(mut: true, pat, ty, val):
            return "mut \(pat)\(ann(ty)) = \(val)"
        case let .If(cond, then, nil):
            return "if \(cond) {\n\(then.newlines())\n}"
        case let .If(cond, then, else_?):
            return "if \(cond) {\n\(then.newlines())\n} else {\n\(else_.newlines())\n}"
        case let .While(cond, body):
            return
                "while \(cond) {\n\(body.map(indent).newlines())\n}"
        case let .For(pat, iterator, body):
            return
                "for \(pat) in \(iterator) {\n\(body.map(indent).newlines())\n}"
        case .Return(nil):
            return "return"
        case let .Return(.some(ret)):
            return "return \(ret)"
        case let .Yield(expr):
            return "yield \(expr)"
        case .Break:
            return "break"
        case let .Error(error, span: (start, end)):
            return "Error(\(error), span: (\(start), \(end)))"
        }
    }
    
    func rewrite(with _f: @escaping (Expr) -> Expr) -> Stmt {
        let f = { (expr: Expr) in expr.rewrite(with: _f) }
        
        func aux(_ stmt: Stmt) -> Stmt {
            switch stmt {
            case let .Expr(expr):
                return .Expr(f(expr))
            case let .Let(mut, pat, ty, val):
                return .Let(mut: mut, pat: pat, ty: ty, val: f(val))
            case let .If(cond, then, else_):
                return .If(cond: f(cond), then: then.map(aux), else_: else_.map({ $0.map(aux) }))
            case let .While(cond, body):
                return .While(cond: f(cond), body: body.map(aux))
            case let .For(pat, iterator, body):
                return .For(pat: pat, iterator: f(iterator), body: body.map(aux))
            case let .Return(ret):
                return .Return(ret.map(f))
            case let .Yield(val):
                return .Yield(f(val))
            case .Break:
                return .Break
            case let .Error(err, span):
                return .Error(err, span: span)
            }
        }
        
        return aux(self)
    }
}

enum Decl: CustomStringConvertible {
    case Let(pub: Bool, mut: Bool, pat: Pattern, ty: Ty?, val: Expr)
    case Stmt(Stmt)
    case TypeAlias(pub: Bool, name: String, args: [TyVarId], ty: Ty)
    case Enum(pub: Bool, name: String, args: [TyVarId], variants: [(name: String, ty: Ty?)])
    case Fun(pub: Bool, modifier: FunModifier, name: String, args: [(Pattern, Ty?)], retTy: Ty?, body: Expr)
    case Rewrite(pub: Bool, ruleName: String, args: [String], rhs: Expr)
    case Declare(pub: Bool, name: String, ty: Ty)
    case Import(path: String, members: [String]?)
    case Trait(
        pub: Bool,
        name: String,
        args: [TyVarId],
        methods: [(modifier: FunModifier, name: String, args: [(String, Ty)], ret: Ty)]
    )
    case TraitImpl(
        trait: String,
        args: [Ty],
        methods: [(pub: Bool, modifier: FunModifier, name: String, args: [(Pattern, Ty?)], ret: Ty?, body: Expr)]
    )
    case Error(ParserError, span: (Int, Int))
    
    var description: String {
        switch self {
        case let .Let(pub, mut, pat, ty, val):
            return "pub ".when(pub) + (mut ? "mut" : "let") + " \(pat)\(ann(ty)) = \(val)"
        case let .Stmt(stmt):
            return "\(stmt)"
        case let .TypeAlias(pub, name, [], ty):
            return "pub ".when(pub) + "type \(name) = \(ty)"
        case let .TypeAlias(pub, name, args, ty):
            let argsFmt = args
                .map({ TyVar.showTyVarId($0).lowercased() })
                .commas()
            
            return "pub ".when(pub) + "type \(name)<\(argsFmt)> = \(ty)"
        case let .Enum(pub, name, args, variants):
            let variantsFmt = variants
                .map({ (name, ty) in ty != nil ? "\(name)(\(ty!))" : name })
                .newlines()
            
            if args.isEmpty {
                return "enum \(name) {\n\(indent(variantsFmt))\n}"
            }
            
            let argsFmt = args
                .map({ TyVar.showTyVarId($0).lowercased() })
                .commas()
            
            return "pub ".when(pub) + "enum \(name)<\(argsFmt)> {\n\(variantsFmt)\n}"
        case let .Fun(pub, modifier, name, args, retTy, body):
            let argsFmt = args
                .map({ (pat, ty) in "\(pat)\(ann(ty))" })
                .commas()

            return "pub ".when(pub) + "\(modifier) \(name)(\(argsFmt))\(ann(retTy)) {\n\(indent(body))\n}"
        case let .Rewrite(pub, ruleName, args, rhs):
            return "pub ".when(pub) + "rewrite \(ruleName)(\(args.commas())) -> \(rhs)"
        case let .Declare(pub, name, ty):
            return "pub ".when(pub) + "declare \(name): \(ty)"
        case let .Import(path, members?):
            return "import \"\(path)\" { \(members.commas()) }" 
        case let .Import(path, nil):
            return "import \"\(path)\"" 
        case let .Trait(pub, name, args, members):
            let argsFmt = args
                .map({ TyVar.showTyVarId($0).lowercased() })
                .commas()
            
            let membersFmt = members
                .map({ (modifier, name, args, retTy) in
                    let argsFmt = args
                        .map({ (pat, ty) in "\(pat)\(ann(ty))" })
                        .commas()
                    
                    return "\(modifier) \(name) (\(argsFmt)): \(retTy)"
                })
                .newlines()

            return "pub ".when(pub) + "trait \(name)" + "<\(argsFmt)>".when(!args.isEmpty) +  "{\n\(membersFmt)\n}"
        case let .TraitImpl(trait, args, methods):
            let argsFmt = args
                .map({ $0.description })
                .commas()
            
            let methodsFmt = methods
                .map({ (pub, modifier, name, args, retTy, body) in
                    let argsFmt = args
                        .map({ (pat, ty) in "\(pat)\(ann(ty))" })
                        .commas()
                    
                    return "pub".when(pub) + "\(modifier) \(name) (\(argsFmt))\(ann(retTy)) {\n\(indent(body))\n}"
                })
                .newlines()
            
            return "impl trait" + " \(trait)" + "<\(argsFmt)>".when(!args.isEmpty) + " {\n\(methodsFmt)\n}"
        case let .Error(err, span):
            return "\(err) at \(span)"
        }
    }

    enum Kind {
        case Let, Stmt, TypeAlias, Enum, Fun, Rewrite, Declare, Import, Trait, TraitImpl, Error
    }

    var kind: Kind {
        switch self {
        case .Let: return .Let
        case .Stmt: return .Stmt
        case .TypeAlias: return .TypeAlias
        case .Enum: return .Enum
        case .Fun: return .Fun
        case .Rewrite: return .Rewrite
        case .Declare: return .Declare
        case .Import: return .Import
        case .Trait: return .Trait
        case .TraitImpl: return .TraitImpl
        case .Error: return .Error
        }
    }
}

enum Pattern: CustomStringConvertible {
    case any
case variable(String)
    case literal(Literal)
    indirect case tuple([Pattern])
    indirect case record([(String, Pattern?)])
    indirect case variant(enumName: Ref<String?>, variant: String, [Pattern])
    
    public var description: String {
        switch self {
        case .any:
            return "_"
        case let .variable(name):
            return name
        case let .literal(lit):
            return "\(lit)"
        case let .tuple(patterns):
            return "(\(patterns.map({ p in "\(p)" }).commas()))"
        case let .record(entries):
            return "{ \(entries.map({ (k, p) in p == nil ? k : "\(k): \(p!)" }).commas()) }"
        case let .variant(_, name, patterns):
            if patterns.isEmpty {
                return name
            }
            
            return "\(name)(\(patterns.commas()))"
        }
    }
    
    func rewrite(with f: (Pattern) -> Pattern) -> Pattern {
        func aux(_ pat: Pattern) -> Pattern {
            let res: Pattern
            
            switch pat {
            case .any:
                res = .any
            case let .literal(lit):
                res = .literal(lit)
            case let .variable(name):
                res = .variable(name)
            case let .tuple(patterns):
                res = .tuple(patterns.map(aux))
            case let .record(entries):
                res = .record(entries.map({ (field, pat) in (field, pat.map(aux)) }))
            case let .variant(enumName, variant, patterns):
                res = .variant(enumName: enumName, variant: variant, patterns.map(aux))
            }
            
            return f(res)
        }
        
        return aux(self)
    }
}

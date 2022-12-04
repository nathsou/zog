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

func indent(_ str: CustomStringConvertible) -> String {
    return String(describing: str)
        .split(separator: "\n")
        .map({ s in "    \(s)" })
        .joined(separator: "\n")
}

func ann(_ annotation: Ty?) -> String {
    if let annotation {
        return ": \(annotation)"
    }
    
    return ""
}

indirect enum Expr: CustomStringConvertible {
    case Literal(Literal)
    case UnaryOp(UnaryOperator, Expr)
    case BinaryOp(Expr, BinaryOperator, Expr)
    case Parens(Expr)
    case Var(String)
    case Fun(args: [(Pattern, Ty?)], retTy: Ty?, body: Expr, isIterator: Bool)
    case Call(f: Expr, args: [Expr])
    case Block([Stmt], ret: Expr?)
    case If(cond: Expr, thenExpr: Expr, elseExpr: Expr?)
    case Assignment(Expr, AssignmentOperator, Expr)
    case Tuple([Expr])
    case UseIn(pat: Pattern, ty: Ty?, val: Expr, rhs: Expr)
    case Array([Expr])
    case Record([(String, Expr)])
    case RecordSelect(Expr, field: String)
    case Pipeline(arg1: Expr, f: String, remArgs: [Expr])
    case Raw(js: String)
    case Match(Expr, cases: [(Pattern, Expr)])
    case Variant(typeName: String?, variantName: String, val: Expr?)
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
        case let .Fun(args, retTy, body, isIterator):
            let res: String
            if args.count == 1, case let (arg, ty) = args[0], ty == nil {
                res = "\(arg)\(ann(retTy)) => \(body)"
            } else {
                res = "(\(args.map({ (arg, ty) in "\(arg)\(ann(ty))" }).joined(separator: ", ")))\(ann(retTy)) => \(body)"
            }
            
            if isIterator {
                return "iterator \(res)"
            } else {
                return res
            }
        case let .Call(f, args):
            return "\(f)(\(args.map({ "\($0)" }).joined(separator: ", ")))"
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
        case let .If(cond, thenExpr, nil):
            return "if \(cond) \(thenExpr)"
        case let .If(cond, thenExpr, elseExpr?):
            return "if \(cond) \(thenExpr) else \(elseExpr)"
        case let .Assignment(lhs, op, rhs):
            return "\(lhs) \(op) \(rhs)"
        case let .Tuple(exprs):
            return "(\(exprs.map({ "\($0)" }).joined(separator: ", ")))"
        case let .UseIn(pat, ty, val, rhs):
            return "use \(pat)\(ann(ty)) = \(val) in \(rhs)"
        case let .Array(elems):
            return "[\(elems.map({ "\($0)" }).joined(separator: ", "))]"
        case let .Record(fields):
            let sep = fields.count > 4 ? ",\n" : ", "
            return "{ \(fields.map({ (field, val) in "\(field): \(val)" }).joined(separator: sep)) }"
        case let .RecordSelect(record, field):
            return "\(record).\(field)"
        case let .Pipeline(arg1, f, remArgs):
            return "\(arg1)->\(f)(\(remArgs.map({ "\($0)" }).joined(separator: ", ")))"
        case let .Raw(js):
            return "raw {\n \(indent(js)) \n}"
        case let .Match(expr, cases):
            return "match \(expr) {\n\(cases.map({ (pat, body) in indent("\(pat) => \(body)") }).joined(separator: "\n"))\n}"
        case let .Variant(typeName, variantName, nil):
            return "\(typeName ?? "").\(variantName)"
        case let .Variant(typeName, variantName, val):
            return "\(typeName ?? "").\(variantName) \(val!)"
        case let .BuiltInCall(name, args):
            return "@\(name)(\(args.map({ "\($0)" }).joined(separator: ", "))"
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
            case let .Fun(args, retTy, body, isIterator):
                res = .Fun(args: args, retTy: retTy, body: aux(body), isIterator: isIterator)
            case let .Call(fun, args):
                res = .Call(f: aux(fun), args: args.map(aux))
            case let .Block(stmts, ret):
                res = .Block(stmts.map({ $0.rewrite(with: f) }), ret: ret.map(aux))
            case let .If(cond, thenExpr, elseExpr):
                res = .If(cond: aux(cond), thenExpr: aux(thenExpr), elseExpr: elseExpr.map(aux))
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
            case let .Variant(typeName, variantName, val):
                res = .Variant(typeName: typeName, variantName: variantName, val: val.map(aux))
            case let .BuiltInCall(name, args):
                res = .BuiltInCall(name, args.map(aux))
            }
            
            return f(res)
        }
        
        return aux(self)
    }
    
    func substitute(mapping: [String:Expr]) -> Expr {
        return rewrite(with: { expr in
            switch expr {
            case let .Var(name) where mapping.keys.contains(name):
                return mapping[name]!
            default:
                return expr
            }
        })
    }
}

enum Stmt: CustomStringConvertible {
    case Expr(Expr)
    case Let(mut: Bool, pat: Pattern, ty: Ty?, val: Expr)
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
        case let .While(cond, body):
            return
                "while \(cond) {\n\(body.map(indent).joined(separator: "\n"))\n}"
        case let .For(pat, iterator, body):
            return
                "for \(pat) in \(iterator) {\n\(body.map(indent).joined(separator: "\n"))\n}"
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
    case Stmt(Stmt)
    case TypeAlias(name: String, args: [TyVarId], ty: Ty)
    case Enum(name: String, args: [TyVarId], variants: [(name: String, ty: Ty?)])
    case Rewrite(ruleName: String, args: [String], rhs: Expr)
    
    var description: String {
        switch self {
        case let .Stmt(stmt):
            return "\(stmt)"
        case let .TypeAlias(name, [], ty):
            return "type \(name) = \(ty)"
        case let .TypeAlias(name, args, ty):
            let argsFmt = args
                .map({ TyVar.showTyVarId($0).lowercased() })
                .joined(separator: ", ")
            
            return "type \(name)<\(argsFmt)> = \(ty)"
        case let .Enum(name, args, variants):
            let variantsFmt = variants
                .map({ (name, ty) in ty != nil ? "\(name) \(ty!)" : name })
                .joined(separator: "\n")
            
            if args.isEmpty {
                return "enum \(name) {\n\(variantsFmt)\n}"
            }
            
            let argsFmt = args
                .map({ TyVar.showTyVarId($0).lowercased() })
                .joined(separator: ", ")
            
            return "enum \(name)<\(argsFmt)> {\n\(variantsFmt)\n}"
        case let .Rewrite(ruleName, args, rhs):
            return "rewrite \(ruleName)(\(args.joined(separator: ", "))) -> \(rhs)"
        }
    }
}

enum Pattern: CustomStringConvertible {
    case any
    case variable(String)
    case literal(Literal)
    indirect case tuple([Pattern])
    indirect case record([(String, Pattern?)])
    indirect case variant(enumName: Ref<String?>, variant: String, Pattern?)
    
    public var description: String {
        switch self {
        case .any:
            return "_"
        case let .variable(name):
            return name
        case let .literal(lit):
            return "\(lit)"
        case let .tuple(patterns):
            return "(\(patterns.map({ p in "\(p)" }).joined(separator: ", ")))"
        case let .record(entries):
            return "{ \(entries.map({ (k, p) in p == nil ? k : "\(k): \(p!)" }).joined(separator: ", ")) }"
        case let .variant(_, name, pat):
            if let pat {
                return "\(name) \(pat)"
            } else {
                return name
            }
        }
    }
}

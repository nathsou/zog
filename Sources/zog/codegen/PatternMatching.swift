//
//  PatternMatching.swift
//  
//
//  Created by nathan on 26/11/2022.
//

import Foundation

// Based on "Compiling Pattern Matching to Good Decision Trees" by Luc Maranget
// http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf

enum PathKey: CustomStringConvertible {
    case index(Int)
    case field(String)
    
    var description: String {
        switch self {
        case .index(let index): return "\(index)"
        case .field(let field): return "\(field)"
        }
    }
}

typealias Path = [PathKey]

extension CoreExpr {
    func at(path: Path) -> CoreExpr {
        var expr = self
        
        for key in path {
            switch key {
            case let .index(index):
                if case let .const("Tuple", tys) = expr.ty.deref() {
                    expr = .ArraySubscript(
                        expr,
                        index: .Literal(.num(Float64(index)), ty: .num),
                        ty: tys[index]
                    )
                }
            case let .field(field):
                if case .record(let row) = expr.ty.deref() {
                    // TODO: optimize
                    let entries = row.entries()
                    if let index = entries.firstIndex(where: { (k, _) in k == field }) {
                        let (key, ty) = entries[index]
                        expr = .RecordSelect(expr, field: key, ty: ty)
                    }
                }
            }
        }
        
        return expr
    }
}

enum Ctor: Hashable {
    case tag(String)
    case tuple
    case record([String])
    case literal(Literal)
    
    func at(_ i: Int) -> PathKey {
        switch self {
        case let .record(fields):
            return .field(fields[i])
        default:
            return .index(i)
        }
    }
}

enum SimplifiedPattern {
    case any
    case variable(String)
    indirect case const(Ctor, [SimplifiedPattern])
    
    func isInfallible() -> Bool {
        switch self {
        case .any, .variable(_):
            return true
        default:
            return false
        }
    }
}

extension CorePattern {
    fileprivate func simplified() -> SimplifiedPattern {
        func aux(_ pat: CorePattern) -> SimplifiedPattern {
            switch pat {
            case .any:
                return .any
            case .variable(let name):
                return .variable(name)
            case .literal(let lit):
                return .const(.literal(lit), [])
            case .tuple(let args):
                return .const(.tuple, args.enumerated().map({ (i, arg) in aux(arg) }))
            case .record(let entries):
                var subPatterns = [SimplifiedPattern]()
                
                for (field, p) in entries {
                    if let p {
                        subPatterns.append(aux(p))
                    } else {
                        subPatterns.append(.variable(field))
                    }
                }
                
                return .const(.record(entries.map({ (field, _) in field })), subPatterns)
            }
        }
        
        return aux(self)
    }

    func vars() -> [String:Path] {
        var vars = [String:Path]()
        func aux(_ pat: CorePattern, _ path: Path) {
            switch pat {
            case .any:
                break
            case .variable(let name):
                vars[name] = path
            case .literal(_):
                break
            case .tuple(let args):
                for (i, arg) in args.enumerated() {
                    aux(arg, path + [.index(i)])
                }
            case .record(let entries):
                for (field, p) in entries {
                    if let p {
                        aux(p, path + [.field(field)])
                    } else {
                        vars[field] = path
                    }
                }
            }
        }
        
        aux(self, [])

        return vars
    }
}

fileprivate struct SimplifiedTy {
    let ctor: String
    let args: [SimplifiedTy]
    
    fileprivate func isExhaustive(_ ctors: [Ctor]) -> Bool {
        return false
    }
}

extension Ty {
    fileprivate func simplified() -> SimplifiedTy {
        switch self {
        case let .variable(v):
            switch v.ref {
            case let .link(to):
                return to.simplified()
            default:
                fatalError("encountered type variable in Ty.simplified()")
            }
        case let .const(ctor, args):
            return SimplifiedTy(ctor: ctor, args: args.map({ $0.simplified() }))
        case let .fun(args, ret):
            return SimplifiedTy(ctor: "=>", args: (args + [ret]).map({ $0.simplified() }))
        case let .record(row):
            return SimplifiedTy(
                ctor: "record",
                args: row.entries().map({ (_, ty) in ty.simplified() })
            )
        }
    }
}

fileprivate func heads(_ patterns: [SimplifiedPattern]) -> [Ctor:Int] {
    var heads = [Ctor:Int]()
    
    for p in patterns {
        switch p {
        case .any, .variable(_):
            break
        case let .const(ctor, args):
            heads[ctor] = args.count
        }
    }
    
    return heads
}

enum DecisionTree: CustomStringConvertible {
    case fail
    case leaf(rowIndex: Int, action: CoreExpr)
    indirect case switch_(
        occurrence: Path,
        cases: [(ctor: Ctor, dt: DecisionTree)],
        defaultCase: DecisionTree?
    )
    
    static func from(exprTy: Ty, cases: [(CorePattern, CoreExpr)]) -> DecisionTree {
        var clauseMatrix = ClauseMatrix.from(
            type: exprTy.simplified(),
            cases: cases.map({ (p, a) in (p.simplified(), a) })
        )
        
        return clauseMatrix.compile()
    }
    
    var description: String {
        switch self {
        case .fail: return "fail"
        case let .leaf(_, action): return "\(action)"
        case let .switch_(occurrence, cases, defaultCase):
            var casesFmt = cases.map({ (ctor, dt) in "\(ctor) => \(dt)" })
            if let defaultCase {
                casesFmt.append("_ => \(defaultCase)")
            }
            
            return "switch \(occurrence) {\n \(casesFmt.map(indent).joined(separator: "\n")) \n}"
        }
    }
}

fileprivate struct ClauseMatrix {
    typealias Row = [SimplifiedPattern]
    typealias Col = [SimplifiedPattern]
    
    let dims: (rows: Int, cols: Int)
    var types: [SimplifiedTy]
    var patterns: [Row]
    let actions: [(rowIndex: Int, action: CoreExpr)]
    
    init(types: [SimplifiedTy], patterns: [Row], actions: [(rowIndex: Int, action: CoreExpr)]) {
        self.dims = (rows: patterns.count, cols: patterns.first?.count ?? 0)
        self.types = types
        self.patterns = patterns
        self.actions = actions
    }
    
    static func from(type: SimplifiedTy, cases: [(SimplifiedPattern, CoreExpr)]) -> Self {
        return ClauseMatrix(
            types: [type],
            patterns: cases.map({ (p, _) in [p] }),
            actions: cases.enumerated().map({ (index, arg1) in let (_, a) = arg1; return (index, a) })
        )
    }
    
    private static func specializedRow(row: Row, ctor: Ctor, arity: Int) -> Row? {
        let (p, ps) = (row.first, row[1...])
        
        switch p {
        case .any, .variable(_):
            return [_](repeating: .any, count: arity) + ps
        case let .const(c, args) where c == ctor:
            return args + ps
        default:
            return nil
        }
    }
    
    private static func defaultedRow(_ row: Row) -> Row? {
        switch row.first {
        case .any, .variable(_):
            return Array(row[1...])
        default:
            return nil
        }
    }
    
    private func specialized(ctor: Ctor, args: [SimplifiedTy]) -> ClauseMatrix {
        let arity = args.count
        let patterns = self.patterns.map({ row in
            ClauseMatrix.specializedRow(row: row, ctor: ctor, arity: arity)
            
        })
        
        let actions = zip(patterns, self.actions.map({ ($0.rowIndex, $0.action) }))
            .filter({ (p, _) in p != nil })
            .map({ (_, a) in a })
        
        let types = args + self.types[1...]
        
        return ClauseMatrix.init(
            types: types,
            patterns: patterns.compactMap({ $0 }),
            actions: actions
        )
    }
    
    private func defaulted() -> ClauseMatrix {
        let patterns = self.patterns.map(ClauseMatrix.defaultedRow)
        let actions = zip(patterns, self.actions.map({ ($0.rowIndex, $0.action) }))
            .filter({ (p, _) in p != nil })
            .map({ (_, a) in a })
        
        return ClauseMatrix.init(
            types: Array(types[1...]),
            patterns: patterns.compactMap({ $0 }),
            actions: actions
        )
    }
    
    func getColumn(_ j: Int) -> Col {
        return patterns.map({ row in row[j] })
    }
    
    private func selectColumn() -> Int {
        for j in 0..<dims.cols {
            if getColumn(j).contains(where: { p in p.isInfallible() }) {
                return j
            }
        }
        
        return 0
    }
    
    private mutating func swapColumns(_ j1: Int, _ j2: Int) {
        for i in 0..<dims.rows {
            patterns[i].swapAt(j1, j2)
        }
        
        types.swapAt(j1, j2)
    }
    
    mutating func compile() -> DecisionTree {
        func aux(matrix: inout ClauseMatrix, occurrences: inout [Path]) -> DecisionTree {
            if matrix.dims.rows == 0 {
                return .fail
            }
            
            if matrix.dims.cols == 0 || matrix.patterns[0].allSatisfy({ p in p.isInfallible() }) {
                let (index, action) = matrix.actions[0]
                return .leaf(rowIndex: index, action: action)
            }
            
            let columnIndex = matrix.selectColumn()
            
            if columnIndex != 0 {
                occurrences.swapAt(0, columnIndex)
                matrix.swapColumns(0, columnIndex)
            }
            
            let col = matrix.getColumn(0)
            let hds = heads(col)
            let isExhaustive = matrix.types[0].isExhaustive(Array(hds.keys))
            var cases = [(ctor: Ctor, dt: DecisionTree)]()
            
            for (ctor, arity) in hds {
                var o1 = (0..<arity).map({ i in occurrences[0] + [ctor.at(i)] }) + occurrences[1...]
                var S = matrix.specialized(ctor: ctor, args: matrix.types[0].args)
                let Ak = aux(matrix: &S, occurrences: &o1)
                cases.append((ctor, Ak))
            }
            
            var defaultCase: DecisionTree? = nil
            
            if !isExhaustive {
                var D = matrix.defaulted()
                var occurrences = Array(occurrences[1...])
                defaultCase = aux(matrix: &D, occurrences: &occurrences)
            }
            
            return .switch_(
                occurrence: occurrences[0],
                cases: cases,
                defaultCase: defaultCase
            )
        }
        
        var occurrences = [Path]([[]])
        return aux(matrix: &self, occurrences: &occurrences)
    }
}

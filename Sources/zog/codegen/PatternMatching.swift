//
//  PatternMatching.swift
//  
//
//  Created by nathan on 26/11/2022.
//

import Foundation

// Based on "Compiling Pattern Matching to Good Decision Trees" by Luc Maranget
// http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf

enum PathKey {
    case index(Int)
    case field(String)
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
                    let entries = row.sortedEntries()
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

fileprivate enum SimplifiedPattern {
    case any
    indirect case const(Ctor, [SimplifiedPattern])
    
    func isAny() -> Bool {
        if case .any = self {
            return true
        }
        
        return false
    }
}

extension CorePattern {
    fileprivate func simplified() -> SimplifiedPattern {
        switch self {
        case .any, .variable(_):
            return .any
        case .literal(let lit):
            return .const(.literal(lit), [])
        case .tuple(let args):
            return .const(.tuple, args.map({ $0.simplified() }))
        case .record(let entries):
            var subPatterns = [SimplifiedPattern]()
            
            for (_, p) in entries {
                if let p {
                    subPatterns.append(p.simplified())
                } else {
                    subPatterns.append(.any)
                }
            }
            
            return .const(.record(entries.map({ (k, _) in k })), subPatterns)
        }
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
                args: row.sortedEntries().map({ (_, ty) in ty.simplified() })
            )
        }
    }
}

fileprivate func heads(_ patterns: [SimplifiedPattern]) -> [Ctor:Int] {
    var heads = [Ctor:Int]()
    
    for p in patterns {
        switch p {
        case .any:
            break
        case let .const(ctor, args):
            heads[ctor] = args.count
        }
    }
    
    return heads
}

enum DecisionTree: CustomStringConvertible {
    case leaf(CoreExpr)
    indirect case switch_(
        occurrence: Path,
        cases: [(ctor: Ctor, dt: DecisionTree)],
        defaultCase: DecisionTree?
    )
    
    static func from(exprTy: Ty, cases: [(CorePattern, CoreExpr)]) -> DecisionTree {
        var clauseMatrix = ClauseMatrix.init(
            type: exprTy.simplified(),
            cases: cases.map({ (p, a) in (p.simplified(), a) })
        )
        
        return clauseMatrix.compile()
    }
    
    var description: String {
        switch self {
        case let .leaf(action): return "\(action)"
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
    let actions: [CoreExpr]
    
    init(dims: (rows: Int, cols: Int), types: [SimplifiedTy], patterns: [Row], actions: [CoreExpr]) {
        self.dims = dims
        self.types = types
        self.patterns = patterns
        self.actions = actions
    }
    
    init(type: SimplifiedTy, cases: [(SimplifiedPattern, CoreExpr)]) {
        dims = (rows: cases.count, cols: 1)
        types = [type]
        patterns = cases.map({ (p, _) in [p] })
        actions = cases.map({ (_, a) in a })
    }
    
    private static func specializedRow(row: Row, ctor: Ctor, arity: Int) -> Row? {
        let (p, ps) = (row.first, row[1...])
        
        switch p {
        case .any:
            return [_](repeating: .any, count: arity) + ps
        case let .const(c, args) where c == ctor:
            return args + ps
        default:
            return nil
        }
    }
    
    private static func defaultedRow(_ row: Row) -> Row? {
        if case .any = row.first {
            return Array(row[1...])
        }
        
        return nil
    }
    
    private func specialized(ctor: Ctor, args: [SimplifiedTy]) -> ClauseMatrix {
        let arity = args.count
        let patterns = self.patterns.map({ row in ClauseMatrix.specializedRow(row: row, ctor: ctor, arity: arity) })
        let actions = zip(patterns, self.actions).filter({ (p, _) in p != nil }).map({ (_, a) in a })
        let types = args + self.types[1...]
        
        return ClauseMatrix.init(
            dims: (rows: actions.count, cols: arity + self.dims.cols - 1),
            types: types,
            patterns: patterns.compactMap({ $0 }),
            actions: actions
        )
    }
    
    private func defaulted() -> ClauseMatrix {
        let patterns = self.patterns.map(ClauseMatrix.defaultedRow)
        let actions = zip(patterns, self.actions).filter({ (p, _) in p != nil }).map({ (_, a) in a })
        
        return ClauseMatrix.init(
            dims: (rows: actions.count, cols: dims.cols - 1),
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
            if getColumn(j).contains(where: { p in p.isAny() }) {
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
                fatalError("empty clause matrix")
            }
            
            if matrix.dims.cols == 0 || matrix.patterns[0].allSatisfy({ p in p.isAny() }) {
                return .leaf(matrix.actions[0])
            }
            
            let columnIndex = matrix.selectColumn()
            
            if columnIndex != 0 {
                occurrences.swapAt(0, columnIndex)
                matrix.swapColumns(0, columnIndex)
            }
            
            let col = matrix.getColumn(0)
            let hds = heads(col)
            let isExhaustive = matrix.types[0].isExhaustive(Array(hds.keys))
            var cases = [(Ctor, DecisionTree)]()
            
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
            
            return .switch_(occurrence: occurrences[0], cases: cases, defaultCase: defaultCase)
        }
        
        var occurrences = [Path]([[]])
        return aux(matrix: &self, occurrences: &occurrences)
    }
}

//
//  PatternMatching.swift
//  
//
//  Created by nathan on 26/11/2022.
//

import Foundation

// Based on "Compiling Pattern Matching to Good Decision Trees" by Luc Maranget
// http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf

typealias Path = [Int]

extension CoreExpr {
    func at(path: Path) -> CoreExpr {
        var expr = self
        
        for index in path {
            switch expr.ty.deref() {
            case let .const("Tuple", tys):
                expr = .ArraySubscript(
                    expr,
                    index: .Literal(.num(Float64(index)), ty: .num),
                    ty: tys[index]
                )
            case let .record(row):
                let (key, ty) = row.entries()[index]
                expr = .RecordSelect(expr, field: key, ty: ty)
            default:
                return expr
            }
        }
        
        return expr
    }
}

enum Ctor: Hashable {
    case tag(String)
    case tuple
    case record
    case literal(Literal)
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
    fileprivate func simplified(ty: Ty) -> SimplifiedPattern {
        func aux(_ pat: CorePattern, _ ty: Ty) -> SimplifiedPattern {
            switch pat {
            case .any:
                return .any
            case .variable(let name):
                return .variable(name)
            case .literal(let lit):
                return .const(.literal(lit), [])
            case .tuple(let args):
                if case let .const("Tuple", tys) = ty.deref() {
                    return .const(.tuple, args.enumerated().map({ (i, arg) in aux(arg, tys[i]) }))
                } else {
                    fatalError("expected tuple pattern to be associated with a tuple type")
                }
            case .record(let entries):
                // rewrite partial record patterns into
                // complete ordered patterns (like tuples)

                if case let .record(row) = ty.deref() {
                    var presentEntries = [String:CorePattern?]()
                    for (field, pat) in entries {
                        presentEntries[field] = pat
                    }

                    var subPatterns = [SimplifiedPattern]()
                
                    for (field, fieldTy) in row.entries() {
                        if let pat = presentEntries[field] {
                            if let pat {
                                subPatterns.append(aux(pat, fieldTy))
                            } else {
                                subPatterns.append(.variable(field))
                            }
                        } else {
                            subPatterns.append(.any)
                        }
                    }
                    
                    return .const(.record, subPatterns)
                } else {
                    fatalError("expected record pattern to be associated with a record type")
                }
            }
        }
        
        return aux(self, ty)
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
                    aux(arg, path + [i])
                }
            case .record(let entries):
                let sortedEntries = entries.sorted(by: { $0.field < $1.field })
                for (index, (field, p)) in sortedEntries.enumerated() {
                    if let p {
                        aux(p, path + [index])
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
        case let .enum_(variants):
            return SimplifiedTy(
                ctor: "enum",
                args: variants.map({ (name, ty) in
                    if let ty {
                        return SimplifiedTy(ctor: name, args: [ty.simplified()])
                    } else {
                        return SimplifiedTy(ctor: name, args: [])
                    }
                })
            )
        }
    }
}

fileprivate func heads(_ patterns: [SimplifiedPattern]) -> [(Ctor, (arity: Int, rowIndex: Int))] {
    var heads = [Ctor:(arity: Int, rowIndex: Int)]()
    
    for (index, pattern) in patterns.enumerated() {
        switch pattern {
        case .any, .variable(_):
            break
        case let .const(ctor, args):
            heads[ctor] = (args.count, index)
        }
    }
    
    return heads.sorted(by: { $0.value.rowIndex < $1.value.rowIndex })
}

enum DecisionTree: CustomStringConvertible {
    case fail
    case leaf(rowIndex: Int, action: CoreExpr)
    indirect case switch_(
        occurrence: Path,
        cases: [(ctor: Ctor, rowIndex: Int, dt: DecisionTree)],
        defaultCase: DecisionTree?
    )
    
    static func from(exprTy: Ty, cases: [(CorePattern, CoreExpr)]) -> DecisionTree {
        var clauseMatrix = ClauseMatrix.from(
            type: exprTy.simplified(),
            cases: cases.map({ (p, a) in (p.simplified(ty: exprTy), a) })
        )
        
        return clauseMatrix.compile()
    }
    
    var description: String {
        switch self {
        case .fail: return "fail"
        case let .leaf(_, action): return "\(action)"
        case let .switch_(occurrence, cases, defaultCase):
            var casesFmt = cases.map({ (ctor, rowIndex, dt) in "\(ctor): \(rowIndex) => \(dt)" })
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
            actions: cases.enumerated().map({ ($0, $1.1) })
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
            let isExhaustive = matrix.types[0].isExhaustive(hds.map({ $0.0 }))
            var cases = [(ctor: Ctor, rowIndex: Int, dt: DecisionTree)]()
            
            for (ctor, (arity: arity, rowIndex: rowIndex)) in hds {                
                var o1 = (0..<arity).map({ i in occurrences[0] + [i] }) + occurrences[1...]
                var S = matrix.specialized(ctor: ctor, args: matrix.types[0].args)
                let Ak = aux(matrix: &S, occurrences: &o1)
                cases.append((ctor, rowIndex, Ak))
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

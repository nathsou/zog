
struct BuiltIn {
    static func codegen(name: String, args: [CoreExpr], ctx: CoreContext) throws -> JSExpr {
        switch name {
        case "type":
            let ZogType = Ty.const("Type", [])
            
            func typeVariant(_ name: String, val: CoreExpr? = nil) -> CoreExpr {
                return .Variant(
                    enumName: Ref("Type"),
                    variantName: name,
                    val: val,
                    ty: ZogType
                )
            }
            
            var tyVarIdMapping = [TyVarId:String]()
            
            func tyVarName(_ id: TyVarId) -> String {
                if let name = tyVarIdMapping[id] {
                    return name
                }
                
                let count = tyVarIdMapping.count
                let name = TyVar.showId(UInt(count))
                tyVarIdMapping[id] = name
                return name
            }
            
            func typeOf(_ ty: Ty) -> CoreExpr {
                switch ty {
                case let .variable(v):
                    switch v.ref {
                    case let .generic(id):
                        return typeVariant(
                            "Variable",
                            val: .Literal(.str(tyVarName(id).lowercased()), ty: .str)
                        )
                    case let .link(to):
                        return typeOf(to)
                    case let .unbound(id, _, _):
                        return typeVariant(
                            "Variable",
                            val: .Literal(.str(tyVarName(id)), ty: .str)
                        )
                    }
                case .const("unit", []):
                    return typeVariant("Unit")
                case .const("bool", []):
                    return typeVariant("Bool")
                case .const("str", []):
                    return typeVariant("Str")
                case .const("num", []):
                    return typeVariant("Num")
                case let .const("Tuple", args):
                    return typeVariant("Tuple", val: .Array(args.map(typeOf), ty: .array(ZogType)))
                case let .const("Array", args) where args.count == 1:
                    return typeVariant("Array", val: typeOf(args[0]))
                case let .const(name, args):
                    return typeVariant(
                        "Alias",
                        val: .Record([
                            ("name", .Literal(.str(name), ty: .str)),
                            ("args", .Tuple(args.map(typeOf), ty: .tuple(args)))
                        ], ty: .record(Row.from(entries: [("name", .str), ("args", .tuple(args))])))
                    )
                case let .fun(args, ret):
                    let argsTy = Ty.array(ZogType)
                    return typeVariant(
                        "Function",
                        val: .Record([
                            ("args", .Array(args.map(typeOf), ty: argsTy)),
                            ("returns", typeOf(ret))
                        ], ty: .record(Row.from(entries: [("args", argsTy), ("returns", ZogType)])))
                    )
                case let .record(row):
                    let entries = row.entries(sorted: true)
                    let entryType = Ty.record(Row.from(entries: [("field", .str), ("type", ZogType)]))
                    return typeVariant(
                        "Record",
                        val: .Array(entries.map({ (field, ty) in
                                .Record([
                                    ("field", .Literal(.str(field), ty: .str)),
                                    ("type", typeOf(ty))
                                ], ty: .array(entryType))
                        }), ty: ZogType)
                    )
                }
            }
            
            return try typeOf(args[0].ty).codegen(ctx)
        case "jsTypeOf":
            return .typeof(try args[0].codegen(ctx))
        default:
            fatalError("invalid built in function: impossible -> checked in infer")
        }
    }
}
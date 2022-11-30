import XCTest

@testable import zog

final class zogTests: XCTestCase {
    let env = TypeEnv()
    
    func testUnifySameConstsWithNoArgs() {
        XCTAssertNoThrow(try env.unify(.num, .num))
    }

    func testUnifyConstsWithArgs() {
        let s = Ty.const("A", [.num, .const("B", [])])
        let t = Ty.const("A", [.num, .const("B", [])])

        XCTAssertNoThrow(try env.unify(s, t))
    }

    func testUnifyDiffConstsWithArgs() {
        let s = Ty.const("A", [.num, .const("B", [])])
        let t = Ty.const("A", [.const("B", []), .num])

        XCTAssertThrowsError(try env.unify(s, t))
    }

    func testUnifyUnboundVar() {
        let tv = Ref(TyVar.fresh(level: 0))
        let s = Ty.variable(tv)
        let t = Ty.num

        XCTAssertNoThrow(try env.unify(s, t))
        XCTAssertEqual(tv.ref, .link(t))
    }

    func testUnifyLinkedVar() {
        let s = Ty.variable(Ref(.link(.variable(Ref(.link(.num))))))
        let t = Ty.num

        XCTAssertNoThrow(try env.unify(s, t))
    }

    func testUnifyDiffLinkedVar() {
        let s = Ty.variable(Ref(.link(.variable(Ref(.link(.bool))))))
        let t = Ty.num

        XCTAssertThrowsError(try env.unify(s, t))
    }
    
    func testUnifyRecursiveType() {
        let v = Ty.variable(Ref(.unbound(id: 0, level: 0)))
        let ty = Ty.const("Rec", [v])
        
        XCTAssertThrowsError(try env.unify(v, ty))
    }
}

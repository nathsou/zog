import XCTest

@testable import zog

final class zogTests: XCTestCase {
    func testUnifySameConstsWithNoArgs() {
        XCTAssertNil(unify(.num, .num))
    }

    func testUnifyConstsWithArgs() {
        let s = Ty.const("A", [.num, .const("B", [])])
        let t = Ty.const("A", [.num, .const("B", [])])

        XCTAssertNil(unify(s, t))
    }

    func testUnifyDiffConstsWithArgs() {
        let s = Ty.const("A", [.num, .const("B", [])])
        let t = Ty.const("A", [.const("B", []), .num])

        XCTAssertEqual(unify(s, t), .cannotUnify(.const("B", []), .num))
    }

    func testUnifyUnboundVar() {
        let tv = Ref(TyVar.fresh(level: 0))
        let s = Ty.variable(tv)
        let t = Ty.num

        XCTAssertNil(unify(s, t))
        XCTAssertEqual(tv.ref, .link(t))
    }

    func testUnifyLinkedVar() {
        let s = Ty.variable(Ref(.link(.variable(Ref(.link(.num))))))
        let t = Ty.num

        XCTAssertNil(unify(s, t))
    }

    func testUnifyDiffLinkedVar() {
        let s = Ty.variable(Ref(.link(.variable(Ref(.link(.bool))))))
        let t = Ty.num

        XCTAssertNotNil(unify(s, t))
    }
    
    func testUnifyRecursiveType() {
        let v = Ty.variable(Ref(.unbound(id: 0, level: 0)))
        let s = Ty.const("Rec", [v])
        
        
        XCTAssertNotNil(unify(v, s))
    }
}

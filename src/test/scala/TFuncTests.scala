class IP_Nich_PS2_Tests extends org.scalatest.funsuite.AnyFunSuite {
    def checkEqual(testF: TFunc, checkF: Array[Boolean] => Boolean, resStr: String): Unit = {
        val n = testF.inputLength
        val input = new Array[Boolean](n)
        for (i <- 0 until n) {
            assert(testF.fromArray(input) === checkF(input))
        }
        assert(testF.toString() === resStr)
    }

    test("P") {
        checkEqual(TFunc("P"), a => a(0), "P")
    }

    test("P & Q") {
        checkEqual(TFunc("P & Q"), a => a(0) && a(1), "(P ∧ Q)")
    }

    test("P -> Q & R") {
        checkEqual(TFunc("P -> Q & R"), a => !a(0) || (a(1) && a(2)), "(P → (Q ∧ R))")
    }

    test("(P -> Q) & R") {
        checkEqual(TFunc("(P -> Q) & R"), a => (!a(0) || a(1)) && a(2), "((P → Q) ∧ R)")
    }

    test("~P & Q") {
        checkEqual(TFunc("~P & Q"), a => !a(0) && a(1), "(¬P ∧ Q)")
    }

    test("P & Q | R") {
        checkEqual(TFunc("P & Q | R"), a => (a(0) && a(1)) || a(2), "((P ∧ Q) ∨ R)")
    }

    test("(P -> (Q & R)) | P") {
        checkEqual(TFunc("(P -> (Q & R)) | P"), a => true, "((P → (Q ∧ R)) ∨ P)")
    }

    test("(P <-> Q & ~R)") {
        checkEqual(TFunc("(P <-> Q & ~R)"), a => a(0) == (a(1) && !a(2)), "(P ↔ (Q ∧ ¬R))")
    }

    test("T") {
        checkEqual(TFunc("T"), _ => true, "T")
    }

    test("F") {
        checkEqual(TFunc("F"), _ => false, "F")
    }
}
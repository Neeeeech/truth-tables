class IP_Nich_PS2_Tests extends org.scalatest.funsuite.AnyFunSuite {
    def checkEqual(testF: TFunc, checkF: Array[Boolean] => Boolean): Unit = {
        val n = testF.inputLength
        val input = new Array[Boolean](n)
        for (i <- 0 until n) {
            assert(testF.fromArray(input) === checkF(input))
        }
    }

    test("P") {
        checkEqual(TFunc("P"), a => a(0))
    }

    test("P & Q") {
        checkEqual(TFunc("P & Q"), a => a(0) && a(1))
    }

    test("P -> Q & R") {
        checkEqual(TFunc("P -> Q & R"), a => !a(0) || (a(1) && a(2)))
    }

    test("(P -> Q) & R") {
        checkEqual(TFunc("(P -> Q) & R"), a => (!a(0) || a(1)) && a(2))
    }

    test("~P & Q") {
        checkEqual(TFunc("~P & Q"), a => !a(0) && a(1))
    }

    test("P & Q | R") {
        checkEqual(TFunc("P & Q | R"), a => (a(0) && a(1)) || a(2))
    }

    test("(P -> (Q & R)) | P") {
        checkEqual(TFunc("(P -> (Q & R)) | P"), a => true)
    }

    test("(P <-> Q & ~R)") {
        checkEqual(TFunc("(P <-> Q & ~R)"), a => a(0) == (a(1) && !a(2)))
    }

    test("T") {
        checkEqual(TFunc("T"), _ => true)
    }

    test("F") {
        checkEqual(TFunc("F"), _ => false)
    }
}
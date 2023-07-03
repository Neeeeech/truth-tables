/** Truth function, generated either from composing smaller truth functions or from parsing a suitable string. */
class TFunc(val f: (Array[Boolean]) => Boolean, private val inputLength: Int) {
    /** To directly access the function
      *
      * @param a input booleans
      * @return result of truth function
      */
    def apply(a: Boolean*): Boolean = {
        require(a.length == inputLength, f"Expected ${inputLength} inputs but ${a.length} were given")
        f(a.toArray)
    }

    /** Generates truth table for the truth function
      *
      * @return truth table as a string to be printed
      */
    def table: String = ???
}

object TFunc {
    /** Constructor with string input
      *
      * @param s input string
      */
    def apply(s: String) = { val (f, len) = parse(s); new TFunc(f, len) }

    /** Turns propositional logic sentence into truth function.
      *
      * @param s input string
      * @return (corresponding truth function, number of inputs)
      */
    private def parse(s: String): ((Array[Boolean]) => Boolean, Int) = ???
}
/** Truth function, generated either from composing smaller truth functions or from parsing a suitable string. */
class TFunc(val f: (Array[Boolean]) => Boolean, private val senLets: Array[Char], private val inputLength: Int) {
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
    class UnrecognisedCharException extends RuntimeException
    private val operators = Array[Char]('T', 'F', '~', '&', '|', '→', '↔', '(', ')')

    /** Constructor with string input
      *
      * @param s input string
      */
    /*** FIX THIS HERE IT'S NOT RIGHT ***/
    def apply(s: String) = { val (f, len) = parse(s); new TFunc(f, Array[Char](), len) }

    /** Turns propositional logic sentence into truth function.
      *
      * @param s input string
      * @return (corresponding truth function, number of inputs)
      */
    private def parse(s: String): ((Array[Boolean]) => Boolean, Int) = ???

    /** Cleans the string
      * i.e. removes spaces, turns -> and <-> to → and ↔) and returns the sentence letters
      * 
      * @param s input string
      * @return (cleaned string, sentence letters
      */
    def clean(s: String): (String, Array[Char]) = {
        var (i, res, n, senLets, letterCount) = (0, List[Char](), s.length, List[Char](), 0)

        // check each token
        while (i < n) {
            val currToken: Char = s(i)
            if (currToken != ' ') { 
                // we don't care about spaces
                if (operators.contains(currToken)) {
                    // we don't need to change this token
                    res = currToken :: res
                } else if (currToken == '-' && i < s.length - 1 && s(i+1) == '>') {
                    // ->
                    res = '→' :: res; i += 1
                } else if (currToken == '<' && i < s.length - 2 && s(i+1) == '-' && s(i+2) == '>') {
                    // <->
                    res = '↔' :: res; i += 2
                } else if (65 <= currToken.toInt && currToken.toInt <= 122) {
                    // sentence letter
                    if (!senLets.contains(currToken)) senLets = currToken :: senLets
                    res = currToken :: res
                } else {
                    // unrecognised character
                    throw new UnrecognisedCharException
                }
            }

            i += 1
        }
        (res.reverse.mkString, senLets.reverse.toArray)
    }
}
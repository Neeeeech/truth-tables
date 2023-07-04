import scala.collection.mutable.Stack
/** Truth function, generated either from composing smaller truth functions or from parsing a suitable string. */
class TFunc(private val f: (Array[Boolean]) => Boolean, private val senLets: Array[Char], resStr: String) {
    val inputLength = senLets.length
    /** To directly access the function
      *
      * @param a input booleans
      * @return result of truth function
      */
    def apply(a: Boolean*): Boolean = {
        require(a.length == inputLength, f"Expected ${inputLength} inputs but ${a.length} were given")
        f(a.toArray)
    }

    def fromArray(a: Array[Boolean]) = {
        require(a.length == inputLength, f"Expected ${inputLength} inputs but ${a.length} were given")
        f(a)
    }

    override def toString(): String = resStr

    /** Generates truth table for the truth function
      *
      * @return truth table as a string to be printed
      */
    def table: String = ???
}

object TFunc {
    /*** Constants ***/
    /** operator characters for reference */
    private val operators = Array[Char]('~', '&', '|', '→', '↔', 'T', 'F', '(', ')')
    /** array of operator class factory functions */
    private val opClasses = Array[() => Operator](
        () => new NotOp, () => new AndOp, () => new OrOp, () => new ImpliesOp, () => new IffOp
    )
    /** truth function that always returns true */
    private val tautFunc = (_: Array[Boolean]) => true
    /** truth function that always returns false */
    private val contFunc = (_: Array[Boolean]) => false



    /*** Classes ***/
    class UnrecognisedCharException extends RuntimeException
    class MissingCharException(s: String) extends RuntimeException(s)

    /** Standard operators, with "compose" method to combine functions (if applicable) */
    abstract class Operator { def >=[T <: Operator](that: T): Boolean = true }
    class Bracket extends Operator

    abstract class UnOp extends Operator {
        def compose(f: Array[Boolean] => Boolean): Array[Boolean] => Boolean

        def composeName(s: String): String = f"${this}${s}"
    }

    class NotOp extends UnOp {
        override def compose(f: Array[Boolean] => Boolean): Array[Boolean] => Boolean = 
            (a: Array[Boolean]) => !f(a)
        // not binds the strongest, and so does not need to override >=
        override def toString(): String = "¬"
    }

    abstract class BinOp extends Operator { 
        def compose(f1: Array[Boolean] => Boolean, f2: Array[Boolean] => Boolean): Array[Boolean] => Boolean

        def composeName(s1: String, s2: String): String = f"(${s1} ${this} ${s2})"
    }

    class AndOp extends BinOp {
        override def compose(f1: Array[Boolean] => Boolean, f2: Array[Boolean] => Boolean): Array[Boolean] => Boolean = 
            (a: Array[Boolean]) => f1(a) && f2(a)
        
        override def >=[T <: Operator](that: T): Boolean = that match {
            case _: NotOp => false
            case _ => true
        }

        override def toString(): String = "∧"
    }

    class OrOp extends BinOp {
        override def compose(f1: Array[Boolean] => Boolean, f2: Array[Boolean] => Boolean): Array[Boolean] => Boolean = 
            (a: Array[Boolean]) => f1(a) || f2(a)
        
        override def >=[T <: Operator](that: T): Boolean = that match {
            case _: NotOp => false
            case _ => true
        }

        override def toString(): String = "∨"
    }

    class ImpliesOp extends BinOp {
        override def compose(f1: Array[Boolean] => Boolean, f2: Array[Boolean] => Boolean): Array[Boolean] => Boolean = 
            (a: Array[Boolean]) => (!f1(a)) || f2(a)
        
        override def >=[T <: Operator](that: T): Boolean = that match {
            case _: IffOp => true
            case _: ImpliesOp => true
            case _ => false
        }

        override def toString(): String = "→"
    }

    class IffOp extends BinOp {
        override def compose(f1: Array[Boolean] => Boolean, f2: Array[Boolean] => Boolean): Array[Boolean] => Boolean = 
            (a: Array[Boolean]) => f1(a) == f2(a)
        
        override def >=[T <: Operator](that: T): Boolean = that match {
            case _: IffOp => true
            case _: ImpliesOp => true
            case _ => false
        }

        override def toString(): String = "↔"
    }



    /*** Methods ***/
    /** Constructor with string input
      *
      * @param s input string
      */
    def apply(s: String) = { 
        val (cleanS, senLets) = clean(s)
        val (f, resStr) = parse(cleanS, senLets)
        new TFunc(f, senLets, resStr) 
    }

    /** Turns propositional logic sentence into truth function.
      * 
      * uses a modified version of Dijkstra's Shunting-yard algorithm
      * pre: assumes the input string s is clean
      * 
      * @param s input string
      * @return (corresponding truth function, number of inputs)
      */
    private def parse(s: String, senLets: Array[Char]): (Array[Boolean] => Boolean, String) = {
        val nInputs = senLets.length
        val fnStack = new Stack[(Array[Boolean] => Boolean, String)]()
        val opStack = new Stack[Operator]()
        
        // parsing through the whole string
        for (c <- s) {
            if (isSenLet(c)) { // Sentence Letter
                fnStack.push((giveSLFunc(c, senLets), c.toString()))
            } else if (c == 'T') { // Truth
                fnStack.push((tautFunc, "T"))
            } else if (c == 'F') { // Falsity
                fnStack.push((contFunc, "F"))
            } else if (c == '(') { // Open Bracket
                opStack.push(new Bracket)
            } else if (c == ')') { // Close Bracket
                var working = true
                while (working) {
                    if (opStack.isEmpty) throw new MissingCharException(f"No '(' to match the ')'")
                    opStack.head match {
                        case _: Bracket  => { working = false; opStack.pop() }
                        case o: Operator => applyTop(fnStack, opStack)
                    }
                }
            } else if (operators.contains(c)) { // all other operators
                val newOp: Operator = opClasses(operators.indexOf(c))()
                if (opStack.nonEmpty && opStack.head >= newOp) {
                    opStack.head match {
                        case _: Bracket  => {}
                        case o: Operator => applyTop(fnStack, opStack)
                    }
                }
                opStack.push(newOp)
            } else {
                throw new UnrecognisedCharException
            }
        }

        // cleaning up the whatever's left in the stacks
        while (opStack.nonEmpty) opStack.head match {
            case _: Bracket  => throw new MissingCharException("Missing a ')'")
            case o: Operator => applyTop(fnStack, opStack)
        }

        if (fnStack.length != 1) throw new MissingCharException("Expecting more operators")
        fnStack.head
    }

    /** Cleans the string
      * i.e. removes spaces, turns -> and <-> to → and ↔) and returns the sentence letters
      * 
      * @param s input string
      * @return (cleaned string, sentence letters)
      */
    private def clean(s: String): (String, Array[Char]) = {
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
                } else if (isSenLet(currToken)) {
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

    /** Checks whether a token is a sentence letter
      *
      * @param c the token
      * @return whether c is a sentence letter
      */
    private def isSenLet(c: Char): Boolean = { val i = c.toInt; 65 <= i && i <= 122 && c != 'T' && c != 'F' }

    /** Gives sentence letter c's corresponding truth function
      *
      * pre: c is in senLets
      * 
      * @param c the sentence letter
      * @param senLets array of all the sentence letters in order
      * @return truth function corresponding to c (i.e. returns ith input where senLets(i) == c)
      */
    private def giveSLFunc(c: Char, senLets: Array[Char]): (Array[Boolean]) => Boolean =
        (a: Array[Boolean]) => a(senLets.indexOf(c))

    /** Applies the top op of opStack to the appropriate functions in fnStack
      * 
      * NB does nothing if the top operator is a bracket
      *
      * @param fnStack function stack
      * @param opStack operator stack
      */
    private def applyTop(fnStack: Stack[(Array[Boolean] => Boolean, String)], opStack: Stack[Operator]): Unit = opStack.head match {
        case _: Bracket => {}
        case o: UnOp => {
            opStack.pop()
            if (fnStack.isEmpty) 
                throw new MissingCharException(f"${o} missing an argument")
            val (fn, name) = fnStack.pop()
            fnStack.push((o.compose(fn), o.composeName(name)))
        }
        case o: BinOp => {
            opStack.pop()
            if (fnStack.length < 2) 
                throw new MissingCharException(f"${o} missing an argument")
            val (f2, s2) = fnStack.pop()
            val (f1, s1) = fnStack.pop()
            fnStack.push((o.compose(f1, f2), o.composeName(s1, s2)))
        }
    }
}
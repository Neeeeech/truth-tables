-- exercises I used for testing taken from https://www.logicmatters.net/wp-content/uploads/2020/06/Exercises_solutions_12.pdf

-- valid inputs (words are backwards bc when parsing it's easier to add to front rather than back)
validWords :: [Char] -> Char
validWords "T" = 'T'
validWords "&" = '&'
validWords "F" = 'F'
validWords "v" = 'v'
validWords "V" = 'v'
validWords "~" = '!'
validWords ">-" = '→'
validWords "!" = '!'
validWords "dna" = '&'
validWords "ro" = 'v'
validWords "eutr" = 'T'
validWords "eslaf" = 'F'
validWords "ton" = '!'
validWords "seilpmi" = '→'
validWords "p" = 'p'
validWords "q" = 'q'
validWords "r" = 'r'
validWords "s" = 's'
validWords "(" = '('
validWords ")" = ')'
validWords _ = ' '

parseInner :: [Char] -> [Char] -> [Char]
parseInner (x:xs) acc
    | x == ' '       = parseInner xs acc
    | nextSym == ' ' = parseInner xs (x:acc)
    | otherwise      = nextSym : parseInner xs []
    where nextSym = validWords (x:acc)
parseInner [] _ = []

parse :: [Char] -> [Char]
parse all
    | output == "" = error "Something's wrong, empty string returned"
    | otherwise    = output
    where output = parseInner all []

-- assume given a sentence in good form, returns value given arguemnts in [Bool]
process :: [Char] -> [Bool] -> Bool
-- base cases
process "p" inputs = head inputs
process "q" inputs = inputs !! 1
process "r" inputs = inputs !! 2
process "s" inputs = inputs !! 3
process "T" _ = True
process "F" _ = False
-- checks if the whole thing is encased in a big bracket; if so, removes it
process all@('(':xs) inputs
    | checkParAll xs 1 = process (init xs) inputs
    | otherwise        = giveBiOp biOp (process phrase1 inputs) (process phrase2 inputs)
    where [phrase1, biOp, phrase2] = splitBiOp all 0
-- checks for double negatives
process ('!':'!':xs) inputs = process xs inputs
-- makes sure a ! only applies to !x, otherwise lets splitBiOp process it
process ['!',x] inputs = not $ process [x] inputs
process all@('!':'(':xs) inputs
    | checkParAll xs 1 = not $ process ('(':xs) inputs
    | otherwise        = giveBiOp biOp (process phrase1 inputs) (process phrase2 inputs)
    where [phrase1, biOp, phrase2] = splitBiOp all 0
-- at this point it must be a binary operation, so splits it into its componenets
process all inputs = giveBiOp biOp (process phrase1 inputs) (process phrase2 inputs)
    where [phrase1, biOp, phrase2] = splitBiOp all 0

-- called when '(' at the beginning of string
-- check to see if the whole string is bracketed
-- pass in string without the first '(' and n=1
checkParAll :: [Char] -> Int -> Bool
checkParAll [] 0 = True
checkParAll [] _ = error "You've bracketed something wrong"
checkParAll _ 0 = False
checkParAll ('(':xs) n = checkParAll xs (n+1)
checkParAll (')':xs) n = checkParAll xs (n-1)
checkParAll (x:xs) n = checkParAll xs n

-- gives the corresponding binary operation
giveBiOp :: [Char] -> Bool -> Bool -> Bool
giveBiOp "&" = (&&)
giveBiOp "v" = (||)
giveBiOp "→" = \x y -> not x || (x && y)
giveBiOp _ = error "unrecognised binary op"

-- takes string with form "phrase1 biOp phrase2" and splits into ["phrase1", "biOp", "phrase2"]
splitBiOp :: [Char] -> Int -> [[Char]]
-- first deals with parentheses
splitBiOp (')':xs) 1 = ")":splitBiOp xs 0
splitBiOp ('(':xs) n = addToHStr '(' $ splitBiOp xs (n+1)
splitBiOp (')':xs) n = addToHStr ')' $ splitBiOp xs (n-1)
-- makes sure a ! sticks to the next bits no matter what
splitBiOp ('!':xs) n = addToHStr '!' $ splitBiOp xs n
-- adds anything else 
splitBiOp (x:xs) 0 = [x] : splitBiOp xs 0
splitBiOp (x:xs) n = addToHStr x $ splitBiOp xs n
-- ends the recursion
splitBiOp [] _ = []

-- adds Char to the head of the first string in list of strings --
addToHStr :: Char -> [[Char]] -> [[Char]]
addToHStr x (y:ys) = (x:y) : ys
addToHStr x [] = [[x]]

-- gives a truth table for n vars
truthTable :: Int -> ([Bool] -> Bool) -> [Bool]
truthTable n f = map f $ predTable n

-- gives the combination of Bools for n vars
predTable :: Int -> [[Bool]]
predTable 1 = [[True], [False]]
predTable n = map (True:) prevPredTable ++ map (False:) prevPredTable
    where prevPredTable = predTable (n-1)

-- gives a truthtable given the number of vars (up to 4) and the sentence
tableFromSentence :: Int -> [Char] -> [Bool]
tableFromSentence n sentence = truthTable n $ process $ parse sentence
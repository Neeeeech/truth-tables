-- assume given a sentence in good form, returns value given arguemnts in [Bool]
process :: [Char] -> [Bool] -> Bool
-- base cases
process "p" inputs = head inputs
process "q" inputs = inputs !! 1
process "r" inputs = inputs !! 2
process "T" _ = True
process "F" _ = False
-- makes sure a ! only applies to !x, otherwise lets splitBiOp process it
process ['!',x] inputs = not $ process [x] inputs
process all inputs = giveBiOp biOp (process phrase1 inputs) (process phrase2 inputs)
    where [phrase1, biOp, phrase2] = splitBiOp all 0

-- gives the corresponding binary operation
giveBiOp :: [Char] -> Bool -> Bool -> Bool
giveBiOp "&" = (&&)
giveBiOp "v" = (||)
giveBiOp "→" = \x y -> not x || (x && y)
giveBiOp _ = error "unrecognised binary op"

-- takes string with form "phrase1 biOp phrase2" and splits into ["phrase1", "biOp", "phrase2"]
splitBiOp :: [Char] -> Int -> [[Char]]
-- first deals with parentheses
splitBiOp ('(':xs) 0 = splitBiOp xs 1
splitBiOp (')':xs) 1 = "":splitBiOp xs 0
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
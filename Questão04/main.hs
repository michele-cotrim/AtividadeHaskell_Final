import Dicionario

main = do
    fileName <- getLine
    contents <- readFile fileName
    -- split by empty space
    let splitWords = words contents
    -- binary tree dictionary to count occurrences
    let dict = addToDict splitWords
    -- list all pairs in dictionary
    let keyValuePairs = ordsim dict
    let header = "### word - count ###\n"
    let result = printToTerminal keyValuePairs
    -- save to file
    writeFile "word_count.txt" (header ++ result)
    return ()

-- Add all words to the binary tree dictionary
addToDict :: [String] -> Dicionario
addToDict x = addToDict' x novo

-- Internal implementation of addToDict
addToDict' :: [String] -> Dicionario -> Dicionario
addToDict' [] curDict = curDict
addToDict' (x : xs) curDict = addToDict' xs (colocar x curDict)

-- Converts pair of values to printable string
pairToString :: (String, Int) -> String
pairToString (a, b) = a ++ " - " ++ (show b)

-- Printing functions
printToTerminal :: [(String, Int)] -> String
printToTerminal x = printToTerminal' x ""

printToTerminal' :: [(String, Int)] -> String -> String
printToTerminal' [] curString = curString
printToTerminal' (pair : rest) curString = printToTerminal' rest (curString ++ (pairToString pair) ++ "\n")
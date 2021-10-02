import Dicionario

main = do
    contents <- readFile "input.txt"
    let splitWords = words contents
    let count = countStrings splitWords
    let dict = addToDict splitWords
    let keyValuePairs = ordsim dict
    let result = printToTerminal keyValuePairs
    putStrLn "### word - count ###\n"
    putStrLn result
    return ()


countStrings :: [String] -> Int
countStrings [] = 0
countStrings (x : xs) = 1 + countStrings xs

addToDict :: [String] -> Dicionario
addToDict x = addToDict' x novo

addToDict' :: [String] -> Dicionario -> Dicionario
addToDict' [] curDict = curDict
addToDict' (x : xs) curDict = addToDict' xs (colocar x curDict)

printToTerminal :: [(String, Int)] -> String
printToTerminal x = printToTerminal' x ""

printToTerminal' :: [(String, Int)] -> String -> String
printToTerminal' [] curString = curString
printToTerminal' (pair : rest) curString = printToTerminal' rest (curString ++ (pairToString pair) ++ "\n")

pairToString :: (String, Int) -> String
pairToString (a, b) = a ++ " - " ++ (show b)
-- pairToString pair = (fst pair) ++ " - " ++ (show (snd pair)) 
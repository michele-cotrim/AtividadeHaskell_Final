import Dicionario

main = do
    contents <- readFile "input.txt"
    let splitWords = words contents
    let count = countStrings splitWords
    let dict = addToDict splitWords
    let result = ordsim dict
    return result

countStrings :: [String] -> Int
countStrings [] = 0
countStrings (x : xs) = 1 + countStrings xs

addToDict :: [String] -> Dicionario
addToDict x = addToDict' x novo

addToDict' :: [String] -> Dicionario -> Dicionario
addToDict' [] curDict = curDict
addToDict' (x : xs) curDict = addToDict' xs (colocar x curDict)
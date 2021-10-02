main = do
    contents <- readFile "input.txt"
    let splitWords = words contents
    let count = countStrings splitWords
    return count

countStrings :: [String] -> Int
countStrings [] = 0
countStrings (x : xs) = 1 + countStrings xs
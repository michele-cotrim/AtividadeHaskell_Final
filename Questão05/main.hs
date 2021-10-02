data Formula = Var String | Not Formula | And Formula Formula | Or Formula Formula

main = do
    -- fileName <- getLine
    -- contents <- readFile fileName
    contents <- readFile "input.txt"
    return contents
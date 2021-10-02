data Formula = Var String | Not Formula | And Formula Formula | Or Formula Formula deriving (Read, Show)

main = do
    -- fileName <- getLine
    -- contents <- readFile fileName
    contents <- readFile "input.txt"
    let formula :: Formula
        formula = read contents
    return (diversao formula)


toBinary :: Int -> String
toBinary 0 = "0"
toBinary 1 = "1"
toBinary x = toBinary (div x 2) ++ show (mod x 2)

diversao :: Formula -> Int
diversao x = 666

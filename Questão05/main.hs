import Data.List

data Formula = Var String | Not Formula | And Formula Formula | Or Formula Formula deriving (Read, Show)

main = do
    -- fileName <- getLine
    -- contents <- readFile fileName
    contents <- readFile "input.txt"
    let formula = (read contents :: Formula)
    let uniqueVars = vars formula
    let map = createMapping uniqueVars
    return (map)


toBinary :: Int -> String
toBinary 0 = "0"
toBinary 1 = "1"
toBinary x = toBinary (div x 2) ++ show (mod x 2)

type Name = String
vars :: Formula -> [Name] 
vars (Var x) = [x]
vars (Not x) = vars x
vars (And x y) = vars x `union` vars y
vars (Or x y) = vars x `union` vars y

type Attr = (Name, Bool)
createMapping :: [Name] -> [(Name, Bool)]
createMapping x = mapping' x []

mapping' :: [Name] -> [(Name, Bool)] -> [(Name, Bool)]
mapping' [] curList = curList
mapping' (x : xs) curList = mapping' xs (curList ++ [(x, False)])


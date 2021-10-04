import Data.List

data Formula = Var String | Not Formula | And Formula Formula | Or Formula Formula deriving (Read, Show)

main = do
    -- fileName <- getLine
    -- contents <- readFile fileName
    contents <- readFile "input2.txt"
    let formula = (read contents)
    let uniqueVars = vars formula
    let map = createMapping uniqueVars
    let allMappings = generateAll uniqueVars []
    putStrLn ((printHeader uniqueVars) ++ (printFinalFormula formula))
    putStrLn (evaluateAll formula allMappings)
    return ()

type Name = String
vars :: Formula -> [Name] 
vars (Var x) = [x]
vars (Not x) = vars x
vars (And x y) = vars x `union` vars y
vars (Or x y) = vars x `union` vars y

evaluate :: Formula -> Mapping -> Bool
evaluate (Var x) m = getValue x m
evaluate (Not x) m = not (evaluate x m)
evaluate (And x y) m = and [(evaluate x m), (evaluate y m)]
evaluate (Or x y) m = or [(evaluate x m), (evaluate y m)]

prettyPrint :: Bool -> String
prettyPrint True = "V"
prettyPrint False = "F"

printFinalFormula :: Formula -> String
printFinalFormula (Var x) = show (x)
printFinalFormula (Not x) = "not (" ++ (printFinalFormula x) ++ ")"
printFinalFormula (And x y) = "(" ++ printFinalFormula x ++ " and " ++ printFinalFormula y ++ ")"
printFinalFormula (Or x y) = "(" ++ printFinalFormula x ++ " or " ++ printFinalFormula y ++ ")"

evaluateAll :: Formula -> [Mapping] -> String
evaluateAll f [] = ""
evaluateAll f (m : ms) = printFormula (vars f) m ++ prettyPrint (evaluate f m) ++ "\n" ++ evaluateAll f ms

printHeader :: [Name] -> String
printHeader [] = ""
printHeader (x : xs) = show x ++ " " ++ printHeader xs

printFormula :: [Name] -> Mapping -> String
printFormula [] m = ""
printFormula (n : ns) m = prettyPrint (getValue n m) ++ " " ++ printFormula ns m


createMapping :: [Name] -> [(Name, Bool)]
createMapping x = mapping' x []

type Mapping = [(Name, Bool)]
mapping' :: [Name] -> Mapping -> Mapping
mapping' [] curList = curList
mapping' (x : xs) curList = mapping' xs (curList ++ [(x, True)])

getValue :: Name -> Mapping -> Bool
getValue name ((n, v) : m) = if n == name then v   
                            else getValue name m 

generateAll :: [Name] -> Mapping -> [Mapping]
generateAll [] m = [m]
generateAll (x : xs) m = (generateAll xs ([(x, True)] ++ m)) `union` (generateAll xs ([(x, False)] ++ m))
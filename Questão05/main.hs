import Data.List

data Formula = Var String | Not Formula | And Formula Formula | Or Formula Formula deriving (Read, Show)

type Name = String
-- Mapping is a list of attributions (variable -> value)
-- e.g [('A', True), ('B', False), ('C', False)]
type Mapping = [(Name, Bool)]

main = do
    -- fileName <- getLine
    -- contents <- readFile fileName
    contents <- readFile "input.txt"
    let formula = (read contents) -- read Formula directly from input
    let uniqueVars = getUniqueVars formula -- remove duplicate variables
    -- generate all possible value attributions for each variable
    let allMappings = generateAll uniqueVars [] 
    putStrLn ((printHeader uniqueVars) ++ (printFinalFormula formula))
    putStrLn (evaluateAll formula allMappings)
    putStrLn ("Is tautology? " ++ show (isTautology formula allMappings))
    return ()

-- Remove duplicate variables, as we don't want them showing multiple times at 
-- the truth table
getUniqueVars :: Formula -> [Name] 
getUniqueVars (Var x) = [x]
getUniqueVars (Not x) = getUniqueVars x
getUniqueVars (And x y) = getUniqueVars x `union` getUniqueVars y
getUniqueVars (Or x y) = getUniqueVars x `union` getUniqueVars y

-- Evaluate formula recursively to True or False
-- Makes use of a specific mapping (variable, attributed value)
-- e.g (A and B)
-- given A -> False,
--       B -> True evaluates to False
evaluate :: Formula -> Mapping -> Bool
evaluate (Var x) m = getValue x m
evaluate (Not x) m = not (evaluate x m)
evaluate (And x y) m = and [(evaluate x m), (evaluate y m)]
evaluate (Or x y) m = or [(evaluate x m), (evaluate y m)]

-- Wrapper to evaluate formulas given multiple mappings
evaluateAll :: Formula -> [Mapping] -> String
evaluateAll f [] = ""
evaluateAll f (m : ms) = printFormula (getUniqueVars f) m ++ prettyPrint (evaluate f m) ++ "\n" ++ evaluateAll f ms

isTautology :: Formula -> [Mapping] -> Bool
isTautology f [] = True
isTautology f (m : ms) = and [(evaluate f m), (isTautology f ms)]

-- Get value of specific variable in a given mapping
getValue :: Name -> Mapping -> Bool
getValue name ((n, v) : m) = if n == name then v   
                            else getValue name m 

-- Recursively generates all possible 2^N mappings, in lexicographical order
generateAll :: [Name] -> Mapping -> [Mapping]
generateAll [] m = [m]
generateAll (x : xs) m = (generateAll xs ([(x, True)] ++ m)) `union` (generateAll xs ([(x, False)] ++ m))

-- Printing functions
prettyPrint :: Bool -> String
prettyPrint True = "V"
prettyPrint False = "F"

printHeader :: [Name] -> String
printHeader [] = ""
printHeader (x : xs) = show x ++ " " ++ printHeader xs

printFormula :: [Name] -> Mapping -> String
printFormula [] m = ""
printFormula (n : ns) m = prettyPrint (getValue n m) ++ " " ++ printFormula ns m

printFinalFormula :: Formula -> String
printFinalFormula (Var x) = show (x)
printFinalFormula (Not x) = "not (" ++ (printFinalFormula x) ++ ")"
printFinalFormula (And x y) = "(" ++ printFinalFormula x ++ " and " ++ printFinalFormula y ++ ")"
printFinalFormula (Or x y) = "(" ++ printFinalFormula x ++ " or " ++ printFinalFormula y ++ ")"
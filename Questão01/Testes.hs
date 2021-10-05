module Testes where

assertTrue :: Bool -> String -> IO()
assertTrue x claim = if x
                       then putStrLn $ " [OK] " ++ claim
                       else putStrLn $ " [ERRO] " ++ claim


assertFalse :: Bool -> String -> IO()
assertFalse x claim = assertTrue (not x) claim


assertEqual :: (Eq a, Show a) => a -> a -> IO()
assertEqual x y = if x == y
                    then putStrLn $ " [OK] " ++ (show x) ++ " igual " ++ (show y)
                    else putStrLn $ " [ERRO] " ++ (show x) ++ " diferente " ++ (show y)
module Stack (Stack,push, pop, top, 
empty, isEmpty, stackNew, peek) where


stackNew :: Stack a
stackNew = Stk[]

data Stack a = Stk [a] -- representação usando listas


push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

peek :: Stack a -> a
peek (Stk (x:s)) = x

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Sintaxe invalida"

top :: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False
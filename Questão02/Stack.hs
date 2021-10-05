module Stack (Stack(Stk),push, pop, 
empty, isEmpty, stackNew, peek) where


stackNew :: Stack a
stackNew = Stk[]

data Stack a = Stk [a] -- representação usando listas


push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

peek :: Stack a -> a
peek (Stk (x:s)) = x
peek _ = error "Vazio"

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"


empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False
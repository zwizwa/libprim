
-- parameter stack
data Stack = Empty 
           | Push Datum Stack 
           deriving (Show)

-- data residing on parameter stack
data Datum = Number Int 
           | Stack Stack 
           | Code Sub 
           deriving (Show)

-- primitive functions
data Prim  = Dup 
           | Drop 
           deriving (Show)

-- evaluation result
data Value = Error [Char] 
           | Success Stack 
           deriving (Show)

-- subroutine 
data Sub   = Run 
           | Prim Prim 
           | Quote Datum 
           | Seq Sub Sub 
           deriving (Show)

-- continuation
data Cont  = Done 
           | Frame Sub Cont 
           deriving (Show)

-- interpreter state
data State = Halt Value 
           | State Stack Cont 
           deriving (Show)
    
        
lit n = Quote $ Number n
quot sub = Quote $ Code sub

-- code primitives
-- apply (Dup, Push d stk) = Success $ Push $ d Push $ d stk
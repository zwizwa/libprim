
data Stack = Empty | Push Datum Stack deriving (Show)
data Datum = Number Int | Stack Stack deriving (Show)
data Prim  = Minus | Plus deriving (Show)

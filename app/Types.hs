module Types(MonOp(..), BinOp(..)) where 


data MonOp = Neg | Not deriving Show
data BinOp = Mul | Div | Plus | Minus | Lt | Leq | Equal | Neq | Gt | Geq deriving Show
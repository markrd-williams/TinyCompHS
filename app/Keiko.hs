module Keiko (Code) where 

type Symbol = String

data Op = Plus | Minus | Times | Div | Mod | Eq | Lt | Gt | Leq | Geq | Neq | And | Or 

data Code 
  = Const Int 
  | LDGW Symbol 
  | STGW Symbol 
  | Monop Op 
  | Binop Op 
  | Jump Integer
  | JumpC Op Integer
  | Label Integer
  
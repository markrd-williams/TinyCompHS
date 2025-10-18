module TinyLang(Stmt (..), Expr (..), MonOp(..), BinOp(..), Name) where

type Name = String

data Stmt =
    Empty
  | Assign Name Expr
  | Print Expr
  | If Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  deriving (Show)

data Expr where
  Var :: Name -> Expr
  Const :: Int -> Expr
  Monop :: MonOp -> Expr -> Expr
  Binop :: BinOp -> Expr -> Expr -> Expr
  PTrue :: Expr 
  PFalse :: Expr 
  deriving (Show)

data MonOp = Neg | Not deriving Show
data BinOp = Mul | Div | Plus | Minus | Lt | Leq | Equal | Neq | Gt | Geq deriving Show


module TinyLang(Stmt (..), Expr (..), Name, eval) where

import Types
import Control.Monad (foldM)

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



--Below is a little interpereter for the language!
eval :: [(Name, Value)] -> [Stmt] -> IO [(Name, Value)]
eval = foldM evalStmt

data Value
  = VInt Int
  | VBool Bool
  deriving Show

evalExpr :: [(Name, Value)] ->  Expr -> Value
evalExpr con (Var n) =
  case lookup n con of
    Just val -> val
    Nothing  -> error $ "Variable " <> n <> " not defined"
evalExpr _ (Const n) = VInt n
evalExpr con (Monop Neg e1) =
  case evalExpr con e1 of
    VInt i -> VInt (-i)
    _ -> error "Type error: Expected a integer"
evalExpr con (Monop Not e1) =
  case evalExpr con e1 of
    VBool b -> VBool (not b)
    _ -> error "Type error: Expected a boolean"
evalExpr con (Binop Mul e1 e2) = doIntOp (*) (evalExpr con e1) (evalExpr con e2)
evalExpr con (Binop Div e1 e2) = doIntOp div (evalExpr con e1) (evalExpr con e2)
evalExpr con (Binop Plus e1 e2) = doIntOp (+) (evalExpr con e1) (evalExpr con e2)
evalExpr con (Binop Minus e1 e2) = doIntOp (-) (evalExpr con e1) (evalExpr con e2)
evalExpr con (Binop Lt e1 e2) = doCompOp (<) (evalExpr con e1) (evalExpr con e2)
evalExpr con (Binop Leq e1 e2) = doCompOp (<=) (evalExpr con e1) (evalExpr con e2)
evalExpr con (Binop Equal e1 e2) = doCompOp (==) (evalExpr con e1) (evalExpr con e2)
evalExpr con (Binop Neq e1 e2) = doCompOp (/=) (evalExpr con e1) (evalExpr con e2)
evalExpr con (Binop Gt e1 e2) = doCompOp (>) (evalExpr con e1) (evalExpr con e2)
evalExpr con (Binop Geq e1 e2) = doCompOp (>=) (evalExpr con e1) (evalExpr con e2)
evalExpr _ PTrue = VBool True
evalExpr _ PFalse = VBool False

doIntOp :: (Int -> Int -> Int) -> Value -> Value -> Value
doIntOp f (VInt a) (VInt b) = VInt (f a b)
doIntOp _ _ _ = error "Type error! Expected integers"

doCompOp :: (forall a . Ord a => a -> a -> Bool) -> Value -> Value -> Value
doCompOp f (VInt a) (VInt b) = VBool (f a b)
doCompOp f (VBool a) (VBool b) = VBool (f a b)
doCompOp _ _ _ = error "Type error! Comparison has different types"

-- data BinOp = Mul | Div | Plus | Minus | Lt | Leq | Equal | Neq | Gt | Geq deriving Show

update :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
update n v [] = [(n,v)]
update n v ((n',v'):xs)
  | n == n'   = (n',v):xs
  | otherwise = (n',v'):update n v xs

evalStmt :: [(Name, Value)] -> Stmt  -> IO [(Name, Value)]
evalStmt con Empty              = return con
evalStmt con (Assign n e)       = return $ update n (evalExpr con e ) con
evalStmt con (Print e) = do
  let v = evalExpr con e
  print v
  return con
evalStmt con (If e s1 s2) =
  case evalExpr con e of
    VBool True ->
      eval con s1
    VBool False ->
      eval con s2
    _ -> error "Expected boolean"
evalStmt con (While e s) = evalWhile con e s

evalWhile :: [(Name, Value)] -> Expr -> [Stmt] -> IO [(Name, Value)]
evalWhile con e s =
  case evalExpr con e of
    VBool True -> do
      con' <- eval con s
      evalWhile con' e s
    VBool False ->
      return con
    _ -> error "Expected boolean"
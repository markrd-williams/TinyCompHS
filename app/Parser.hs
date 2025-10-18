{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser(pStmts) where

import TinyLang
import Data.Text (Text)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Data.Void (Void)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
      space1
      (L.skipLineComment "//")
      (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pName :: Parser Name
pName = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
-- Expr = Expr + Exp1 | Expr - Expr1 | Expr1
-- Exp1 = Exp1 * Exp 
-- Exp1 -> Exp1 * Exp1 


pVal :: Parser Expr
pVal = pBool <|> pInt <|> pVar  <?> "const"

pVar :: Parser Expr
pVar = Var <$> pName

pInt :: Parser Expr
pInt = Const <$> lexeme L.decimal

pBool :: Parser Expr
pBool =  (lexeme "true" >> pure PTrue)
     <|> (lexeme "false" >> pure PFalse)


pExpr :: Parser Expr
pExpr = try pOrdL <|> pExpr1  
pExpr1 :: Parser Expr
pExpr1 = try pAddL <|> pExpr2
pExpr2 :: Parser Expr 
pExpr2 = try pMulL <|> pExpr3
pExpr3 :: Parser Expr 
pExpr3 = try pVal <|> parens pExpr 


pOrdL :: Parser Expr
pOrdL = do
  e1 <- pExpr1
  op <- choice [ symbol "<=" >> pure (Binop Leq)
               , symbol "<>" >> pure (Binop Neq)
               , symbol ">=" >> pure (Binop Geq)
               , symbol "<"  >> pure (Binop Lt)
               , symbol "=" >> pure (Binop Equal)
               , symbol ">"  >> pure (Binop Gt) ]
  e2 <- pExpr
  return $ op e1 e2

pAddL :: Parser Expr 
pAddL = do 
  e1 <- pExpr2
  op <- choice [ symbol "+"  >> pure (Binop Plus)
               , symbol "-" >> pure (Binop Minus) ]
  e2 <- pExpr1
  return $ op e1 e2 


pMulL :: Parser Expr 
pMulL = do 
  e1 <- pExpr3
  op <- choice [ symbol "*"  >> pure (Binop Mul)
               , symbol "/" >> pure (Binop Div)]
  e2 <- pExpr2
  return $ op e1 e2

pWhile :: Parser Stmt
pWhile = do
  _ <- lexeme "while"
  x <- pExpr
  st <- between (symbol "{") (symbol "}") pStmts
  return $ While x st

pIf :: Parser Stmt
pIf = do
  _ <- lexeme "if"
  x <- pExpr
  st <- between (symbol "{") (symbol "}") pStmts
  st2 <- (lexeme "else" >> between (symbol "{") (symbol "}") pStmts) <|> pure []
  return $ If x st st2

pAss :: Parser Stmt
pAss = do
  x <- pName
  _ <- symbol ":="
  e <- pExpr 
  _ <- symbol ";"
  return $ Assign x e

pStmts :: Parser [Stmt]
pStmts = many pStmt

pPrint :: Parser Stmt
pPrint = lexeme "print" >> between (symbol "(") (symbol ")") (Print <$> pExpr) <* symbol ";"

pStmt :: Parser Stmt
pStmt = choice [ pWhile
               , pIf
               ,  pPrint
               , pAss 
               ] 
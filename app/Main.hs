{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Main where
import System.Environment (getArgs)
import Data.Text.IO (hGetContents)
import Text.Megaparsec (parseTest, parse, errorBundlePretty)
import Parser (pStmts)
import TinyLang
import System.IO (openFile, IOMode (ReadMode))

main :: IO ()
main = do
  args <-getArgs
  let filePath = head args
  file <- openFile filePath ReadMode
  fileTxt <- hGetContents file
  case parse  pStmts  filePath fileTxt of 
    Left e -> putStrLn $ errorBundlePretty e 
    Right s ->  eval [] s >> return () 

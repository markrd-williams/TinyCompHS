module Main where
import System.Environment (getArgs)
import Data.Text.IO (hGetContents)
import Text.Megaparsec (parseTest)
import Parser (pStmts)
import System.IO (openFile, IOMode (ReadMode))

main :: IO ()
main = do
  args <-getArgs
  let filePath = head args
  file <- openFile filePath ReadMode
  fileTxt <- hGetContents file
  parseTest pStmts fileTxt 

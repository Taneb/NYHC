module NYHC.Main (main) where

import NYHC.Parse (Module, Name(..), Result(..), parseFile)
import NYHC.Utils (decl)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"]     -> usage
    ["--help"] -> usage
    [fname]    -> parseFile fname >>= findMain
    _ -> usage

usage :: IO ()
usage = putStrLn "usage: nyhc <file>"

findMain :: Result Module -> IO ()
findMain (Ok modul) =
  maybe (putStrLn "No main function!") (\m -> putStrLn "Found main:" >> print m) . decl modul $ Ident "main"
findMain err = do
  putStrLn "Something went wrong!"
  putStrLn ""
  print err
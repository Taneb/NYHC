module NYHC.Main (main) where

import NYHC.Parse (parseFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"]     -> usage
    ["--help"] -> usage
    [fname]    -> parseFile fname >>= print
    _ -> usage

usage :: IO ()
usage = putStrLn "usage: nyhc <file>"
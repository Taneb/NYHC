module NYHC.Main (main) where

import Control.Monad (forM_)
import NYHC.Parse (Module(..), ModuleName(..), Name(..), Result(..), parseFile)
import NYHC.Utils (decl)
import NYHC.Modules
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"]     -> usage
    ["--help"] -> usage
    [fname]    -> recursiveParseFile fname [ModulePath "/tmp"] >>= showRes
    _ -> usage

usage :: IO ()
usage = putStrLn "usage: nyhc <file>"

showRes :: Result (Module, [(ModuleName, Result Module)]) -> IO ()
showRes (Ok (Module _ (ModuleName m) _ _ _ _ _, ms)) = do
  putStrLn "OK."
  forM_ ms $ \(ModuleName m, res) -> case res of
    (Ok _)             -> putStrLn $ "OK: " ++ m
    (BadParse loc msg) -> putStrLn $ "BAD: " ++ m ++ " " ++ show loc ++ ", " ++ show msg
    (Exception exc)    -> putStrLn $ "BAD: " ++ m ++ " " ++ show exc
showRes (BadParse loc msg) = putStrLn $ "BAD. " ++ show loc ++ ", " ++ show msg
showRes (Exception exc)    = putStrLn $ "BAD. " ++ show exc

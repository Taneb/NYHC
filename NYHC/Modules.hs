module NYHC.Modules where

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import NYHC.Parse
import NYHC.Utils (splitOn)
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))

newtype ModulePath = ModulePath FilePath
  deriving (Eq, Ord, Show)

-- | Attempt to parse a file, and any modules it imports
-- (recursively). Modules are looked for relative to the current
-- directory, and the list of 'ModulePath's.
--
-- Haskell 98, no extensions on my default, LANGUAGE and LINE pragmas
-- ignored, and no default fixities.
recursiveParseFile :: FilePath -> [ModulePath] -> IO (Result (Module, [(ModuleName, Result Module)]))
recursiveParseFile fp mps = parseFile fp >>= \res -> case res of
  Ok mod -> (\ms -> Ok (mod, ms)) <$> recursiveParse mod mps

  BadParse loc msg -> return $ BadParse loc msg
  Exception exc    -> return $ Exception exc

-- | Attempt to parse a module by name, and any modules it imports
-- (recursively). Modules are looked for relative to the current
-- directory, and the list of 'ModulePath's.
--
-- Haskell 98, no extensions on my default, LANGUAGE and LINE pragmas
-- ignored, and no default fixities.
recursiveParseModule :: ModuleName -> [ModulePath] -> IO (Result (Module, [(ModuleName, Result Module)]))
recursiveParseModule (ModuleName m) mps = moduleFile >>= flip recursiveParseFile mps where
  -- File corresponding to the module in the list of 'ModulePath's. If
  -- nothing could be found, this is just the module path relative to
  -- the current working directory.
  moduleFile = do
    mf <- filterM doesFileExist $ map (\(ModulePath mp) -> mp </> asPath) mps
    return $ case mf of
      (mf':_) -> mf'
      []      -> asPath

  -- The 'ModuleName' as a path.
  asPath = foldr (</>) "" (splitOn '.' m) <.> "hs"

-- | Attempt to recursively parse all modules included from this
-- one. Modules are looked for relative to the current directory, and
-- the list of 'ModulePath's.
--
-- Haskell 98, no extensions on my default, LANGUAGE and LINE pragmas
-- ignored, and no default fixities.
recursiveParse :: Module -> [ModulePath] -> IO [(ModuleName, Result Module)]
recursiveParse (Module _ _ _ _ _ imports _) mps = go [] imports where
  go done [] = return done
  go done (i:is)
    -- If the module has already been parsed, continue.
    | modul `elem` parsed = go done is
    -- Otherwise, parse it. This dispatch here allows cyclic module
    -- imports.
    | otherwise = do
       res <- recursiveParseModule modul mps
       case res of
         Ok (m, ms)       -> go (merge (modul, Ok m) ms)         is
         BadParse loc msg -> go ((modul, BadParse loc msg):done) is
         Exception exc    -> go ((modul, Exception exc):done)    is

    where
      -- The name of the module
      modul = importModule i

      -- The names of modules already done
      parsed = map fst done

      -- Merge the parse result with the done list
      merge m ms = m : (filter (\(name,_) -> name `notElem` parsed) ms ++ done)

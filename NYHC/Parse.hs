module NYHC.Parse
  ( -- * Parsing
    Result(..)
  , parseFile

  -- * Syntax
  , module Language.Haskell.Exts.Syntax
  ) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Exception (SomeException, catch)
import Data.Monoid (Monoid(..))
import Language.Haskell.Exts (parseFileWithMode)
import Language.Haskell.Exts.Extension (Language(Haskell98))
import Language.Haskell.Exts.Fixity (preludeFixities)
import Language.Haskell.Exts.Parser (ParseMode(..), ParseResult(..))
import Language.Haskell.Exts.SrcLoc (SrcLoc(..), noLoc)
import Language.Haskell.Exts.Syntax

-- | The result of attempting to parse a file.
data Result a
  = Ok a
  -- ^ The parse succeeded, yielding a value.
  | BadParse SrcLoc String
  -- ^ The parse failed at the given location.
  | Exception SomeException
  -- ^ An exception arose whilst trying to open the file.
  deriving Show

instance Functor Result where
  fmap f (Ok x)             = Ok $ f x
  fmap _ (BadParse loc msg) = BadParse loc msg
  fmap _ (Exception e)      = Exception e

instance Applicative Result where
  pure = Ok

  (Ok f)             <*> x = f <$> x
  (BadParse loc msg) <*> _ = BadParse loc msg
  (Exception e)      <*> _ = Exception e

instance Monad Result where
  return = pure

  fail = BadParse noLoc

  (Ok x)             >>= f = f x
  (BadParse loc msg) >>= _ = BadParse loc msg
  (Exception e)      >>= _ = Exception e

instance Monoid o => Monoid (Result o) where
  mempty = Ok mempty

  (Ok x) `mappend` (Ok y) = Ok $ x `mappend` y
  (Ok _) `mappend` err    = err
  err    `mappend` _      = err

-- | Attempt to parse a file.
--
-- Haskell 98, no extensions on my default, LANGUAGE and LINE pragmas
-- ignored, and no default fixities.
parseFile :: FilePath -> IO (Result Module)
parseFile fp = catch (toResult <$> parseFileWithMode mode fp) (return . Exception) where

  mode = ParseMode
    { parseFilename         = fp
    , baseLanguage          = Haskell98
    , extensions            = []
    , ignoreLanguagePragmas = True
    , ignoreLinePragmas     = True
    , fixities              = Nothing
    }

  toResult (ParseOk a) = Ok a
  toResult (ParseFailed loc msg) = BadParse loc msg

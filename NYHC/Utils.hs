module NYHC.Utils (splitOn, decl, decl', findDef) where

import Control.Applicative ((<$>), (<|>))
import Data.Maybe (mapMaybe)
import NYHC.Parse

-- | Split a list on a given delimiter element.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a as = case dropWhile (==a) as of
  [] -> []
  as' -> let (p, r) = break (==a) as' in p : splitOn a r

-- | Look up a top-level declaration by name.
--
-- If the name is bound in a pattern match or data constructor, the
-- entire thing is returned.
--
-- This ignores all non-Haskell98 decls: 'TypeFamDecl',
-- 'ClosedTypeFamDecl', 'GDataDecl', 'DataFamDecl', 'TypeInsDecl',
-- DataInsDecl', 'GDataInsDecl', 'ForImp', and 'ForExp'.
--
-- In addition, type signature decls are also ignored.
decl :: Module -> Name -> Maybe Decl
decl (Module _ _ _ _ _ _ decls) = decl' decls

-- | Look up a declaration by name.
--
-- If the name is bound in a pattern match or data constructor, the
-- entire thing is returned.
--
-- This ignores all non-Haskell98 decls: 'TypeFamDecl',
-- 'ClosedTypeFamDecl', 'GDataDecl', 'DataFamDecl', 'TypeInsDecl',
-- DataInsDecl', 'GDataInsDecl', 'ForImp', and 'ForExp'.
--
-- In addition, type signature decls are also ignored.
decl' :: [Decl] -> Name -> Maybe Decl
decl' [] _ = Nothing
decl' (d:ds) name = case d of
  (TypeDecl _ n _ _)        -> check n d   <|> decl' ds name
  (DataDecl _ _ _ n _ cs _) -> check n d   <|> inQCon cs <|> decl' ds name
  (ClassDecl _ _ n _ _ cd)  -> check n d   <|> inCDec cd <|> decl' ds name
  (FunBind ms)              -> matching ms <|> decl' ds name
  (PatBind _ p _ _)         -> inPat p     <|> decl' ds name
  _ -> decl' ds name

  where
    -- Check if a name matches.
    check n d = if name == n then Just d else Nothing

    -- Find a data constructor with the right name.
    inQCon (QualConDecl _ _ _ c:qs) = inCon c <|> inQCon qs where
      -- 'inQCon' but for unqualified constructors.
      inCon c@(ConDecl n _)  = check n d
      inCon c@(RecDecl n rs) = check n d <|> if name `elem` concatMap fst rs then Just d else Nothing
      inCon c@(InfixConDecl _ n _) = check n d
    inQCon [] = Nothing

    -- Find a typeclass member with the right name/
    inCDec (ClsDecl d:cs) = decl' [d] name <|> inCDec cs
    inCDec [] = Nothing

    -- Extract function match clauses with the right name.
    matching ms = if null matches then Nothing else Just $ FunBind matches where
      matches = filter (\(Match _ n _ _ _ _) -> name == n) ms

    -- Check if a pattern match binds the name.
    inPat p = if inPat' p then Just d else Nothing where
      inPat' (PVar n)            = name == n
      inPat' (PInfixApp p1 _ p2) = inPat' p1 || inPat' p2
      inPat' (PApp _ ps)         = any inPat' ps
      inPat' (PTuple _ ps)       = any inPat' ps
      inPat' (PList ps)          = any inPat' ps
      inPat' (PParen p)          = inPat' p
      inPat' (PRec _ fs)         = any inPatRec fs
      inPat' (PAsPat n p)        = name == n || inPat' p
      inPat' (PIrrPat p)         = inPat' p
      inPat' (PatTypeSig _ p _)  = inPat' p
      inPat' (PBangPat p)        = inPat' p
      inPat' _ = False

      -- 'inPat'' for record field patterns.
      inPatRec (PFieldPat _ p) = inPat' p
      inPatRec _ = False

-- | Find the module which defined a name. If multiple modules are
-- returned, the name is ambiguous.
findDef :: [Module] -> Name -> [ModuleName]
findDef ms name = mapMaybe (\m -> const (modName m) <$> decl m name) ms where
  modName (Module _ n _ _ _ _ _) = n

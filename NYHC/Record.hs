module NYHC.Record where

import Control.Exception (assert)
import Control.Monad
import Control.Monad.Trans.Writer
import qualified Data.Map.Strict as M
import Language.Haskell.Exts.Syntax

reconstruct :: QualConDecl -> QualConDecl
reconstruct (QualConDecl sl bs cxt c) = QualConDecl sl bs cxt $ case c of
  RecDecl name ts -> ConDecl name (map snd ts)
  _ -> c

undefineRecords :: [Decl] -> ([Decl], [Decl])
undefineRecords ds0 = runWriter $ forM ds0 $ \decl -> case decl of
  DataDecl sl don con name ts qs ds ->
    if any (\(QualConDecl _ _ _ c) -> case c of RecDecl _ _ -> True; _ -> False) qs
    then do
      tell [decl]
      return $ DataDecl sl don con name ts (map reconstruct qs) ds
    else
      return decl
  _ -> return decl

reshuffle :: [([a], b)] -> [(a, b)]
reshuffle = concatMap (\(ns, t) -> map (flip (,) t) ns)

getAccessorInformation :: Decl -> (SrcLoc, Name, M.Map Name (Type, [(Name,Int,Int)]))
getAccessorInformation (DataDecl sl _ _ name _ qs _) = 
  let recs = [(n, reshuffle fs) | (QualConDecl _ _ _ (RecDecl n fs)) <- qs]
  in (sl, name, foldr (\(n, fs) m0 -> let l = length fs in foldr (\(i, (an, t)) m1 -> M.insertWith mergeThings an (t, [(n, i, l)]) m1) m0 (zip [0..] fs)) M.empty recs)
  where
    mergeThings :: (Type, [(Name, Int, Int)]) -> (Type, [(Name, Int, Int)]) -> (Type, [(Name, Int, Int)])
    mergeThings (t1, xs) (t2, ys) = assert (t1 == t2) (t1, xs ++ ys)
getAccessorInformation _ = error "stuff"

redefineAccessors :: [Decl] -> [Decl]
redefineAccessors decls = do
  (sl, name, m) <- map getAccessorInformation decls
  (n, (t, as)) <- M.toAscList m
  [TypeSig sl [n] (TyFun (TyCon (UnQual name)) t),
   FunBind $ map (\(cn, i, l) -> Match sl n [PApp (UnQual cn) (replicate i PWildCard ++ PVar (Ident "x") : replicate (l - i - 1) PWildCard)] Nothing (UnGuardedRhs (Var (UnQual (Ident "x")))) (BDecls [])) as
    ]

-- Transform a module to remove record syntax, by the following transformations:
-- * data types declared as records are changed to regular, boring, product types
-- * accessor functions are explicitly defined
-- TODO:
-- * replace record-ish pattern matching with less fanc stuff
-- * do something with update syntax
derecord :: Module -> Module
derecord (Module sl mn mps wt ess ids ds) =
  let (sansRecords, oldRecordDecls) = undefineRecords ds
  in Module sl mn mps wt ess ids (sansRecords ++ redefineAccessors oldRecordDecls)


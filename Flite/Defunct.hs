module Flite.Defunct where

import Flite.Syntax
import Flite.Traversals
import Flite.Pretty
-- import Flite.Descend

import Debug.Trace
import List


type Request = (Id, Exp)

type Replacement = (Exp, Exp) -- (from, to)

defunctionalise :: Prog -> Prog
defunctionalise p = trace ("\n\n::::: After :::::::\n" ++ show p' ++ "\n::::::::::::\n" ) p'
    where
        p' = findUsedDecls $ defunctionalise' $
            trace ("::::: Before ::::::\n"   ++ show p ++ "\n::::::::::::\n" ) p

defunctionalise' :: Prog -> Prog
defunctionalise' p = after
    where
        before = p
        after  = case rs of 
            [] -> p'
            _  -> -- trace ("Requests:" ++ show rs)
                defunctionalise' $ makeRequestedDecls p' rs
        (p', rss) = unzip $ map (defuncRhs p) $ before
        rs = concat rss


-- we pass in Prog because we're going to need to know arity later.
defuncRhs :: Prog -> Decl -> (Decl, [Request])
defuncRhs p (Func id pats rhs) = (Func id pats rhs', rs)
    where 
        (rhs', rs) = 
            -- trace ("Stripping Funs passed as args in " ++ id) $
            defuncExp p rhs


defuncExp :: Prog -> Exp -> (Exp, [Request])
defuncExp p e@(App f args) = (e', rs)
    where
        e' = App f' args'
        (f', args', rs) = defuncApp p f args
defuncExp p e@(Case ex alts) = (Case ex' alts', rs')
    where
        (alts', rss) = unzip $ map (defuncAlt p) alts
        (ex', rs) = defuncExp p ex
        rs' = concat (rs:rss)
defuncExp p e@(Let bs ex) = (Let bs' ex', rs')
    where
        (bs', rss) = unzip $ map (defuncBinding p) bs
        (ex', rs) = defuncExp p ex
        rs' = concat (rs:rss)
defuncExp p e@(Lam is ex) = (Lam is ex', rs)
    where
        (ex', rs) = defuncExp p ex
defuncExp p e = 
--    trace ("Unchanged " ++ show e) 
    (e, [])


defuncBinding :: Prog -> Binding -> (Binding, [Request])
defuncBinding p (id, ex) = ( (id, ex'), rs )
    where
        ( ex', rs ) = defuncExp p ex

defuncAlt :: Prog -> Alt -> (Alt, [Request])
defuncAlt p (pat, ex) = ( (pat', ex'), rs'')
    where
        (pat', rs) = defuncExp p pat
        (ex', rs') = defuncExp p ex
        rs'' = rs ++ rs'

data AppClassification = 
    Primitive |
    Nullary |
    Partial |
    Saturated |
    OverApplied |
    HigherOrder |
    HigherOrderWithPartial

classifyApplication :: Prog -> Exp -> AppClassification
classifyApplication p e@( App (Fun id) args ) =
    if isPrimId id then
        Primitive
    else if arityOf p id == 0 then
        Nullary
    else if arityOf p id > length args then
        Partial
    else if arityOf p id < length args then
        OverApplied
    else if isFunctionAppliedToPartial p e then
        HigherOrderWithPartial
    else if (or $ map (isItAFunction p) args) then
        HigherOrder
    else
        Saturated


defuncApp :: Prog -> Exp -> [Exp] -> (Exp, [Exp], [Request]) -- (f', args', rs)
defuncApp p f args =
    case classifyApplication p (App f' args') of
        HigherOrderWithPartial ->
            defuncAppWithPartiallyAppliedArg p f' args' rs'
        HigherOrder -> 
            defuncApp' p f' args' rs'
        _ ->
            (f', args', rs')
    where
        (f', rs) = defuncExp p f
        (args', rss) = unzip $ map (defuncExp p) args
        rs' = concat (rs:rss)


-- The business-end of the basic defunctionaliser
defuncApp' :: Prog -> Exp -> [Exp] -> [Request] -> (Exp, [Exp], [Request])
defuncApp' p f args rs = (f', vArgs, rs')
    where 
        req = ( sName, App f args )
        rs' = req:rs
        sName = specialisedName (f:fArgs)
        (fArgs, vArgs) = partitionArgs p args
        f' = case f of 
            Fun _ -> Fun $ sName
            Con _ -> Con $ sName
            _     -> error "This shouldn't be possible!"


isFunctionAppliedToPartial :: Prog -> Exp -> Bool
isFunctionAppliedToPartial p e@(App (Fun _) (App (Fun id) aargs):_) =
    if length aargs < arityOf p id then True else False
isFunctionAppliedToPartial p e@(App (Fun _) (App (Con id) aargs):_) = 
    if length aargs < arityOf p id then True else False
isFunctionAppliedToPartial p _ = False


-- Defunctionalise a function applied to a partial application of another function. Only handles a contiguous list of partial apps at the start of the arg list. Don't know how much of a problem this will be.
defuncAppWithPartiallyAppliedArg :: Prog -> Exp -> [Exp] -> [Request] -> (Exp, [Exp], [Request])
defuncAppWithPartiallyAppliedArg p f args@((App af aargs):as) rs = (f', args', rs')
    where
        args' = aargs ++ as
        newName = specialisedName (f:args')
        f' = Fun newName
        rs' = (newName, (App f args)):rs

partitionArgs :: Prog -> [Exp] -> ([Exp], [Exp]) -- (funcArgs, valArgs)
partitionArgs p = partition (isItAFunction p)


isItAFunction :: Prog -> Exp -> Bool
isItAFunction p (Fun id) = if (arityOf p id) == 0 then False else True
isItAFunction p (Con id) = True
isItAFunction p (App (Fun id) args) =
    if (arityOf p id) > length args
        then True
        else False
isItAFunction p _ = False

specialisedName :: [Exp] -> Id
specialisedName es = -- trace ("Requested name for " ++ show es) $
    foldr ( (++) . ('^':) ) "" $ map getId es


getId :: Exp -> Id
getId (Fun id) = id
getId (Con id) = id
getId (App (Fun id) args) = show (length args) ++ id
getId e = ""




makeRequestedDecls :: Prog -> [Request] -> Prog
makeRequestedDecls p [] = p
makeRequestedDecls p (r:rs) = makeRequestedDecls p' rs
    where
        p' = (makeDeclFromRequest p r) ++ p


-- makeDeclFromRequest and specialiseDecl should only return a single
-- Decl, but they do so in a list to gracefully handle the possiblity
-- of a new Decl not being required.
makeDeclFromRequest :: Prog -> Request -> [Decl]
makeDeclFromRequest pr (newName, ex@(App f args)) =
    case (existing, classifyApplication pr ex) of
        ([], HigherOrderWithPartial) ->
            inlinePartialApplication pr specialiseMe (head args) newName
        ([], HigherOrder) -> 
            specialiseDecl pr specialiseMe args newName
        (_, _) -> []
    where
        existing = lookupFuncs newName pr
        specialiseMe = lookupFuncs (getId f) pr
makeDeclFromRequest pr (newName, e@(Fun id)) = [Func newName args rhs]
    where
        args = case arityOf pr id of 
            1 -> [Var "?a"]
            2 -> [Var "?a", Var "?b"]
        rhs = (App e args)
makeDeclFromRequest pr e = error ("Non-application passed to function specialiser: " ++ show e)

arityOf :: Prog -> Id -> Int
arityOf p id =
    case ds of 
        [] -> if isBinaryPrim id then 2
                else if isUnaryPrim id then 1
                else error ("Couldn't find a declaration for " ++ id)
        [d] -> length $ funcArgs d
        _ -> error ("Multiple declarations for " ++ id)
    where
        ds = lookupFuncs id p


-- specialiseDecl takes and returns lists of Decls to handle the 
-- possibility of a new Decl not being needed (ie, we simply 
-- return the empty list.
specialiseDecl :: Prog -> [Decl] -> [Exp] -> Id -> [Decl]
specialiseDecl p [d@(Func id params rhs)] args newId =
    -- trace ("Specialising " ++ id ++ " to " ++ newId) $
    [Func newId params' rhs']
    where
        whichArgsAreFunctional = map (isItAFunction p) args
        replaceMe = zip whichArgsAreFunctional params
        replaceWith = zip whichArgsAreFunctional args
        repls = zip (extractSndWhereFstIsTrue replaceMe) (extractSndWhereFstIsTrue replaceWith)
        rhs' = replaceInExp repls rhs
        params' = extractSndWhereFstIsFalse $ zip whichArgsAreFunctional params 
specialiseDecl _ _ _ newId = error $ "Couldn't honour request for " ++ newId

inlinePartialApplication :: Prog -> [Decl] -> Exp -> Id -> [Decl]
inlinePartialApplication p [d@(Func id (arg:args) rhs)] aarg id =
    Func newId args' rhs'
    where
        args' = (tail aarg) ++ args


extractSndWhereFstIsTrue :: [(Bool, b)] -> [b]
extractSndWhereFstIsTrue = (map snd) . (filter fst)

extractSndWhereFstIsFalse :: [(Bool, b)] -> [b]
extractSndWhereFstIsFalse = (map snd) . (filter (not.fst))



replaceInExp :: [Replacement] -> Exp -> Exp
replaceInExp rs e = 
    if (length matches) > 0 then
        snd $ head matches
    else case e of
        (App f args) -> 
            App (replaceInExp rs f) (map (replaceInExp rs) args)
        (Case e alts) ->
            Case (replaceInExp rs e) (map (replaceInAlt rs) alts)
        (Let bs e) ->
            Let (map (replaceInLet rs) bs) (replaceInExp rs e)
        (Lam is e) ->
            Lam is (replaceInExp rs e)
        e -> e
    where
        matches = filter ( (==e) . fst ) rs

replaceInAlt :: [Replacement] -> Alt -> Alt
replaceInAlt rs a = (replaceInExp rs $ fst a, replaceInExp rs $ snd a)

replaceInLet :: [Replacement] -> Binding -> Binding
replaceInLet rs b = (fst b, replaceInExp rs $ snd b)





-- Tidy up the resulting program

findUsedDecls :: Prog -> Prog
findUsedDecls p = findUsedDecls' p $ lookupFuncs "main" p


findUsedDecls' :: Prog -> [Decl] -> Prog
findUsedDecls' p acc =
    if length acc' == length acc 
        then acc'
        else findUsedDecls' p acc'
    where
        acc' = filter ( \d -> (funcName d) `elem` ("main":usedFuncs) ) p
        usedFuncs = catalogueCalledFunctions acc


catalogueCalledFunctions :: [Decl] -> [Id]
catalogueCalledFunctions ds = concatMap (findFunsInExp . funcRhs) ds


findFunsInExp :: Exp -> [Id]
findFunsInExp (App f args) = concatMap findFunsInExp (f:args)
findFunsInExp (Case e alts) =
    concatMap findFunsInExp (e:( (fst $ unzip alts) ++ (snd $ unzip alts)))
findFunsInExp (Let bs e) = concatMap findFunsInExp ( e:(snd $ unzip bs) )
findFunsInExp (Lam is e) = findFunsInExp e
findFunsInExp (Fun id) = [id]
-- findFunsInExp (Con id) = [id] -- top-level constructors get inlined with this commented. Good/bad? Not sure.
findFunsInExp _ = []



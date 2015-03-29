--Type-checker program is closely based on "The Implementation of
--Functional Programming Languages" book by S. Peyton Jones, chapters 8 and 9.
--This program also allows the declaration of algebraic data types.
--The version is an optimized version of TypeChecker.

module Flite.TypeChecker2 where
import Flite.Syntax
import Flite.Identify
import Flite.Fresh
import Flite.Matching
import Flite.Dependency
import Flite.Traversals
import Flite.Descend
import qualified Flite.Prelude as Prelude
import Data.List
import Data.Maybe

arrow :: Type_exp -> Type_exp -> Type_exp
arrow  t1 t2 = TCONS "TArrow" [t1,t2]

tint   :: Type_exp
tint   = TCONS "Int" []

tbool   :: Type_exp
tbool   = TCONS "Bool" []

tcross :: Type_exp -> Type_exp -> Type_exp
tcross t1 t2 = TCONS "(,)" [t1,t2]

tlist ::  Type_exp -> Type_exp
tlist t = TCONS "List" [t]

tOutlist :: [String] -> [Type_exp] -> Type_exp
tOutlist ns ts = TCONS "" [tOutlist'' (ns', ts') | (ns',ts')
                                                   <- zip (map tOutlist' ns) ts]
                 where tOutlist' s  = TCONS s []
                       tOutlist'' (t1,t2) = TCONS "::" [t1,t2]

tvars_in :: Type_exp -> [Tvname]
tvars_in t = tvars_in' t []
  where
     tvars_in' (TVAR x) l     = x:l
     tvars_in' (TCONS y ts) l = foldr tvars_in' l ts

---Substitutions
type Subst = Tvname -> Type_exp

sub_type :: Subst -> Type_exp -> Type_exp
sub_type phi (TVAR tvn)     = phi tvn
sub_type phi (TCONS tcn ts) = TCONS tcn (map (sub_type phi) ts)

scomp :: Subst -> Subst -> Subst
scomp sub2 sub1 tvn= sub_type sub2 (sub1 tvn)

id_subst ::Subst
id_subst tvn = TVAR tvn

delta :: Tvname -> Type_exp -> Subst
delta tvn t tvn' | tvn==tvn'   = t
                 | otherwise   = TVAR tvn'

extend :: Subst -> Tvname -> Type_exp -> Maybe Subst
extend phi tvn t
 | t == TVAR tvn          = Just phi
 | tvn `elem` (tvars_in t)= error $ "Occurs check error" ++ show (t) ++ "," ++ show (tvars_in t)
 | otherwise              = Just ((delta tvn t) `scomp` phi)

unify:: Subst -> (Type_exp,Type_exp) -> Maybe Subst
unify phi ((TVAR tvn),t)
          | phitvn == TVAR tvn = extend phi tvn phit
          | otherwise          = unify phi (phitvn, phit)
 where
    phitvn = phi tvn
    phit   = sub_type phi t

unify phi ((TCONS tcn ts), (TVAR tvn)) = unify phi ((TVAR tvn), (TCONS tcn ts))

unify phi ((TCONS tcn ts), (TCONS tcn' ts'))
 | tcn == tcn'= unifyl phi (ts `zip` ts')
 | otherwise = error $ "Types :"++ show tcn ++" and "++ show tcn'++" don't unify"

unifyl :: Subst -> [(Type_exp,Type_exp)] -> Maybe Subst
unifyl phi eqns = foldr unify' (Just phi) eqns
 where
 unify' eqn(Just phi)= unify phi eqn
 unify' eqn Nothing = error $ show $ "Types does not unify : " ++ show eqns

---Type Schemmes
data Type_scheme =  SCHEME [Tvname] Type_exp deriving (Show,Eq)

unknowns_scheme :: Type_scheme -> [Tvname]
unknowns_scheme (SCHEME scvs t) = (tvars_in t) \\ scvs

sub_scheme :: Subst -> Type_scheme -> Type_scheme
sub_scheme phi (SCHEME scvs t) = SCHEME scvs (sub_type (exclude phi scvs) t)
  where
     exclude phi scvs tvn
                         | tvn `elem` scvs = TVAR tvn
                         | otherwise       = phi tvn

---Association Lists
type Assoc_list a b = [(a,b)]
type Type_env = Assoc_list Id Type_scheme

dom :: Assoc_list a b -> [a]
dom al   = [k| (k,v)<- al]

val :: (Ord a,Show b,Show a) => Assoc_list a b -> a -> b
val al k =  case [v | (k',v)<- al , k==k'] of
             [] -> error $ "Variable not found in the environment : " ++ show  k
             _  -> head [v | (k',v)<- al , k==k']

unknowns_te :: Type_env -> [Tvname]
unknowns_te gamma = concat $ map unknowns_scheme (map (val gamma) (dom gamma))

sub_te :: Subst -> Type_env -> Type_env
sub_te phi gamma = [(x, sub_scheme phi st) | (x,st) <- gamma]

---New Variables
type Name_supply = Tvname

next_name :: Name_supply -> Tvname
next_name ns = ns

deplete :: Name_supply -> Name_supply
deplete (n:ns) = (n+2:ns)

split_name :: Name_supply -> (Name_supply,Name_supply)
split_name ns = (0:ns,1:ns)

name_sequence :: Name_supply -> [Tvname]
name_sequence ns = next_name ns : name_sequence(deplete ns)

allowName :: Name_supply -> (Tvname, Name_supply)
allowName ns = (next_name ns, deplete ns)

---Type-Checker
tc:: Type_env -> Name_supply -> Exp -> Maybe (Subst, Type_exp,Type_env)
tc gamma ns (App e1 [e2])              = tcap  gamma ns e1 e2
tc gamma ns (Let bindings e)           = tcletrec gamma ns bindings e
tc gamma ns (Var x)                    = tcvar gamma ns x
tc gamma ns (Con x)                    = tcvar gamma ns x
tc gamma ns (Fun x)                    = tcvar gamma ns x
tc gamma ns (Case x alts)              = tccase gamma ns x alts
tc gamma ns (Lam [x] e)                = tclambda gamma ns x e
tc gamma ns (Int x)                    = tcint gamma ns (show x)
--tc gamma ns (TLet bindings e)          = tctlet gamma ns bindings e
--tc gamma ns e                          = error $ show e

tcl :: Type_env -> Name_supply -> [Exp] -> Maybe (Subst, [Type_exp],Type_env)
tcl gamma ns []      = return (id_subst,[],gamma)
tcl gamma ns (e:es)  = tcl1 es (tc gamma ns1 e)
  where (ns0,ns1)                 = split_name ns
        tcl1 es (Just (phi,t,gamma'))       = tcl2 (tcl (sub_te phi gamma') ns0 es) phi t
        tcl2 (Just (psi,ts,gamma'')) phi t  = Just (psi `scomp` phi,(sub_type psi t):ts,sub_te (psi `scomp` phi) gamma'')

tcint:: Type_env -> Name_supply -> Id -> Maybe (Subst,Type_exp,Type_env)
tcint gamma ns x = Just (id_subst, tint, gamma)

tcvar:: Type_env -> Name_supply -> Id -> Maybe (Subst,Type_exp,Type_env)
tcvar gamma ns x = Just (id_subst, newinstance ns scheme, gamma)
  where scheme = val gamma x

newinstance :: Name_supply -> Type_scheme -> Type_exp
newinstance ns (SCHEME scvs t)=  sub_type phi t
  where  al = scvs `zip` (name_sequence ns)
         phi= al_to_subst al

al_to_subst :: Assoc_list Tvname Tvname -> Subst
al_to_subst al tvn
                  |tvn `elem` (dom al) = TVAR(val al tvn)
                  |otherwise           = TVAR tvn

tcap :: Type_env -> Name_supply -> Exp -> Exp -> Maybe (Subst,Type_exp,Type_env)
tcap gamma ns e1 e2 =  tcap1 (tcl gamma ns'  [e1,e2])
  where
  (tvn,ns')                        = allowName ns
  tcap1 (Just (phi,[t1,t2],gamma1))= tcap2 ((unify phi (t1,t2 `arrow` (TVAR tvn))), gamma1)
  tcap2 ((Just phi),gamma1)        =  Just(phi,phi tvn,sub_te phi gamma1)
  tcap2 (Nothing,_)                =  Nothing

convApp :: Exp -> Exp
convApp (App e xs) = apply (convApp e) (map convApp xs)
convApp e = descend convApp e

apply :: Exp -> [Exp] -> Exp
apply e [] = e
apply e (x:xs) = apply (App e [x]) xs

tclambda :: Type_env -> Name_supply -> Id -> Exp -> Maybe (Subst,Type_exp,Type_env)
tclambda gamma ns x e = tclambda1 (tc (new_bvar (x,tvn):gamma) ns' e)
  where (tvn,ns')                   = allowName ns
        new_bvar (x,tvn)            = (x,SCHEME [] (TVAR tvn))
        tclambda1  Nothing          = Nothing
        tclambda1  (Just (phi,t,gamma1))= Just (phi, (phi tvn) `arrow` t,gamma)

new_bvars :: [Id] -> Name_supply -> [(Id, Type_scheme)]
new_bvars exps ns = map new_bvar (exps `zip` (name_sequence ns))
                     where new_bvar (x,tvn)   = (x,SCHEME [] (TVAR tvn))

old_bvar :: (Id, Type_scheme) -> Type_exp
old_bvar (x,SCHEME [] t) =t

 {--old case rule
tccase :: Type_env -> Name_supply -> Exp -> [(Exp,Exp)] -> Maybe (Subst,Type_exp)
tccase gamma ns exp alts =
 do
    let (tvn,ns')    = allowName ns
        (ns0,ns1)    = split_name ns'
        tvn'         = next_name ns0
        (pats1,alts1)= unzip alts
        nbvs               = new_bvars (getAltsId pats1) ns'
        (pats',alts')      = (map convApp pats1, map convApp alts1)
    (s2,tpats)    <- tcl (gamma++nbvs) ns0 pats'
    (s1,talts)    <- tcl (sub_te s2 (nbvs++gamma)) tvn alts'
    (s3,tsubj)    <- tc gamma ns1 (convApp exp)
    spat          <- unifyl s3 [(tsubj,tpat)|tpat <- tpats]
    sa            <- unifyl s1 [(TVAR tvn',talt)|talt <- talts]
    return (spat `scomp` sa `scomp` s3 `scomp` s2  `scomp` s1 , TVAR tvn')
--}
tccase :: Type_env -> Name_supply -> Exp -> [(Exp,Exp)] -> Maybe (Subst,Type_exp,Type_env)
tccase gamma ns exp alts =
 do
    let (tvn,ns')    = allowName ns
        (ns0,ns1)    = split_name ns'
        tvn'         = next_name ns0
        (pats1,alts1)= unzip alts
        nbvs               = new_bvars (getAltsId pats1) ns'
        (pats',alts')      = (map convApp pats1, map convApp alts1)
    (s3,tsubj,gamma1)    <- tc gamma ns1 (convApp exp)
    (s2,tpats,gamma2)    <- tcl (gamma++nbvs) ns0 pats'
    spat          <- unifyl s3 [(tsubj,tpat)|tpat <- tpats]
    (s1,talts,gamma3)    <- tcl (sub_te (spat `scomp` s2) ( nbvs++gamma)) tvn alts'
    sa            <- unifyl s1 [(TVAR tvn',talt)|talt <- talts]
    return (spat `scomp` sa `scomp` s3 `scomp` s2  `scomp` s1 , TVAR tvn',sub_te (spat `scomp` sa `scomp` s3 `scomp` s2  `scomp` s1 ) gamma3)

getAltsId::[Exp]->[Id]
getAltsId ((App c (x:xs)):exps)    =   (map getVarId (x:xs)) ++ (getAltsId exps)
getAltsId ((App c []):exps)        =   getAltsId exps
getAltsId e                        =   map getVarId e --it was []

 {--
tctlet :: Type_env -> Name_supply -> [(Id,Exp)] -> Exp -> Maybe (Subst, Type_exp, Type_env)
tctlet gamma ns binds  e = Just(s,t,gamma)
  where t         =  tOutlist fs ts
        (s,ts1)   =  (id_subst,ts)
        ts        =  map gett (map snd gamma)
        fs        =  map fst gamma
        gett (SCHEME sv t) = t
--}

tcletrec :: Type_env -> Name_supply -> [(Id,Exp)] -> Exp -> Maybe (Subst,Type_exp,Type_env)
tcletrec gamma ns binds  e =
 tcletrec1 gamma ns0 nbvs  e (tcl (nbvs ++ gamma) ns1 es')
  where
     (ns0,ns')         = split_name ns
     (ns1,ns2)         = split_name ns'
     (xs',es')         = unzip binds
     nbvs              = (new_bvars xs' ns2)

tcletrec1 gamma ns nbvs e (Just (phi,ts,gamma1)) = --if (fst (head (nbvs))=="append1") then error $ show (ts,ts') else
 tcletrec2 gamma' ns nbvs' e (unifyl phi(ts `zip` ts'),gamma')
  where
     ts'               = map old_bvar nbvs'
     nbvs'             = sub_te phi nbvs
     gamma'            = sub_te phi gamma

tcletrec2 gamma ns nbvs e (Just (phi),gamma1)   =  -- if (fst (head (nbvs))=="mains") then error $ show (gamma'') else
 tclet2 phi (tc gamma'' ns1 e)
  where ts              = (map old_bvar nbvs')   --nbvs
        (nbvs',gamma')  = (sub_te phi nbvs ,sub_te phi gamma)
        gamma''         = add_decls gamma' ns0 (map fst nbvs) ts
        (ns0,ns1)       = split_name ns
        tclet2 phi Nothing                = Nothing
        tclet2 phi (Just (phi',t,gamma''')) = Just (phi' `scomp` phi,t,sub_te (phi' `scomp` phi)gamma''')

add_decls ::  Type_env -> Name_supply -> [Id] -> [Type_exp] -> Type_env
add_decls gamma ns xs ts = (xs `zip` schemes) ++ gamma
  where schemes              =   (map (genbar unknowns ns) ts)
        unknowns             =   (unknowns_te gamma)
        genbar unknowns ns t = SCHEME (map snd al) t'
               where al   = scvs `zip` (name_sequence ns)
                     scvs = (nub (tvars_in t)) \\ unknowns
                     t'   = sub_type (al_to_subst al) t

---Auxiliar functions
getVarId::Exp->Id
getVarId (Var n) = n
getVarId _       = []

blam :: Id -> Exp -> Exp
blam v e = Lam [v] e

plambda :: [Id] -> Exp -> Exp
plambda vs e  = (foldr blam e vs)

f1amb::Decl-> Decl
flamb (Func name [] rhs) = Func name [] rhs
f1amb (Func name args rhs) = (Func name [] rhs')
   where rhs' = plambda args' rhs
         args'= map getVarId args

---data types
datanames :: [Decl] -> [Id]
datanames ls= map dataName ls

dataargs :: [Decl] -> [[Id]]
dataargs  ls= map dataArgs ls

datacons :: [Decl] -> [[(Id, [TypeExp])]]
datacons  ls= map dataCons ls

faux:: [Decl] -> [(Id, [Id], [(Id, [TypeExp])])]
faux decls = zip3 (datanames decls) (dataargs decls) (datacons decls)

ienv :: [Decl] -> Type_env -> Name_supply -> [(Id, Type_scheme)]
ienv decls env ns = concat([updEnvData ns env names args cons| (names,args,cons)<-faux decls] ++ [env] )

updEnvData:: Name_supply->Type_env-> Id-> [Id]->[(Id,[TypeExp])] ->Type_env
updEnvData ns gamma name [] ((id,texp):exs)   = new_bvar (id,tvn) : updEnvData ns gamma name [] exs
  where
     (tvn,ns')          = allowName ns
     new_bvar (id,tvn)  = (id,SCHEME [] (foldr arrow (TCONS (name) []) (map convtype' texp)))
updEnvData ns gamma name args ((id,texp):exs) = new_bvar (id,tvn) : updEnvData ns gamma name args exs
  where
     (tvn,ns')          = allowName ns
     new_bvar (id,tvn)  = case texp of
                          [] -> (id,SCHEME [tvn] result')
                          _  -> (id, SCHEME (getVars tvn args) (foldr arrow result' args'))
     args'              = map (convtype''' args) xs
                          where xs=  case (length args ==1) of
                                     True  -> [(x,y) | x <- (getVars tvn args), y <- texp]
                                     False -> (zip (getVars tvn args) texp)
     result'            = TCONS (name) (map TVAR (getVars tvn args))
updEnvData ns gamma name args []=  []

convtype'::TypeExp -> Type_exp
convtype'  (TEVar var)     =  TCONS (var ) []
convtype'  (TECons con texps) = TCONS con (map convtype' texps)
convtype'  (TECon con) = TCONS con []

convtype'''::[String] -> (Name_supply,TypeExp) ->  Type_exp
convtype'''  args (ns,TEVar var)  = if var `elem` args then TVAR ns else TCONS (var) []
convtype'''  args (ns,TECons con texps) = TCONS (con) [TVAR ns]
convtype'''  args (ns,TECon con)  = TCONS con []

getVars :: Name_supply-> [a]-> [Name_supply]
getVars ns []   = []
getVars ns args = take (length args) (ns: getVars tvn args)
                  where tvn = next_name ns'
                        ns' = deplete ns
--------Main---------
env_initial::Type_env
env_initial=
 [("True",SCHEME [] tbool),("False",SCHEME [] tbool),
  ("Pair",SCHEME [[4],[5]] (TCONS "TArrow"[TVAR [4],TCONS "TArrow"[TVAR [5],
                                                    TCONS "Pair" [TVAR [4],TVAR [5]]]])),
  ("(+)",SCHEME [] (TCONS "TArrow" [tint,TCONS "TArrow"[tint,tint]])),
  ("(*)",SCHEME [] (TCONS "TArrow" [tint,TCONS "TArrow"[tint,tint]])),

  ("(-)",SCHEME [] (TCONS "TArrow" [tint,TCONS "TArrow"[tint,tint]])),
  ("(==)",SCHEME [] (TCONS "TArrow" [tint,TCONS "TArrow"[tint,tbool]])),
  ("(/=)",SCHEME [] (TCONS "TArrow" [tint,TCONS "TArrow"[tint,tbool]])),
  ("(<=)",SCHEME [] (TCONS "TArrow" [tint,TCONS "TArrow"[tint,tbool]])),
  ("(>=)",SCHEME [] (TCONS "TArrow" [tint,TCONS "TArrow"[tint,tbool]])),
  ("(>)",SCHEME [] (TCONS "TArrow" [tint,TCONS "TArrow"[tint,tbool]])),
  ("(<)",SCHEME [] (TCONS "TArrow" [tint,TCONS "TArrow"[tint,tbool]])),
  ("emitInt",SCHEME [[2]] (TCONS "TArrow" [tint,TCONS "TArrow"[TVAR [2],TVAR [2]]])),
  ("emit"   ,SCHEME [[3]] (TCONS "TArrow" [tint,TCONS "TArrow"[TVAR [3],TVAR [3]]]))
 ]

getFreshProg ::  Prog -> Fresh Prog
getFreshProg  p = return (identifyFuncs p) >>= desugarCase >>= desugarEqn

desugarProg::Prog -> Prog
desugarProg p = snd (runFresh (getFreshProg  p) "$" 0)

introduceBApp :: Prog -> Prog
introduceBApp p = onExp convApp p

introduceLam :: Prog -> Prog
introduceLam p = map f1amb p

partitionDec :: Prog -> (Prog, Prog)
partitionDec = partition isFunc
  where
      isFunc (Func _ _ _) = True
      isFunc (Data _ _ _) = False

nestLet ::[Decl] -> Exp -> Exp
nestLet ds (Let bs e) = (foldr Let e (bindings ds))--(letGroups bs))
nestLet ds e = descend (nestLet ds) e

getBinding::[Decl]->[Binding]
getBinding ((Func n a r):decls) = [(n,r)] ++ getBinding decls
getBinding []                   = []

bindings ::[Decl] -> [[Binding]]
bindings d= map getBinding (callGroups d)

mainexp :: [Decl] -> Exp
mainexp ls = nestLet ls (Let (getbinds ls)  (Int 0))

--tcp :: SourceName -> IO [Type_exp]
tcp p = print $ tcheck p
 --do result <- parseFromFile prog f
 --   case result of
 --    Left  e -> print e
 --    Right p -> print $ tcheck p

getbinds :: [Decl] -> [(Id, Exp)]
getbinds ls =  zip (map funcName ls) (map funcRhs ls)

init_env :: [Decl] -> [(Id, Type_scheme)]
init_env ls = ienv ls env_initial [length (env_initial) + 10]

tcheck :: Prog -> ([(Id, Type_exp)], Prog)
tcheck  progr =  (map funtype $ env \\ (init_env decls),decls)
 where  prog  =  introduceLam.introduceBApp.desugarProg.Prelude.supplyPrelude.fst.partitionDec $ progr
        decls =  snd.partitionDec $ progr
        ti gamma es  = fromJust (tcl gamma [length (env_initial) + 10] es)
        env          = trd' $ ti (init_env decls) [mainexp prog]           -- error $ show $ (mainexp prog)--

fst' (x,y,z) = x
snd' (x,y,z) = y
trd' (x,y,z) = z

funtype (fid,SCHEME vs t) = (fid ,t)

list_types ::  Type_exp -> [Type_exp]
list_types (TCONS ""  ts) = ts
list_types _              = []




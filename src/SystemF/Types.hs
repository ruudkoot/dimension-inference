{-# LANGUAGE TypeSynonymInstances #-}
module SystemF.Types where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe

type TyVar = String

data TyCon =
      TyBool
    | TyReal
    deriving (Eq,Ord,Show)

data Ty =
      TyVar  TyVar          -- Type variables
    | TyCon  TyCon          -- Type constants
    | TyFun  Ty Ty          -- Function-space constructor
    deriving (Eq, Show, Ord)

data TyScheme = TyScheme [TyVar] Ty deriving (Eq, Show, Ord)
    
type TySubst = Map.Map TyVar Ty
type TyEnv = Map.Map String TyScheme

{- Types class for free type variables and substitution -}
class Types a where
    ftv :: a -> Set.Set TyVar
    apply :: TySubst -> a -> a
    
instance Types Ty where
    ftv (TyVar n)   = Set.singleton n
    ftv (TyFun l r) = ftv l `Set.union` ftv r
    ftv _           = Set.empty
    
    apply s (TyVar v)    = fromMaybe (TyVar v) $ Map.lookup v s
    apply s (TyFun l r)  = TyFun (apply s l) (apply s r)
    apply s t           = t
    
instance Types TyScheme where
    ftv (TyScheme vars t)   = ftv t Set.\\ (Set.fromList vars)
    apply s (TyScheme vars t) = TyScheme vars $ apply (foldr Map.delete s vars) t -- scoping
    
instance Types a => Types [a] where
    ftv l   = foldr Set.union Set.empty $ map ftv l
    apply s = map (apply s)

instance Types TyEnv where
    ftv env     = ftv (Map.elems env)  
    apply s env = Map.map (apply s) env


{-Auxiliary functions -}    
nullSubst :: TySubst
nullSubst = Map.empty

composeSubst :: TySubst -> TySubst -> TySubst
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1


generalize :: TyEnv -> Ty -> TyScheme
generalize env t = TyScheme vars t
    where vars = Set.toList $ ftv t Set.\\ ftv env
    


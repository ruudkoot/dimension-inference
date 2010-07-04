{-# LANGUAGE TypeSynonymInstances #-}
module SystemF.Types where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Maybe

import Control.Monad
import Control.Monad.State

import SystemF.Dimensions
import SystemF.Substitution

import Debug.Trace

type TyVar = String

data TyCon =
      TyBool
    | TyReal Dim
    deriving (Eq,Ord)

data Ty =
      TyVar  TyVar          -- Type variables
    | TyCon  TyCon          -- Type constants
    | TyFun  Ty Ty          -- Function-space constructor
    deriving (Eq, Ord)

data TyScheme = TyScheme [TyVar] [DimVar] Ty deriving (Eq, Show, Ord)
    
type TySubst = Map.Map TyVar Ty
type TyEnv = Map.Map String TyScheme

{- Types class for free type variables and substitution -}
class Types a where
    ftv :: a -> Set.Set TyVar
    applyTSubst :: TySubst -> a -> a
    
instance Types Ty where
    ftv (TyVar n)   = Set.singleton n
    ftv (TyFun l r) = ftv l `Set.union` ftv r
    ftv _           = Set.empty
    
    applyTSubst s (TyVar v)    = fromMaybe (TyVar v) $ Map.lookup v s
    applyTSubst s (TyFun l r)  = TyFun (applyTSubst s l) (applyTSubst s r)
    applyTSubst s t           = t
    
instance Types TyScheme where
    ftv (TyScheme vars _ t)   = ftv t Set.\\ (Set.fromList vars)
    applyTSubst s (TyScheme vars dims t) = TyScheme vars dims $ applyTSubst (foldr Map.delete s vars) t -- scoping
    
instance Types a => Types [a] where
    ftv l   = foldr Set.union Set.empty $ map ftv l
    applyTSubst s = map (applyTSubst s)

instance Types TyEnv where
    ftv env     = ftv (Map.elems env)  
    applyTSubst s env = Map.map (applyTSubst s) env


-- | Dimension instances for types

instance Dimensions Ty where
    fdv (TyCon (TyReal d)) = fdv d
    fdv (TyFun l r)        = fdv l `Set.union` fdv r
    fdv _                  = Set.empty
    
    applyDimSubst s (TyCon (TyReal d)) = TyCon $ TyReal $ applyDimSubst s d
    applyDimSubst s (TyFun l r)        = TyFun (applyDimSubst s l) (applyDimSubst s r)
    applyDimSubst _ t                  = t

instance Dimensions TyScheme where
    fdv (TyScheme _ dims t)   = fdv t Set.\\ (Set.fromList dims)
    applyDimSubst s (TyScheme vars dims t) = TyScheme vars dims $ applyDimSubst (foldr Map.delete s dims) t -- scoping

instance Dimensions TyEnv where
    fdv env     = fdv (Map.elems env)  
    applyDimSubst s env = Map.map (applyDimSubst s) env

-- | Substitution instance for TySubst
    
instance Substitution TySubst where
    nullSubst = Map.empty
    a <+> b = (Map.map (applyTSubst a) b) `Map.union` a

type Subst = (TySubst, DimSubst)

tSubst :: Subst -> TySubst
tSubst = fst

dimSubst :: Subst -> DimSubst
dimSubst = snd

apply :: (Dimensions a, Types a) => Subst -> a -> a
apply (ts, ds) = applyDimSubst ds . applyTSubst ts

-- | Auxiliary functions    
generalize :: TyEnv -> Ty -> TyScheme
generalize env t = TyScheme vars dims t
    where vars = Set.toList $ ftv t Set.\\ ftv env
          dims = Set.toList $ fdv t Set.\\ fdv env

instantiate :: String -> TyScheme -> Ty
instantiate prefix (TyScheme vars dims t) = 
    let ntvars = map (TyVar . ((prefix ++ "T_") ++). show) [1..]
        tsubst = Map.fromList $ zip vars ntvars
        ndvars = map (DimVar . ((prefix ++ "D_") ++). show) [1..]
        dsubst = Map.fromList $ zip dims ndvars        
    in apply (tsubst,dsubst) t

-- | Unification

mgu :: Ty -> Ty -> Subst
mgu (TyFun l r) (TyFun l' r') = let s1 = mgu l l'
                                    s2 = mgu (apply s1 r) (apply s1 r')
                                in s1 <+> s2
mgu (TyVar u)   t             = (varBind u t, nullSubst)
mgu t           (TyVar u)     = (varBind u t, nullSubst)
mgu (TyCon (TyReal d1)) (TyCon (TyReal d2)) = 
                                case dimUnify d1 d2 of
                                    Nothing -> error $ "Error unifiying dimensions" ++show d1++" -- " ++ show d2 ++ show (dim2nf (DimProd d1 (DimInv d2)))
                                    Just u  -> (nullSubst, u)
                                
mgu (TyCon a)   (TyCon b)     = if a == b 
                                then nullSubst
                                else error $ "Constants don't unify: " ++ show a ++ " vs " ++ show b
mgu t1          t2            = error $ "Types don't unify: " ++ show t1 ++ " vs " ++ show t2


varBind :: String -> Ty -> TySubst
varBind u t | t == TyVar u          = nullSubst
            | u `Set.member` ftv t  = error $ "Occurs check failed:" ++ u ++ " -- " ++ show t
            | otherwise             = Map.singleton u t

-- | Show instances

instance Show TyCon where
    show TyBool = "Bool"
    show (TyReal d) = "Real "++show d

instance Show Ty where    
    show (TyVar v) = v     
    show (TyCon c) = show c
    show (TyFun t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"


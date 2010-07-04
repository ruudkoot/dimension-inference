{-# LANGUAGE TypeSynonymInstances #-}
module SystemF.Dimensions
    {-( DimVar
    , DimCons
    , UnitCons
    , Dim(..)
    , dimensions
    , NormalForm()
    , dim2nf
    , nf2dim
    , DimSubst
    , Dimensions(..)
    , dimUnify
    ) -}where

import           Prelude         hiding (exp)
import qualified Data.Map as Map 
import qualified Data.Set as Set
import Data.Maybe

import SystemF.Substitution

type DimVar  = String

type DimCons  = String
type UnitCons = String

data Dim =
      DimVar DimVar
    | DimCons DimCons
    | DimUnit
    | DimProd Dim Dim
    | DimInv Dim
    deriving (Eq, Ord)

class Dimensions a where
    fdv :: a -> Set.Set DimVar   
    applyDimSubst :: DimSubst -> a -> a

instance Dimensions a => Dimensions [a] where
    fdv l   = foldr Set.union Set.empty $ map fdv l
    applyDimSubst s = map (applyDimSubst s)
    
instance Dimensions Dim where
    fdv  DimUnit        = Set.empty
    fdv (DimProd d1 d2) = fdv d1 `Set.union` fdv d2
    fdv (DimInv  d    ) = fdv d
    fdv (DimVar d    ) = Set.singleton d
    fdv (DimCons d    ) = Set.empty

    applyDimSubst s (DimVar v)  = fromMaybe (DimVar v) $ Map.lookup v s
    applyDimSubst s (DimProd d1 d2) = DimProd (applyDimSubst s d1) (applyDimSubst s d2)
    applyDimSubst s (DimInv d)  = DimInv (applyDimSubst s d)
    applyDimSubst _ d           = d

dimConsts :: Dim -> Set.Set DimCons 
dimConsts  DimUnit        = Set.empty
dimConsts (DimProd d1 d2) = dimConsts d1 `Set.union` dimConsts d2
dimConsts (DimInv  d    ) = dimConsts d
dimConsts (DimVar d    ) = Set.empty
dimConsts (DimCons d    ) = Set.singleton d

exp :: Dim -> Dim -> Integer    
exp  DimUnit        _ = 0
exp (DimProd d1 d2) v = exp d1 v + exp d2 v
exp (DimInv  d    ) v = -exp d v
exp d               v | v == d    = 1
                      | otherwise = 0    
                      
dimensions :: [(DimCons, UnitCons)]
dimensions = [ ("L", "m" )
             , ("T", "s" )
             , ("M", "kg") ]

-- | Normal forms

data NormalForm = NormalForm (Map.Map DimVar Integer) (Map.Map DimCons Integer) deriving Eq

instance Dimensions NormalForm where
    fdv (NormalForm vs _) = Map.keysSet vs
    applyDimSubst s = dim2nf . applyDimSubst s . nf2dim


-- | Converting between forms
    
nf2dim :: NormalForm -> Dim
nf2dim (NormalForm v c) = DimProd (dim' DimVar v) (dim' DimCons c)
    where dim' t = Map.foldWithKey (\k v ac -> DimProd (pwr (t k) v) ac) DimUnit

dim2nf :: Dim -> NormalForm
dim2nf d = NormalForm (Set.fold (\k -> Map.insert k (exp d (DimVar k))) Map.empty (fdv d))
                      (Set.fold (\k -> Map.insert k (exp d (DimCons k))) Map.empty (dimConsts d))
    
pwr :: Dim -> Integer -> Dim
pwr d n | n >  0 = doN (DimProd d) DimUnit n
        | n == 0 = DimUnit
        | n <  0 = DimInv $ pwr d (-n)
        
doN :: (a -> a) -> a -> Integer -> a
doN _ x 0 = x
doN f x n = f $ doN f x (n - 1)
                                  
-- | Auxiliary function on normal form

numVars :: NormalForm -> Integer
numVars (NormalForm v c) = toInteger $ Map.size v

numCons :: NormalForm -> Integer
numCons (NormalForm v c) = toInteger $ Map.size c

varsHead :: NormalForm -> (DimVar, Integer)
varsHead (NormalForm v c) = Map.findMin v

-- map a function on (exponents of) all dimensional constants
consMap :: (Integer -> Integer) -> NormalForm -> Dim
consMap f (NormalForm v c) = nf2dim $ NormalForm v (Map.map f c)

-- map a fuction on (exponents of) all dimensional variables EXCEPT the first and all dimensional constants
tailMap :: (Integer -> Integer) -> NormalForm -> Dim
tailMap f (NormalForm v c) = let Just ((k, a), v') = Map.minViewWithKey v
                             in nf2dim $ NormalForm (Map.insert k a $ Map.map f v') (Map.map f c)
                             
-- | Dimensional substitutions

type DimSubst = Map.Map DimVar Dim

instance Substitution DimSubst where
    nullSubst = Map.empty
    a <+> b = (Map.map (applyDimSubst a) b) `Map.union` a
    
-- | Dimensional unification (p. 45)

dimUnify :: Dim -> Dim -> Maybe DimSubst
dimUnify d1 d2 = dimUnify' (dim2nf (DimProd d1 (DimInv d2)))

dimUnify' :: NormalForm -> Maybe DimSubst
dimUnify' nf | numVars nf == 0 && numCons nf == 0   = Just nullSubst
             | numVars nf == 0 && numCons nf /= 0   = Nothing
             | numVars nf == 1 && varDividesAllCons = Just complexSubst
             | numVars nf == 1                      = Nothing
             | otherwise                            = do u <- return complexerSubst
                                                         s <- dimUnify' (applyDimSubst u nf)
                                                         return $ s <+> u
    where complexSubst :: DimSubst
          complexSubst      = let (d1, x1) = varsHead nf
                               in Map.singleton d1 (consMap (negate . (`div` x1)) nf)
          complexerSubst :: DimSubst
          complexerSubst    = let (d1, x1) = varsHead nf
                               in Map.singleton d1 (tailMap (negate . (`div` x1)) nf)
          varDividesAllCons :: Bool
          varDividesAllCons = let (d1, x1)            = varsHead nf
                                  (NormalForm _ cons) = nf
                               in allValues (\y -> y `mod` x1 == 0) cons

-- | Hulp functies

allValues :: (a -> Bool) -> Map.Map k a -> Bool
allValues p m = Map.fold ((&&) . p) True m

instance Show Dim where
    show = show . dim2nf 
    
instance Show NormalForm where
    show (NormalForm vars cons) = "["++show' vars++show' cons++"]"
        where showPow v n | n == 0 = ""
                          | n < 0  = v ++ show n
                          | otherwise = v ++ "^" ++ show n
              show' m = Map.foldWithKey (\k v ac -> showPow k v ++ ac) "" m

module SystemF.Dimensions
    ( DimVars
    , DimCons
    , UnitCons
    , Dim(..)
    , dimensions
    , NormalForm()
    , nf
    ) where

import           Prelude         hiding (exp)
import qualified Data.Map as Map 
import qualified Data.Set as Set

type DimVars  = String

type DimCons  = String
type UnitCons = String

data Dim =
      DimVars DimVars
    | DimCons DimCons
    | DimUnit
    | DimProd Dim Dim
    | DimInv Dim
    deriving (Eq, Ord, Show)
    
dimensions :: [(DimCons, UnitCons)]
dimensions = [ ("L", "m" )
             , ("T", "s" )
             , ("M", "kg") ]

-- | Normal forms

data NormalForm = NormalForm (Map.Map DimVars Integer) (Map.Map DimCons Integer) deriving (Show)

dim :: NormalForm -> Dim
dim (NormalForm v c) = DimProd (dim' DimVars v) (dim' DimCons c)
    where dim' t = Map.foldWithKey (\k a b -> DimProd (pwr (t k) a) b) DimUnit
    
pwr :: Dim -> Integer -> Dim
pwr d n | n >  0 = doN (DimProd d) DimUnit n
        | n == 0 = DimUnit
        | n <  0 = doN (DimInv . DimProd d) DimUnit n
        
doN :: (a -> a) -> a -> Integer -> a
doN f x n = doN f x (n - 1)

nf :: Dim -> NormalForm
nf d = NormalForm (Set.fold (\k -> Map.insert k (exp d (DimVars k))) Map.empty (baseVars d))
                  (Set.fold (\k -> Map.insert k (exp d (DimCons k))) Map.empty (baseCons d))

baseVars :: Dim -> Set.Set DimVars
baseVars  DimUnit        = Set.empty
baseVars (DimProd d1 d2) = baseVars d1 `Set.union` baseVars d2
baseVars (DimInv  d    ) = baseVars d
baseVars (DimVars d    ) = Set.singleton d
baseVars (DimCons d    ) = Set.empty

baseCons :: Dim -> Set.Set DimCons
baseCons  DimUnit        = Set.empty
baseCons (DimProd d1 d2) = baseCons d1 `Set.union` baseCons d2
baseCons (DimInv  d    ) = baseCons d
baseCons (DimVars d    ) = Set.empty
baseCons (DimCons d    ) = Set.singleton d


exp :: Dim -> Dim -> Integer
exp  DimUnit        _ = 0
exp (DimProd d1 d2) v = exp d1 v + exp d2 v
exp (DimInv  d    ) v = -exp d v
exp d               v | v == d    = 1
                      | otherwise = 0


                                  
-- empty :: NormalForm -> Bool
-- empty (NormalForm v c) = (Map.empty v) && (Map.empty c)

numVars :: NormalForm -> Integer
numVars (NormalForm v c) = toInteger $ Map.size v

numCons :: NormalForm -> Integer
numCons (NormalForm v c) = toInteger $ Map.size c

varsHead :: NormalForm -> (DimVars, Integer)
varsHead (NormalForm v c) = Map.findMin v

-- map a function on (exponents of) all dimensional constants
consMap :: (Integer -> Integer) -> NormalForm -> Dim
consMap f (NormalForm v c) = dim $ NormalForm v (Map.map f c)

-- map a fuction on (exponents of) all dimensional variables EXCEPT the first and all dimensional constants
tailMap :: (Integer -> Integer) -> NormalForm -> Dim
tailMap f (NormalForm v c) = let Just ((k, a), v') = Map.minViewWithKey v
                              in dim $ NormalForm (Map.insert k a $ Map.map f v') (Map.map f c)


-- | Dimensional substitutions

type DimSubst = Map.Map DimVars Dim

idSubst :: DimSubst
idSubst = Map.empty

apply :: DimSubst -> NormalForm -> NormalForm
apply = undefined

(<+>) :: DimSubst -> DimSubst -> DimSubst
(<+>) = undefined

-- | Dimensional unification (p. 45)

dimUnify :: Dim -> Dim -> Maybe DimSubst
dimUnify d1 d2 = dimUnify' (nf (DimProd d1 (DimInv d2)))

dimUnify' :: NormalForm -> Maybe DimSubst
dimUnify' nf | numVars nf == 0 && numCons nf == 0   = Just idSubst
             | numVars nf == 0 && numCons nf /= 0   = Nothing
             | numVars nf == 1 && varDividesAllCons = Just complexSubst
             | numVars nf == 1                      = Nothing
             | otherwise                            = do u <- return complexerSubst
                                                         s <- dimUnify' (apply u nf)
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


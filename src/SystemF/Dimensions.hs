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
    ) -} where

import           Prelude            hiding (exp)
import           Data.Maybe
import           Data.List  as List
import qualified Data.Map   as Map 
import qualified Data.Set   as Set

-- import Debug.Trace

import SystemF.Matrix
import SystemF.Substitution

-- | Dimensions (2.1)
 
type DimVar   = String
type DimCons  = String
type UnitCons = String

data Dim = DimVar  DimVar   -- dimension variables
         | DimCons DimCons  -- base dimensions
         | DimUnit          -- unit dimension
         | DimProd Dim Dim  -- dimension product
         | DimInv  Dim      -- dimension inverse
         deriving (Eq, Ord)
         
instance Show Dim where
    show = show . dim2nf 
         
-- | Base Dimensions and Units

dimensions :: [(DimCons, UnitCons)]
dimensions = [ ("L", "m" )
             , ("T", "s" )
             , ("M", "kg") ]

-- | Normal forms (2.1)

data NormalForm = NormalForm { vars :: Map.Map DimVar  Integer
                             , cons :: Map.Map DimCons Integer } deriving (Eq)
                             
instance Show NormalForm where
    show (NormalForm vars cons) = "["++show' vars++show' cons++"]"
        where showPow v n | n == 0 = ""
                          | n < 0  = v ++ show n
                          | otherwise = v ++ "^" ++ show n
              show' m = Map.foldWithKey (\k v ac -> showPow k v ++ ac) "" m

-- | Converting between forms

dimConsts :: Dim -> Set.Set DimCons 
dimConsts  DimUnit        = Set.empty
dimConsts (DimProd d1 d2) = dimConsts d1 `Set.union` dimConsts d2
dimConsts (DimInv  d    ) = dimConsts d
dimConsts (DimVar d    ) = Set.empty
dimConsts (DimCons d    ) = Set.singleton d

nf2dim :: NormalForm -> Dim
nf2dim (NormalForm v c) = DimProd (dim' DimVar v) (dim' DimCons c)
    where dim' t = Map.foldWithKey (\k v ac -> DimProd (pwr (t k) v) ac) DimUnit

dim2nf :: Dim -> NormalForm
dim2nf d = NormalForm (Set.fold (\k -> addNotZero (exp d (DimVar k)) k) Map.empty (fdv d))
                      (Set.fold (\k -> addNotZero (exp d (DimCons k)) k) Map.empty (dimConsts d))
    where addNotZero 0 k ac = ac
          addNotZero n k ac = Map.insert k n ac
          
pwr :: Dim -> Integer -> Dim
pwr d n | n >  0 = doN (DimProd d) DimUnit n
        | n == 0 = DimUnit
        | n <  0 = DimInv $ pwr d (-n)
        
doN :: (a -> a) -> a -> Integer -> a
doN f x n | n <= 0    = x
          | otherwise = f $ doN f x (n - 1)

-- | Dimensional substitutions

type DimSubst = Map.Map DimVar Dim

instance Substitution DimSubst where
    nullSubst = Map.empty
    a <+> b   = (Map.map (applyDimSubst a) b) `Map.union` a

-- | Functions on dimensions

exp :: Dim -> Dim -> Integer    
exp  DimUnit        _ = 0
exp (DimProd d1 d2) v = exp d1 v + exp d2 v
exp (DimInv  d    ) v = -exp d v
exp d               v | v == d    = 1
                      | otherwise = 0

class Dimensions a where
    fdv           :: a -> Set.Set DimVar   
    simplify      :: Set.Set DimVar -> a -> DimSubst
    applyDimSubst :: DimSubst -> a -> a

instance Dimensions Dim where
    fdv  DimUnit        = Set.empty
    fdv (DimProd d1 d2) = fdv d1 `Set.union` fdv d2
    fdv (DimInv  d    ) = fdv d
    fdv (DimVar  d    ) = Set.singleton d
    fdv (DimCons d    ) = Set.empty

    applyDimSubst s (DimVar v)      = fromMaybe (DimVar v) $ Map.lookup v s
    applyDimSubst s (DimProd d1 d2) = DimProd (applyDimSubst s d1) (applyDimSubst s d2)
    applyDimSubst s (DimInv d)      = DimInv (applyDimSubst s d)
    applyDimSubst _ d               = d
   
    simplify = dimSimplify

instance Dimensions NormalForm where
    fdv (NormalForm vs _) = Map.keysSet vs
    simplify env          = simplify env . nf2dim
    applyDimSubst s       = dim2nf . applyDimSubst s . nf2dim

instance Dimensions a => Dimensions [a] where
    fdv l           = foldr Set.union Set.empty $ map fdv l
    --simplify env    = map (simplify env)
    applyDimSubst s = map (applyDimSubst s)


-- | Auxiliary function on normal form

deleteVar :: DimVar -> NormalForm -> NormalForm
deleteVar v nf = NormalForm (Map.delete v (vars nf)) (cons nf)

numVars :: NormalForm -> Integer
numVars (NormalForm v c) = toInteger $ Map.size v

numCons :: NormalForm -> Integer
numCons (NormalForm v c) = toInteger $ Map.size c

varsHead :: NormalForm -> (DimVar, Integer)
varsHead (NormalForm v c) = Map.foldWithKey minimum (head list) (Map.fromList  (tail list))
        where 
                minimum k1 v1 (k2, v2) | abs v1 < abs v2   = (k1, v1)
                                       | otherwise         = (k2, v2)
                list = Map.toList v  

varsTail :: NormalForm -> NormalForm
varsTail nf = deleteVar (fst (varsHead nf)) nf
              
-- map a function on (exponents of) all dimensional constants
consMap :: (Integer -> Integer) -> NormalForm -> NormalForm
consMap f (NormalForm v c) = NormalForm v (Map.map f c)

varsMap :: (Integer -> Integer) -> NormalForm -> NormalForm
varsMap f (NormalForm v c) = NormalForm (Map.map f v) c

-- map a fuction on (exponents of) all dimensional variables EXCEPT the first and all dimensional constants
tailMap :: (Integer -> Integer) -> NormalForm -> NormalForm
tailMap f nf = let (d1,x1) = varsHead nf
                   tl = varsTail nf
               in NormalForm (Map.insert d1 x1 $ Map.map f (vars tl)) (Map.map f (cons nf))

onlyCons :: NormalForm -> NormalForm
onlyCons (NormalForm _ c) = NormalForm Map.empty c

adjustCons :: NormalForm -> Integer -> NormalForm
adjustCons nf v = consMap (negate . (`div` v)) $ nf

adjustTail :: NormalForm -> Integer -> NormalForm
adjustTail nf v = tailMap (negate . (`div` v)) nf

-- | Dimensional unification (p. 45)

dimUnify :: Dim -> Dim -> Maybe DimSubst
dimUnify d1 d2 = {-trace ("try to unify" ++ show d1 ++ " & " ++ show d2)-} dimUnify' (dim2nf (DimProd d1 (DimInv d2)))

dimUnify' :: NormalForm -> Maybe DimSubst
dimUnify' nf | m == 0 && numCons nf == 0   = Just nullSubst
             | m == 0 && numCons nf /= 0   = Nothing
             | m == 1 && varDividesAllCons = Just . Map.singleton d1 . nf2dim . onlyCons . adjustCons nf $ x1
             | m == 1                      = Nothing
             | otherwise                   = do u <- (return. Map.singleton d1 . nf2dim . adjustTail nf $ x1)
                                                s <- dimUnify' (applyDimSubst u nf)
                                                return $ s <+> u
    where (d1, x1) = varsHead nf
          m = numVars nf

          varDividesAllCons :: Bool
          varDividesAllCons = allValues (\y -> y `mod` x1 == 0) (cons nf)

allValues :: (a -> Bool) -> Map.Map k a -> Bool
allValues p m = Map.fold ((&&) . p) True m

-- | Simplification

dimSimplify :: Set.Set DimVar -> Dim -> DimSubst
dimSimplify env = Map.map replaceCons . dimSimplify' . dim2nf . replaceVars
        where 
              replaceVars :: Dim -> Dim
              replaceVars (DimProd d1 d2) = DimProd (replaceVars d1) (replaceVars d2)
              replaceVars (DimInv d)      = DimInv (replaceVars d)
              replaceVars (DimVar v)      = if Set.member v env
                                            then DimCons v
                                            else DimVar v
              replaceVars t               = t
                                            
              replaceCons :: Dim -> Dim
              replaceCons (DimProd d1 d2) = DimProd (replaceVars d1) (replaceVars d2)
              replaceCons (DimInv d)      = DimInv (replaceVars d)
              replaceCons (DimCons v)     = if Set.member v env
                                            then DimVar v
                                            else DimCons v
              replaceCons t               = t
              
dimSimplify' :: NormalForm -> DimSubst
dimSimplify' nf | m == 0    = nullSubst
                | x1 < 0    = let u1 = Map.singleton d1 (DimInv (DimVar d1))
                                  u2 = dimSimplify' (applyDimSubst u1 nf)
                              in let v = u2 <+> u1
                                 in {-trace (show v)-} v
                | m == 1    = let v = Map.singleton d1 . (DimProd (DimVar d1) ). nf2dim . onlyCons . adjustCons nf $ x1
                              in {-trace (show v) $-} v
                | otherwise = let u1 = Map.singleton d1 . nf2dim . adjustTail nf $ x1
                                  u2 = dimSimplify' (applyDimSubst u1 nf)
                              in let v = u2 <+> u1
                                 in {-trace (show v)-} v
        where   m = {-trace (show nf) $-} numVars nf
                (d1,x1) = varsHead nf
                

-- | Inverting substitutions

type DimOrd = ([DimVar], [DimCons])

invSubst :: DimSubst -> DimSubst
invSubst s = let ordering      = makeOrdering s       -- make arbitry total ordering on vars and cons
                 matrixForm    = toMatrix ordering s  -- compute substitution matrix wrt. ordering
                 inverseMatrix = inverse matrixForm   -- invert
              in fromMatrix ordering inverseMatrix    -- reconstruct substitution
              
    where

        makeOrdering :: DimSubst -> DimOrd
        makeOrdering s = ((Set.toList
                             ((Map.fold Set.union Set.empty (Map.map (Map.keysSet . vars . dim2nf) s))
                                 `Set.union`
                              (Map.keysSet s)
                             )
                         ) , (Set.toList
                             (Map.fold Set.union Set.empty (Map.map (Map.keysSet . cons . dim2nf) s))
                         ))

        toMatrix :: DimOrd -> DimSubst -> Matrix
        toMatrix o@(ov, oc) s = map (\k -> maybe (unitVector o k) (toVector o) (Map.lookup k s)) ov
                                    ++
                                map (unitVector o) oc

            where

                unitVector :: DimOrd -> DimVar -> Vector
                unitVector o@(ov, oc) v = let pos = (ov ++ oc ?? v) + 1         -- WRONG!!! (assumes vars and cons to be distinctly named)
                                              n   = length ov + length oc
                                           in replicate (pos-1) 0 ++ [1] ++ replicate (n-pos) 0

                toVector :: DimOrd -> Dim -> Vector
                toVector o@(ov, oc) d = let normalForm = dim2nf d
                                         in map (\k -> Map.findWithDefault 0 k (vars normalForm)) ov
                                                ++ 
                                            map (\k -> Map.findWithDefault 0 k (cons normalForm)) oc

        -- DOES NOT CHECK IF CONSTANTS STILL MAP TO IDENTITY VECTORS!!!
        fromMatrix :: DimOrd -> Matrix -> DimSubst  
        fromMatrix o@(ov, oc) m = Map.fromList (zip ov (map (fromVector o) m))

            where

                fromVector :: DimOrd -> Vector -> Dim
                fromVector o@(ov, oc) vec = let (vv, vc) = splitAt (length ov) vec
                                                v        = Map.fromList (zip ov vv)
                                                c        = Map.fromList (zip oc vc)
                                             in nf2dim $ NormalForm { vars = v, cons = c }


-- | Utility functions

infix 4 ??

(??) :: Eq a => [a] -> a -> Int
vs ?? v = fromJust (elemIndex v vs)


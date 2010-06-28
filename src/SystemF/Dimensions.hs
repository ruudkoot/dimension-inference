module SystemF.Dimensions
    ( DimVars
    , DimCons
    , Dim(..)
    , dimensions
    , NormalForm()
    , nf
    ) where

import           Prelude         hiding (exp)
import qualified Data.Map as Map 
import qualified Data.Set as Set

type DimVars = String

type DimCons = String

data Dim =
      DimVars DimVars
    | DimCons DimCons
    | DimUnit
    | DimProd Dim Dim
    | DimInv Dim
    deriving (Eq, Ord, Show)
    
dimensions :: [(String, String)]
dimensions = [ ("L", "m" )
             , ("T", "s" )
             , ("M", "kg") ]

newtype NormalForm = NormalForm (Map.Map Dim Integer) deriving (Show)

base :: Dim -> Set.Set Dim
base  DimUnit        = Set.empty
base (DimProd d1 d2) = base d1 `Set.union` base d2
base (DimInv  d    ) = base d
base d               = Set.singleton d

exp :: Dim -> Dim -> Integer
exp  DimUnit        _ = 0
exp (DimProd d1 d2) v = exp d1 v + exp d2 v
exp (DimInv  d    ) v = -exp d v
exp d               v | v == d    = 1
                      | otherwise = 0

nf :: Dim -> NormalForm
nf d = NormalForm $ Set.fold (\k -> Map.insert k (exp d k)) Map.empty (base d)


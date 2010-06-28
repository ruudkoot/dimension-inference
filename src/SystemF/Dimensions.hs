module SystemF.Dimensions where

type DimVars = String

type DimCons = String

data Dim =
      DimVars DimVars
    | DimCons DimCons
    | DimUnit
    | DimProd Dim Dim
    | DimInv Dim
    deriving (Eq,Ord,Show)

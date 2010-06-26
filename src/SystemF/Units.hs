module SystemF.Units where

type UnVar = String

type UnCon = String

data Un =
      UnVar UnVar
    | UnCon UnCon
    | UnUnit
    | UnProd Un Un
    | UnInv Un
    deriving (Eq,Ord,Show)

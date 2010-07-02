module SystemF.Test.Dimensions where

import Control.Monad
import Test.QuickCheck
import SystemF.Dimensions

instance Arbitrary Dim where
    arbitrary = sized tree
        where tree 0 = oneof [ return DimUnit
                             , liftM  DimVars vars
                             , liftM  DimCons cons      ]
              tree n = oneof [ return DimUnit
                             , liftM  DimVars arbitrary
                             , liftM  DimCons arbitrary
                             , liftM  DimInv  sub1
                             , liftM2 DimProd sub2 sub2 ]
                where sub1 = tree (n - 1)
                      sub2 = tree (n `div` 2)
              vars = oneof $ map return [ "a", "b", "c", "d", "e" ]
              cons = oneof $ map return [ "R", "S", "T", "U", "V" ]


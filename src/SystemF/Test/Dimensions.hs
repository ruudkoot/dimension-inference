{-# LANGUAGE TypeSynonymInstances #-}
module SystemF.Test.Dimensions where

import Control.Monad
import Test.QuickCheck
import SystemF.Dimensions
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

instance Arbitrary Dim where
    arbitrary = sized tree
        where tree 0 = oneof [ return DimUnit
                             , liftM  DimVar vars
                             , liftM  DimCons cons      ]
              tree n = oneof [ return DimUnit
                             , liftM  DimVar vars
                             , liftM  DimCons cons
                             , liftM  DimInv  sub1
                             , liftM2 DimProd sub2 sub2 ]
                where sub1 = tree (n - 1)
                      sub2 = tree (n `div` 2)
              vars = oneof $ map return [ "a", "b", "c", "d", "e" ]
              cons = oneof $ map return [ "R", "S", "T", "U", "V" ]

instance Arbitrary DimSubst where
        arbitrary = do k <- listOf (oneof $ map return [ "a", "b", "c", "d", "e" ])
                       v <- listOf (oneof $  map (return . DimVar)  [ "a", "b", "c", "d", "e" ]
                                          ++ map (return . DimCons) [ "R", "S", "T", "U", "V" ])
                       return $ Map.fromList $ zip k v
                       
prop_comm = \x y -> dim2nf (DimProd x y) == dim2nf (DimProd y x)
prop_inv  = \x   -> dim2nf x == dim2nf (DimInv (DimInv x))
prop_idem = \x   -> dim2nf x == dim2nf (nf2dim (dim2nf x))

prop_eq :: Dim -> DimSubst -> Bool
prop_eq x y = isJust $ dimUnify x (applyDimSubst y x)

prop_simplify_var :: Dim -> Bool
prop_simplify_var d = (numVars . dim2nf . applyDimSubst (simplify Set.empty d) $ d) <= 1

prop_simplify_exp :: Dim -> Property
prop_simplify_exp d = let sf = dim2nf . applyDimSubst (simplify Set.empty d) $ d
                      in collect (numVars sf) $ numVars sf == 1 ==> snd (varsHead sf) > 0

prop_subst_id :: Dim -> Bool
prop_subst_id d = let u = simplify Set.empty d
                      uinv = invSubst u
                  in dim2nf (applyDimSubst uinv (applyDimSubst u d)) == dim2nf d
                      
testDim1 = DimProd 
                (DimInv (DimVar "B"))
                (DimVar "C")
                
testDim2 = DimProd (DimVar "A") (DimVar "A")

testDim3 = DimVar "A"
testDim4 = DimProd 
                (DimProd 
                        (DimProd 
                           {-     (DimInv 
                                        (DimProd 
                                                (DimProd DimUnit DimUnit) 
                                                DimUnit
                                        )
                                ) -} DimUnit 
                                (DimProd 
                                        (DimProd 
                                                (DimVar "B") 
                                                 DimUnit
                                        ) 
                                        DimUnit
                                )
                        ) DimUnit
                ) 
                (DimVar "B")
prep d1 d2 = dim2nf (DimProd d1 (DimInv d2))
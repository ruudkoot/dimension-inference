{- quick and dirty square matrices over Z -}

module SystemF.Matrix where

import Control.Category hiding ((.))
import Data.Functor

tmap :: Functor f => (a -> b) -> f (a, a) -> f (b, b)
tmap f = fmap (\(x,y) -> (f x, f y))

type Vector              = [Integer]
type Matrix              = [Vector]
type MatrixHead          = (Vector, Vector)
type AugmentedMatrix     = [(Vector, Vector)]
type AugmentedMatrixHead = ((Vector, Vector), (Vector, Vector))

identityMatrix :: Int -> Matrix
identityMatrix n = (1 : replicate (n-1) 0, replicate (n-1) 0) <:> identityMatrix (n-1)

transpose :: Matrix -> Matrix
transpose = foldr (zipWith (:)) (repeat [])


-- guassian elimination avoiding fractions
inverse :: Matrix -> Matrix
inverse = augment >>> diagonalize 
                  >>> tmap reverse
                  >>> reverse 
                  >>> diagonalize 
                  >>> reverse
                  >>> tmap reverse 
                  >>> normalize   >>> diminish
                  
augment :: Matrix -> AugmentedMatrix
augment m = zip m (identityMatrix (length m))

diminish :: AugmentedMatrix -> Matrix
diminish = map snd
                  
diagonalize :: AugmentedMatrix -> AugmentedMatrix
diagonalize []            = []
diagonalize (m@(m1@(pivot:_), m2):ms) = let firstColumnReduced = if pivot == 0
                                                                 then error "singular matrix (missing pivot)"
                                                                 else m : map reduceRow ms
                                            head               = augmentedMatrixHead firstColumnReduced
                                            tail               = augmentedMatrixTail firstColumnReduced
                                         in head <:|:> diagonalize tail
    where reduceRow (r@(q:qs),r') = (multiplyAndAdd (-q) m1 pivot r, multiplyAndAdd (-q) m2 pivot r')
    
normalize :: AugmentedMatrix -> AugmentedMatrix
normalize [] = []
normalize m = let ((r1@(r:rs),r2),(c1,c2)) = augmentedMatrixHead m
               in ((scalarDivide r r1, scalarDivide r r2),(c1,c2)) <:|:> normalize (augmentedMatrixTail m)


scalarMultiply :: Integer -> Vector -> Vector
scalarMultiply n v = map (n*) v

scalarDivide :: Integer -> Vector -> Vector
scalarDivide n v = map (\x -> let (d,r) = x `divMod` n in if r == 0 then d else error "singular matrix (not integral)") v

multiplyAndAdd :: Integer -> Vector -> Integer -> Vector -> Vector
multiplyAndAdd n v m w = zipWith (+) (scalarMultiply n v) (scalarMultiply m w)


-- the first row and column
matrixHead :: Matrix -> MatrixHead
matrixHead (r:rs) = (r, map head rs)

augmentedMatrixHead :: AugmentedMatrix -> AugmentedMatrixHead
augmentedMatrixHead ((r, r'):rs) = ((r, r'), unzip (tmap head rs))

-- drop the first row and column
matrixTail :: Matrix -> Matrix
matrixTail (_:rs) = map tail rs

augmentedMatrixTail :: AugmentedMatrix -> AugmentedMatrix
augmentedMatrixTail (_:rs) = tmap tail rs

-- compose a head and tail
(<:>) :: MatrixHead -> Matrix -> Matrix
(r, c) <:> t = r : zipWith (:) c t

(<:|:>) :: AugmentedMatrixHead -> AugmentedMatrix -> AugmentedMatrix
((r1, r2), (c1, c2)) <:|:> t = let (t1, t2) = unzip t
                                in zip (r1 : (zipWith (:) c1 t1)) (r2 : (zipWith (:) c2 t2))


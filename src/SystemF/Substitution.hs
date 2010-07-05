module SystemF.Substitution where

infixl 5 <+>

class Substitution a where
    nullSubst  :: a
    (<+>)      :: a -> a -> a
    
instance (Substitution a, Substitution b) => Substitution (a,b) where
    nullSubst = (nullSubst, nullSubst)
    (a1,b1) <+> (a2,b2) = (a1 <+> a2, b1 <+> b2)

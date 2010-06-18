module Main where

import Data.Map

-- | Expressions

type Var = String

data Con = Bool Bool
         | Num  Integer
         
{- Patterns for the case statement. Can be either just a constant or the
   underscore-pattern (Nothing).                                              -}

type Pat = Maybe Con

{- Simply-typed lambda-calculus with term constants, fixed-point combinator,
   let-bindings and case-statements.
   
   Polymorphically-typed lambda-calculus (System F) in comments. I'm not sure
   if want to explicitly type out source or do type inference/reconstruction. 

   Also, we only need let-polymorphism, not rank-n polymorphism.              
   
   We have (non-recursive) let-bindings, a fixed-point combinator and tuples.
   If we want mutually recursive let-bindings we need to desugar them into
   these three constructs.                                                    
   
   To support general algebraic data types we also need a sum and
   mu (recursion) type.                                                       -}
   
data Exp = Var Var                  -- Variables
         | Con Con                  -- Constants
      -- | Tup Exp Exp              -- Tuples (products)
         | Lam Var Exp              -- Lambda-abstraction
      -- | Lam Var Ty  Exp
         | App Exp Exp              -- Application
      -- | TyLam TyVar Exp          -- Type abstraction
      -- | TyApp Exp Ty             -- Type application
         | Fix Exp                  -- Fixed point
         | Let (Map Var Exp) Exp    -- Let-binding
         | Cas (Map Pat Exp) Exp    -- Case-statement


-- | Types

type TyVar = String

data TyCon = TyBool
           | TyNum
         
{- Need type schemes, this allows for rank-n polymorphism -}
data Ty = TyVar  TyVar      -- Type variables
        | TyCon  TyCon      -- Type constants
        | TyTup  Ty    Ty   -- Product-space constructor
     -- | TySum  Ty    Ty
        | TyArr  Ty    Ty   -- Function-space constructor
        | Forall TyVar Ty   -- Universal type
     -- | Mu     TyVar Ty   -- Recursive types


-- | Substitutions

newtype TySubst = TySubst (Map TyVar Ty)

-- | Unification



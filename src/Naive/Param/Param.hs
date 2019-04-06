{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeOperators #-}
{-# Language TypeFamilies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language ConstraintKinds #-}
{-# Language TypeFamilyDependencies #-}
{-# Language FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Naive.Param.Param (
  Params(..)
  , CR(..)
  , ISO(..)
                   ) where

import Data.Proxy (Proxy(..))
import Prelude (Bool(..))

data Params :: [*] -> * where
  Nil :: Params '[]
  Cons :: (IsParams a ~ 'False) => a -> Params b -> Params (a ': b)

type family IsParams (a :: *) :: Bool where
  IsParams (Params _) = 'True
  IsParams _ = 'False

data Left :: [*] -> *
data Mismatch :: [*] -> [*] -> *


type family (a :: [*]) ++ (b :: [*]) = (c :: [*]) where
  '[] ++ '[] = '[]
  '[] ++ (a:b) = (a:b)
  (a':b) ++ c = (a':(b++c))

class (a ++ b ~ c, Sub c a ~ Left b) => CR (a :: [*]) (b :: [*]) (c :: [*]) where
  to' :: Params c -> (Params a,Params b)

instance CR '[] '[] '[] where
  to' Nil = (Nil,Nil)

instance CR '[] (a ':b) (a ':b) where
  to' c = (Nil, c)

instance (CR a b c) => CR (x:a) b (x:c) where
  to' (Cons x y) = let (p,q) = to' y in (Cons x p,q)

type family Sub (a :: [*]) (b :: [*]) :: *  where
  Sub '[] '[] = Left '[]
  Sub '[] (a:b) = Mismatch '[] (a:b)
  Sub (a:b) '[] = Left (a:b)
  Sub (a:b) (a:c) = Sub b c
  Sub (a:b) (c:d) = Mismatch (a:b) (c:d)

merge :: Params a -> Params b -> Params (a ++ b)
merge Nil Nil = Nil
merge Nil p@(Cons _ _) = p
merge (Cons x m) y = Cons x (merge m y)

type family Explode (a :: *) :: [*] where
  Explode (Params a) = a
  Explode a = '[a]

class ((Explode a) ++ (Explode b) ~ c) => Product a b c where
  from :: a -> b -> (Params c)
  to :: (Params c) -> (a,b)

instance (CR a b c) => Product (Params a) (Params b) c where
  from = merge
  to = to'

-- type family Self (a :: [*]) :: Constraint

instance (IsParams a ~ 'False, (Explode a) ++ b ~ (a:b)) => Product a (Params b) (a:b) where
  from a = from (wrap a)
-- • Could not deduce (CR '[] b b) arising from a use of ‘from’
--   from the context: (IsParams a ~ 'False, (Explode a ++ b) ~ (a : b))
--     bound by the instance declaration
--     at /Users/jinxuanzhu/haskell/naive-supervise/src/Naive/Param/Param.hs:76:10-86
-- • In the expression: from (wrap a)
--   In an equation for ‘from’: from a = from (wrap a)
--   In the instance declaration for ‘Product a (Params b) (a : b)’
wrap :: (IsParams a ~ 'False) => a -> Params '[a]
wrap a = Cons a Nil

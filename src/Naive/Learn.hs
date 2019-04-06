module Naive.Learn  (
  Learn(..),
  compose'
) where


import Prelude hiding ((.), id)
import qualified Prelude as P
import Control.Category (Category(..))

data Learn p a b = Learn {
  _u :: p ->a ->b,
  _i :: p ->a ->b ->p,
  _r :: p ->a ->b ->a
                         }

instance Category (Learn p) where
  id = Learn u i r where
    u _ = P.id
    i x _ _ = x
    r _ a _ = a
  (Learn u2 i2 r2) . (Learn u1 i1 r1) = Learn u i r where
    u p a = u2 p (u1 p a)
    i p a c = let b = u1 p a; p' = i2 p b c in i1 p' a b
    r p a c = let b = u1 p a; b' = r2 p b c in r1 p a b'

compose' :: Learn q b c -> Learn p a b -> Learn (p,q) a c
compose' x y = (cr' x) . (cl' y)

cl' :: Learn p a b -> Learn (p,q) a b
cl' (Learn u1 i1 r1) = Learn u i r where
  u (p, _) = u1 p
  i (p, q) a b = (i1 p a b,q)
  r (p, _) = r1 p

cr' :: Learn q a b -> Learn (p,q) a b
cr' (Learn u1 i1 r1) = Learn u i r where
  u (_, q) = u1 q
  i (p, q) a b= (p,i1 q a b)
  r (_, q) = r1 q

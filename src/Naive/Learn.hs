module Naive.Learn  (
  Learn(..),
  view,
  (^.)
) where


import Prelude (($))
-- import qualified Prelude as P
import Control.Category (Category(..))

data Learn p a b = Learn ((p,a) -> ((p,b), (p,b)-> (p,a)))

instance Category (Learn p) where
  id = Learn $ \x -> (x, \y -> y)
  (Learn s2) . (Learn s1) = Learn $ \x ->
    let (xb, br) = s1 x
        (xc, cr)= s2 xb
    in
      (xc, \yc -> br (cr yc))

data Lens a b = Lens (a -> b) (b -> a -> a)

view :: Lens q p -> Learn p a b -> Learn q a b
view (Lens get put) (Learn s) = Learn sq where
  sq (x,a) =
    let
      ((pb, b),br) = s ((get x),a)
    in
      ((put pb x,b), \(x', b') -> let (p', a') = br (get x', b') in (put p' x', a'))

(^.) :: Lens x q -> Lens x p -> Learn q b c -> Learn p a b -> Learn x a c
(^.) lq lp cq cp = (view lq cq) . (view lp cp)

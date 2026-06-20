{- HLINT ignore "Eta reduce" -}

{-

This module defines a parametrised natural numbers object (PNNO) as at
<https://ncatlab.org/nlab/show/natural+numbers+object#withparams>.
The function `iteratorWithState` is essentially what is in Definition 1.1
in

Leopoldo Román, "Cartesian categories with natural numbers object"
Journal of Pure and Applied Algebra **58** issue 3 (1989) pp 267-278
<https://doi.org/10.1016/0022-4049(89)90042-X>

The only things you can do is use zero, apply successor, or
construct families of functions via the iterator, which is the
defining property of a PNNO. The type constructors for Nat are
not exported, only the type itself, and the structural functions
just mentioned.

There is also a helper function that will construct the list of Nats
from zero up to the input Nat, and one that converts a Nat to the
inbuilt Haskell Integer type.

-}

module PNNO
  ( Nat,
    s,
    iteratorWithState,
    zero,
    nums,
    reifyNat,
  )
where

-- | The natural numbers
data Nat = Zero | Successor Nat deriving (Eq)

-- | successor
s :: Nat -> Nat
s = Successor

zero :: Nat
zero = Zero

-- | recursion with parameters
-- This defines a function \z,n -> J_{g,h}(z, n) as:
-- J_{g,h}(p, 0) := g(p)
-- J_{g,h}(p, Sn) := h(p, n, J_{g,h}(p,n))
iteratorWithState :: (a -> b) -> (a -> Nat -> b -> b) -> a -> Nat -> b
iteratorWithState g _ p Zero = g p
iteratorWithState g h p (Successor n) = h p n (iteratorWithState g h p n)

-- | This function authored by Claude
-- Just for pretty printing small numbers in testing
instance Show Nat where
  showsPrec = go
    where
      go _ Zero = showString "0"
      go _ (Successor Zero) = showString "1"
      go _ (Successor (Successor Zero)) = showString "2"
      go _ (Successor (Successor (Successor Zero))) = showString "3"
      go _ (Successor (Successor (Successor (Successor Zero)))) = showString "4"
      go _ (Successor (Successor (Successor (Successor (Successor Zero))))) = showString "5"
      go _ (Successor (Successor (Successor (Successor (Successor (Successor Zero)))))) = showString "6"
      go q (Successor m) = showParen (q > 10) $ showString "Successor " . go 11 m

-- | make a list of Nums, n |--> [0, one, ...., n]
nums :: Nat -> [Nat]
nums Zero = [zero]
nums (Successor n) = nums n ++ [s n]

-- | Convert a Nat to a Haskell Integer
reifyNat :: Nat -> Integer
reifyNat Zero = 0
reifyNat (Successor n) = succ (reifyNat n)

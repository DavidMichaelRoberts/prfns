{-

This module defines a parametrised natural numbers object as at
<https://ncatlab.org/nlab/show/natural+numbers+object#withparams>
as well as some minor pretty-printing capabilities

The only things you should be able to do is call a constructor, apply
successor, or construction families of functions via the iterator,
which is the defining property of a PNNO.

-}

module PNNO
  ( Nat (..),
    s,
    iteratorWithState,
  )
where

-- | The natural numbers
data Nat = Zero | Successor Nat deriving (Eq)

-- | successor
s :: Nat -> Nat
s = Successor

-- | recursion with parameters
-- This defines a function \z,n -> J_{g,h}(z, n) as:
-- J_{g,h}(p, 0) := g(p)
-- J_{g,h}(p, Sn) := h(p, n, J_{g,h}(p,n))
iteratorWithState :: (a -> x) -> (a -> Nat -> x -> x) -> a -> Nat -> x
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

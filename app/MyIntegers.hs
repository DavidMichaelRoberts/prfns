module MyIntegers where

import Functions
import PNNO (Nat, reifyNat, zero)
import Prelude hiding (id, mod, (*), (+))

newtype MyInt = MyInt Nat deriving (Eq, Show)

retract :: (Nat, Nat) -> MyInt
retract (n, m) = MyInt (pairToCode (n, m))

include :: MyInt -> (Nat, Nat)
include (MyInt n) = decodeToPair n

intZero :: MyInt
intZero = MyInt zero

intSucc :: MyInt -> MyInt
intSucc (MyInt n) = MyInt (rightShiftCode n)

intPred :: MyInt -> MyInt
intPred (MyInt n) = MyInt (leftShiftCode n)

----------------------------------------------------------------------------

-- * Integer arithmetic

----------------------------------------------------------------------------

intPlus :: MyInt -> MyInt -> MyInt
intPlus (MyInt n) (MyInt m) = MyInt (pairToCode (decodeToPair n `pairPlus` decodeToPair m))

neg :: MyInt -> MyInt
neg (MyInt n) = MyInt (pairToCode $ swap $ decodeToPair n)

intMinus :: MyInt -> MyInt -> MyInt
intMinus n m = intPlus n (neg m)

natAsInt :: Nat -> MyInt
natAsInt n = retract (n, zero)

natToNonPosInt :: Nat -> MyInt
natToNonPosInt n = retract (zero, n)

----------------------------------------------------------------------------

-- * Convert to Haskell native Integer

----------------------------------------------------------------------------

-- | helper function to calculate the "real difference"
diffAsInt :: (Nat, Nat) -> Integer
diffAsInt (n, m) = reifyNat n - reifyNat m

reify :: MyInt -> Integer
reify = diffAsInt . include

deReify :: Integer -> MyInt
deReify n
  | n == 0 = intZero
  | n > 0 = intSucc $ deReify (pred n)
  | otherwise = neg $ deReify (-n)

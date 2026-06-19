{- HLINT ignore "Eta reduce" -}

{-
The aim of this module is to construct a function (Nat, Nat) -> Nat
that acts as \n,m -> "n - m" except I don't have the integers, I only
have *codes* for integers as natural numbers (even = positive,
odd = negative), where all the functions in sight are primitive recursive,
built from the ground up, using only the capabilities exported from
the module PNNO defining a parametrised natural numbers object
-}

module Functions where

import PNNO (Nat (..), iterator, s)
import Prelude hiding (id, mod, (*), (+))

----------------------------------------------------------------------------

-- * Simplified iterators and basic constructs/constants

----------------------------------------------------------------------------

iteratorNoState :: (a -> x) -> (x -> x) -> a -> Nat -> x
iteratorNoState g f = iterator g (\_ _ q -> f q)

iteratorNoParam :: x -> (x -> x) -> Nat -> x
iteratorNoParam z0 f = iteratorNoState (\() -> z0) f ()

-- | constant functions
constZero, constOne :: Nat -> Nat
constZero _ = zero
constOne = s . constZero

-- | small constants
zero, one, two, three, four, five, six :: Nat
zero = Zero
one = s zero
two = s one
three = s two
four = s three
five = s four
six = s five

-- | The identity function
id :: Nat -> Nat
id n = n

-- | predecessor
predeccessor :: Nat -> Nat
predeccessor Zero = zero
predeccessor (Successor n) = n

----------------------------------------------------------------------------

-- * Basic arithmetic

----------------------------------------------------------------------------

-- | addition
plus :: Nat -> Nat -> Nat
plus m n = iteratorNoState id s m n

(+) :: Nat -> Nat -> Nat
(+) = plus

-- | multiplication
multiply :: Nat -> Nat -> Nat
multiply m n = iteratorNoState constZero (plus m) m n

(*) :: Nat -> Nat -> Nat
(*) = multiply

-- | truncated minus, m `monus` n = m - n IF ≥ 0, otherwise 0
monus :: Nat -> Nat -> Nat
monus m n = iteratorNoState id predeccessor m n

-- | absolute difference function
absDiff :: Nat -> Nat -> Nat
absDiff m n = (n `monus` m) + (m `monus` n)

----------------------------------------------------------------------------

-- * Predicates, valued 0, 1 :: Nat

----------------------------------------------------------------------------

-- | tests if zero
isZero :: Nat -> Nat
isZero n = one `monus` n

-- | tests if equal
eq :: Nat -> Nat -> Nat
eq m n = isZero (absDiff m n)

-- | tests if unequal
notEq :: Nat -> Nat -> Nat
notEq m n = isZero (eq m n)

-- | tests if nonzero
nonZero :: Nat -> Nat
nonZero = notEq zero

-- | lt m n is (m < n)
lt :: Nat -> Nat -> Nat
lt m n = nonZero (n `monus` m)

-- | geq m n is (m < n)
gt :: Nat -> Nat -> Nat
gt m n = nonZero (m `monus` n)

-- | leq m n is (m ≤ n)
leq :: Nat -> Nat -> Nat
leq m n = lt m (s n)

-- | geq m n is (m ≥ n)
geq :: Nat -> Nat -> Nat
geq m n = gt (s m) n

-- | test if two pairs are equal
eqPairs :: (Nat, Nat) -> (Nat, Nat) -> Nat
eqPairs (n, m) (k, l) = eq n k * eq m l

----------------------------------------------------------------------------

-- * Some more arithmetic

----------------------------------------------------------------------------

-- | remainder of division: the number r with 0 ≤ r < m such that n = km + r
-- or else if m = 0, just return
-- this is n - (n % m), but we define it directly
remainder :: Nat -> Nat -> Nat
remainder m n = iteratorNoState constZero (\y -> s y * (y `lt` predeccessor m)) m n

-- | for the usual infix notation, n `mod` m
mod :: Nat -> Nat -> Nat
mod = flip remainder

-- | parity of a natural number
parity :: Nat -> Nat
parity n = n `mod` two

-- | truncated division
-- divide m n = n % m, the largest integer q=n % m such that m * q ≤ n
-- note that the arguments are the wrong way around!
divide :: Nat -> Nat -> Nat
divide m n = iterator constZero h m n
  where
    h l' m' n' = nonZero l' * (n' + isZero (s m' `mod` l'))

-- | sensible infix operator for divide
(%) :: Nat -> Nat -> Nat
(%) = flip divide

----------------------------------------------------------------------------

-- * Coding the integers

----------------------------------------------------------------------------

-- | this retuns a 'canonical' representative for an integer
-- n - m = (n `monus` m) - (m `monus` n) when calculated in Int
canonicalRep :: (Nat, Nat) -> (Nat, Nat)
canonicalRep (n, m) = (n `monus` m, m `monus` n)

-- | this returns 0 if applied to an odd number
decodeFromEven :: Nat -> Nat
decodeFromEven n = (n % two) * isZero (parity n)

-- | this returns 0 if applied to an even number
decodeFromOdd :: Nat -> Nat
decodeFromOdd n = (s n % two) * nonZero (parity n)

pairToCode :: (Nat, Nat) -> Nat
pairToCode (n, m) = encode $ canonicalRep (n, m)
  where
    encode (k, l) = k * two + predeccessor (l * two)

decodeToPair :: Nat -> (Nat, Nat)
decodeToPair n = (decodeFromEven n, decodeFromOdd n)

-- | this should be "add 1"
rightShift :: (Nat, Nat) -> (Nat, Nat)
rightShift (n, m) = (s n, m)

-- | this should be "subtract 1"
leftShift :: (Nat, Nat) -> (Nat, Nat)
leftShift (n, m) = (n, s m)

-- | "plus 1" on coded integers
rightShiftCode :: Nat -> Nat
rightShiftCode = pairToCode . rightShift . decodeToPair

-- | "minus 1" on coded integers
leftShiftCode :: Nat -> Nat
leftShiftCode = pairToCode . leftShift . decodeToPair

leftThenRightOnCodes :: Nat -> Nat
leftThenRightOnCodes = rightShiftCode . leftShiftCode

rightThenLeftOnCodes :: Nat -> Nat
rightThenLeftOnCodes = leftShiftCode . rightShiftCode

leftRightComparison :: Nat -> Nat
leftRightComparison n = eq n (leftThenRightOnCodes n)

rightLeftComparison :: Nat -> Nat
rightLeftComparison n = eq n (rightThenLeftOnCodes n)

-- | round-trip from code to a pair back to code
decodeEncode :: Nat -> Nat
decodeEncode = pairToCode . decodeToPair

-- | should return 1 always
decodeEncodeComparison :: Nat -> Nat
decodeEncodeComparison n = eq n (pairToCode (decodeToPair n))

-- round trip from a pair to code then to a pair
-- this won't be the identity map!
encodeDecode :: (Nat, Nat) -> (Nat, Nat)
encodeDecode = decodeToPair . pairToCode

-- | should return 1 always
-- note the canonicalRep!
encodeDecodeComparison :: (Nat, Nat) -> Nat
encodeDecodeComparison (n, m) = eqPairs (canonicalRep (n, m)) (encodeDecode (n, m))

-- | for inspecting the output for the round trip "encode then decode"
encodeDecodeComparisonRaw :: (Nat, Nat) -> ((Nat, Nat), (Nat, Nat))
encodeDecodeComparisonRaw (n, m) = (canonicalRep (n, m), encodeDecode (n, m))

-- | Add a pair of Nats
pairPlus :: (Nat, Nat) -> (Nat, Nat) -> (Nat, Nat)
pairPlus (n, m) (k, l) = (n + k, m + l)

-- | Swap a pair of nats
swap :: (Nat, Nat) -> (Nat, Nat)
swap (n, m) = (m, n)

----------------------------------------------------------------------------

-- * Integer arithmetic

----------------------------------------------------------------------------

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

-- | Convert a Nat to a Haskell Integer
reifyNat :: Nat -> Integer
reifyNat Zero = 0
reifyNat (Successor n) = succ (reifyNat n)

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

----------------------------------------------------------------------------

-- * Testing code

----------------------------------------------------------------------------

-- | make a list of Nums as long as you want
nums :: Nat -> [Nat]
nums Zero = [zero]
nums (Successor n) = nums n ++ [s n]

-- | make a bunch of ordered pairs
pairs :: Nat -> Nat -> [(Nat, Nat)]
pairs n m = [(i, j) | i <- nums n, j <- nums m]

-- | turn some codes into representative pairs
decodeSample :: Nat -> [(Nat, Nat)]
decodeSample n = map decodeToPair (nums n)

-- | turn some representative pairs into codes
encodeSample :: Nat -> Nat -> [Nat]
encodeSample n m = map pairToCode (pairs n m)

testDecodeEncode :: Nat -> [Nat]
testDecodeEncode n = map decodeEncodeComparison (nums n)

testEncodeDecode :: Nat -> Nat -> [Nat]
testEncodeDecode n m = map encodeDecodeComparison (pairs n m)

inspectEncodeDecode :: Nat -> Nat -> [((Nat, Nat), (Nat, Nat))]
inspectEncodeDecode n m = map encodeDecodeComparisonRaw (pairs n m)

testLeftRightShift :: Nat -> [Nat]
testLeftRightShift n = map leftRightComparison (nums n)

testRightLeftShift :: Nat -> [Nat]
testRightLeftShift n = map rightLeftComparison (nums n)

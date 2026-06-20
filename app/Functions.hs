{- HLINT ignore "Eta reduce" -}

{-
The aim of this module is to construct a function (Nat, Nat) -> Nat
that acts as \n,m -> "n - m" except I don't have the integers, I only
have *codes* for integers as natural numbers (even = positive,
odd = negative), where all the functions in sight are primitive recursive,
built from the ground up, using only the capabilities exported from
the module PNNO defining a parametrised natural numbers object.
-}

module Functions where

import PNNO (Nat, iteratorWithState, nums, s, zero)
import Prelude hiding (id, mod, (*), (+))

----------------------------------------------------------------------------

-- * Simplified iterators and basic constructs/constants

----------------------------------------------------------------------------

iteratorNoState :: (a -> b) -> (b -> b) -> a -> Nat -> b
iteratorNoState g f = iteratorWithState g (\_ _ q -> f q)

iteratorNoParam :: b -> (b -> b) -> Nat -> b
iteratorNoParam z f = iteratorNoState (\() -> z) f ()

-- | constant functions
constZero, constOne :: a -> Nat
constZero _ = zero
constOne = s . constZero

-- | small positive constants
one, two, three, four, five, six :: Nat
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
predecessor :: Nat -> Nat
predecessor n = iteratorWithState constZero (\_ k _ -> k) () n

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
monus m n = iteratorNoState id predecessor m n

-- | absolute difference function
absDiff :: Nat -> Nat -> Nat
absDiff m n = (n `monus` m) + (m `monus` n)

-- | remainder of division: the number r with 0 ≤ r < m such that n = km + r
-- or else if m = 0, just return
-- this is n - (n % m), but we define it directly
remainder :: Nat -> Nat -> Nat
remainder m n = iteratorNoState constZero (\y -> s y * (y `lt` predecessor m)) m n

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
divide m n = iteratorWithState constZero h m n
  where
    h l' m' n' = nonZero l' * (n' + isZero (s m' `mod` l'))

-- | sensible infix operator for divide
(%) :: Nat -> Nat -> Nat
(%) = flip divide

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

isEven :: Nat -> Nat
isEven = isZero . parity

isOdd :: Nat -> Nat
isOdd = nonZero . parity

----------------------------------------------------------------------------

-- * Working with pairs of Nats

----------------------------------------------------------------------------

-- | test if two pairs are equal
eqPairs :: (Nat, Nat) -> (Nat, Nat) -> Nat
eqPairs (n, m) (k, l) = eq n k * eq m l

-- | scalar multiply
scalarMult :: Nat -> (Nat, Nat) -> (Nat, Nat)
scalarMult k (m, n) = (k * m, k * n)

-- | Add a pair of Nats
pairPlus :: (Nat, Nat) -> (Nat, Nat) -> (Nat, Nat)
pairPlus (n, m) (k, l) = (n + k, m + l)

-- | Swap a pair of nats
swap :: (Nat, Nat) -> (Nat, Nat)
swap (n, m) = (m, n)

----------------------------------------------------------------------------

-- * Coding the integers

----------------------------------------------------------------------------

-- | this retuns a 'canonical' representative for an integer
-- n - m = (n `monus` m) - (m `monus` n) when calculated in Int
-- this function is idempotent
canonicalRep :: (Nat, Nat) -> (Nat, Nat)
canonicalRep (n, m) = (n `monus` m, m `monus` n)

-- | retraction part of the splitting of canonicalRep
pairToCode :: (Nat, Nat) -> Nat
pairToCode (n, m) = encode $ canonicalRep (n, m)
  where
    encode (k, l) = k * two + predecessor (l * two)

-- | inclusion part of the splitting of canonicalRep
decodeToPair :: Nat -> (Nat, Nat)
decodeToPair n = scalarMult (s n % two) (isEven n, isOdd n)

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

----------------------------------------------------------------------------

-- * Testing code

----------------------------------------------------------------------------

-- | make a bunch of ordered pairs
pairs :: Nat -> Nat -> [(Nat, Nat)]
pairs n m = [(i, j) | i <- nums n, j <- nums m]

-- | turn some codes into representative pairs
decodeSample :: Nat -> [(Nat, Nat)]
decodeSample n = map decodeToPair (nums n)

-- | turn some representative pairs into codes
encodeSample :: Nat -> Nat -> [Nat]
encodeSample n m = map pairToCode (pairs n m)

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

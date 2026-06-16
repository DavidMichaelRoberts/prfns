{-
The aim of this module is to construct a function (Nat, Nat) -> Nat
that acts as \n,m -> "n - m" except I don't have the integers, I only
have *codes* for integers as natural numbers (even = positive,
odd = negative), where all the functions in sight are primitive recursive,
build from the ground up. I only allow myself to use Haskell's mechanism
to define Nat inductively,

-}

module Functions where

import Prelude hiding (id, (*), (+))

----------------------------------------------------------------------------

-- * Basic definitions and sample data

----------------------------------------------------------------------------

-- | The natural numbers
data Nat = Zero | Successor Nat deriving (Eq)

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

-- | successor
s :: Nat -> Nat
s = Successor

-- | zero, one and two
zero, one, two, three, four, five, six :: Nat
zero = Zero
one = s zero
two = s one
three = s two
four = s three
five = s four
six = s five

nums :: [Nat]
nums = [zero, one, two, three, four, five, six]

smallNums :: [Nat]
smallNums = [zero, one, two, three]

samplePairs :: [(Nat, Nat)]
samplePairs = [(n, m) | n <- smallNums, m <- smallNums]

-- | constant functions
constZero, constOne :: Nat -> Nat
constZero _ = zero
constOne _ = one

-- | recursion
-- Thanks Tony for this one!
iterate' :: x -> (x -> x) -> Nat -> x
iterate' z _ Zero = z
iterate' z f (Successor n) = let fx = f z in fx `seq` iterate' fx f n

-- | recursion with parameters
-- This defines a function \z,n -> J(z,n) as:
-- J(z,0) := g(z)
-- J(z,Sn) := f(J(z,n))
iterateWithParams :: (a -> x) -> (x -> x) -> a -> Nat -> x
iterateWithParams g _ p Zero = g p
iterateWithParams g f p (Successor n) = let fpx = f (g p) in fpx `seq` iterate' fpx f n

-- | recursion with even more parameters
-- This defines a function \z,n -> J(z,n) as:
-- J(z,0) := g0(z)
-- J(z,Sn) := h(z,n,J(z,n))
iterateWithMoreParams :: (a -> x) -> (a -> Nat -> x -> x) -> a -> Nat -> x
iterateWithMoreParams g0 h p n = pr3 $ iterateWithParams g f p n
  where
    -- g :: a -> (a, Nat, x)
    g z = (z, zero, g0 z)
    -- f :: (a, Nat, x) -> (a, Nat, x)
    f (z, n', q) = (z, s n', h z n' q)
    -- project on third entry
    pr3 (_, _, q) = q

-- | recursion with no parameter after all
iterate'' :: x -> (x -> x) -> Nat -> x
iterate'' z0 f = iterateWithParams (\() -> z0) f ()

-- shorthand
j :: x -> (x -> x) -> Nat -> x
j = iterate''

-- | The identity function
id :: Nat -> Nat
id n = n

----------------------------------------------------------------------------

-- * Basic arithmetic

----------------------------------------------------------------------------

-- | addition
myPlus :: Nat -> Nat -> Nat
myPlus m n = iterateWithParams id s m n

(+) :: Nat -> Nat -> Nat
(+) = myPlus

-- | multiplication
myMultiply :: Nat -> Nat -> Nat
myMultiply m n = iterateWithParams constZero (myPlus m) m n

(*) :: Nat -> Nat -> Nat
(*) = myMultiply

-- | truncated minus 1
predeccessor :: Nat -> Nat
predeccessor Zero = zero
predeccessor (Successor n) = n

-- | truncated minus, m `monus` n = m - n IF ≥ 0, otherwise 0
monus :: Nat -> Nat -> Nat
monus m n = iterateWithParams id predeccessor m n

-- | absolute difference function
absDiff :: Nat -> Nat -> Nat
absDiff m n = (n `monus` m) + (m `monus` n)

----------------------------------------------------------------------------

-- * Predicates, valued 0, 1 :: Nat

----------------------------------------------------------------------------

-- | tests if equal
eq :: Nat -> Nat -> Nat
eq m n = isZero (absDiff m n)

-- | tests if unequal
notEq :: Nat -> Nat -> Nat
notEq m n = isZero (eq m n)

-- | tests if nonzero
nonZero :: Nat -> Nat
nonZero Zero = zero
nonZero _ = one

-- | tests if zero
isZero :: Nat -> Nat
isZero Zero = one
isZero _ = zero

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
eqPairs (n, m) (k, l) = eq n m * eq k l

----------------------------------------------------------------------------

-- * Some more arithmetic

----------------------------------------------------------------------------

-- | parity of a nat
-- This should be the same as (remainder two)
parity :: Nat -> Nat
parity n = iterate'' zero isZero n

-- | remainder of division: the number r with 0 ≤ r < m such that n = km + r
-- or else if m = 0, just return
-- this is n - (n % m), but we define it directly
remainder :: Nat -> Nat -> Nat
remainder m n = iterateWithParams constZero (\y -> (s y) * (y `lt` (predeccessor m))) m n

-- | truncated division
-- divide n m = n % m, the largest integer q=n%m such that m * q ≤ n
divide :: Nat -> Nat -> Nat
divide m n = iterateWithMoreParams constZero h m n
  where
    h l' m' n' = nonZero l' * (n' + isZero (remainder l' (s m')))

----------------------------------------------------------------------------

-- * Coding the integers

----------------------------------------------------------------------------

-- | this retuns a 'canonical' representative for an integer
-- n - m = (n `monus` m) - (m `monus` n) when calculated in Int
canonicalRep :: (Nat, Nat) -> (Nat, Nat)
canonicalRep (n, m) = (n `monus` m, m `monus` n)

encodeAsEven :: Nat -> Nat
encodeAsEven n = n * two

-- | this returns 0 if applied to an odd number
-- TODO: I think this will even work with `divide two n`
-- replaced by `divide two (s n)`, making the definition
-- of decodeToPair
decodeFromEven :: Nat -> Nat
decodeFromEven n = divide two n * isZero (remainder two n)

-- | never apply this to Zero!
encodeAsOdd :: Nat -> Nat
encodeAsOdd Zero = undefined
encodeAsOdd (Successor n) = predeccessor (s n * two)

-- | this returns 0 if applied to an even number
decodeFromOdd :: Nat -> Nat
decodeFromOdd n = divide two (s n) * nonZero (remainder two n)

pairToCode :: (Nat, Nat) -> Nat
pairToCode (n, Zero) = encodeAsEven n
pairToCode (n, Successor m) = encodeSum $ canonicalRep (n, s m)
  where
    encodeSum (n', Zero) = encodeAsEven n'
    encodeSum (n', Successor k) = encodeAsEven n' + encodeAsOdd (s k)

decodeToPair :: Nat -> (Nat, Nat)
decodeToPair n = (decodeFromEven n, decodeFromOdd n)

-- | this should be "add 1"
rightShift :: (Nat, Nat) -> (Nat, Nat)
rightShift (n, m) = (s n, m)

-- | this should be "subtract 1"
leftShift :: (Nat, Nat) -> (Nat, Nat)
leftShift (n, m) = (n, s m)

-- untested, should be "plus 1" on coded integers
rightShiftCode :: Nat -> Nat
rightShiftCode = pairToCode . rightShift . decodeToPair

-- untested, should be "minus 1" on coded integers
leftShiftCode :: Nat -> Nat
leftShiftCode = pairToCode . leftShift . decodeToPair

-- round-trip from code to a pair back to code
decodeEncode :: Nat -> Nat
decodeEncode = pairToCode . decodeToPair

-- should return 1 always
decodeEncodeComparison :: Nat -> Nat
decodeEncodeComparison n = eq n (pairToCode (decodeToPair n))

-- round trip from a pair to code then to a pair
-- this won't be the identity map!
encodeDecode :: (Nat, Nat) -> (Nat, Nat)
encodeDecode = decodeToPair . pairToCode

-- should return 1 always
-- WIP
encodeDecodeComparison :: (Nat, Nat) -> Nat
encodeDecodeComparison (n, m) = eqPairs (canonicalRep (n, m)) (encodeDecode (n, m))

----------------------------------------------------------------------------

-- * Testing code

----------------------------------------------------------------------------

-- | turn some codes into representative pairs
decodeSample :: [(Nat, Nat)]
decodeSample = map decodeToPair nums

-- | turn some representative pairs into codes
encodeSample :: [Nat]
encodeSample = map pairToCode samplePairs

test1 :: [Nat]
test1 = map decodeEncodeComparison nums

-- currently broken! Need to fix
test2 :: [Nat]
test2 = map encodeDecodeComparison samplePairs

{-
The aim of this module is to construct a function (Nat, Nat) -> Nat
that acts as \n,m -> "n - m" except I don't have the integers, I only
have *codes* for integers as natural numbers (even = positive,
odd = negative), where all the functions in sight are primitive recursive,
build from the ground up. I only allow myself to use Haskell's mechanism
to define Nat inductively, and its recursion capabilities. Then everything
is defined using this one tool, which amounts to the definition of
a natural numbers object in a cartesian (closed) category
<https://ncatlab.org/nlab/show/natural+numbers+object#withparams>.
-}

module Functions where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
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
-- This defines a function \z,n -> J_g,f(z,n) as:
-- J_g,f(z,0) := g(z)
-- J_g,f(z,Sn) := f(J(z,n))
iterateWithParams :: (a -> x) -> (x -> x) -> a -> Nat -> x
iterateWithParams g _ p Zero = g p
iterateWithParams g f p (Successor n) = let fpx = f (g p) in fpx `seq` iterate' fpx f n

-- | recursion with even more parameters
-- This defines a function \z,n -> J_g,h(z, n) as:
-- J_g,h(z, 0) := g(z)
-- J_g,h(z, Sn) := h(z, n, J_g,h(z,n))
iterateWithMoreParams :: (a -> x) -> (a -> Nat -> x -> x) -> a -> Nat -> x
iterateWithMoreParams g h p n = pr3 $ iterateWithParams g' f p n
  where
    -- g :: a -> (a, Nat, x)
    g' z = (z, zero, g z)
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
plus :: Nat -> Nat -> Nat
plus m n = iterateWithParams id s m n

(+) :: Nat -> Nat -> Nat
(+) = plus

-- | multiplication
multiply :: Nat -> Nat -> Nat
multiply m n = iterateWithParams constZero (plus m) m n

(*) :: Nat -> Nat -> Nat
(*) = multiply

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

-- | tests if nonzero
nonZero :: Nat -> Nat
nonZero Zero = zero
nonZero _ = one

-- | tests if zero
isZero :: Nat -> Nat
isZero Zero = one
isZero _ = zero

-- | tests if equal
eq :: Nat -> Nat -> Nat
eq m n = isZero (absDiff m n)

-- | tests if unequal
notEq :: Nat -> Nat -> Nat
notEq m n = isZero (eq m n)

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
pairToCode (n, Zero) = encodeAsEven n -- get rid of this one?
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

leftThenRightOnCodes :: Nat -> Nat
leftThenRightOnCodes = rightShiftCode . leftShiftCode

rightThenLeftOnCodes :: Nat -> Nat
rightThenLeftOnCodes = leftShiftCode . rightShiftCode

leftRightComparison :: Nat -> Nat
leftRightComparison n = eq n (leftThenRightOnCodes n)

rightLeftComparison :: Nat -> Nat
rightLeftComparison n = eq n (rightThenLeftOnCodes n)

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
-- note the canonicalRep!
encodeDecodeComparison :: (Nat, Nat) -> Nat
encodeDecodeComparison (n, m) = eqPairs (canonicalRep (n, m)) (encodeDecode (n, m))

-- for inspecting the output
encodeDecodeComparisonRaw :: (Nat, Nat) -> ((Nat, Nat), (Nat, Nat))
encodeDecodeComparisonRaw (n, m) = (canonicalRep (n, m), encodeDecode (n, m))

pairPlus :: (Nat, Nat) -> (Nat, Nat) -> (Nat, Nat)
pairPlus (n, m) (k, l) = (n + k, m + l)

inv :: (Nat, Nat) -> (Nat, Nat)
inv (n, m) = (m, n)

----------------------------------------------------------------------------

-- * Integer arithmetic

----------------------------------------------------------------------------

newtype MyInt = MyInt Nat deriving (Eq)

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
neg (MyInt n) = MyInt (pairToCode $ inv $ decodeToPair n)

intMinus :: MyInt -> MyInt -> MyInt
intMinus n m = intPlus n (neg m)

----------------------------------------------------------------------------

-- * Convert to Haskell native Int

----------------------------------------------------------------------------

-- | Convert a Nat to a Haskell Int
reify :: Nat -> Int
reify Zero = 0
reify (Successor n) = succ (reify n)

-- | helper function to calculate the "real difference"
diffAsInt :: (Nat, Nat) -> Int
diffAsInt (n, m) = reify n - reify m

reifyInt :: MyInt -> Int
reifyInt (MyInt n) = diffAsInt $ decodeToPair n

----------------------------------------------------------------------------

-- * Testing code

----------------------------------------------------------------------------

-- | make a list of Nums as long as you want
nums' :: Nat -> [Nat]
nums' n = NE.toList (iterate' (NE.singleton zero) (\l -> NE.append l (s (NE.last l) :| [])) n)

-- | make a bunch of ordered pairs
pairs' :: Nat -> Nat -> [(Nat, Nat)]
pairs' n m = [(i, j) | i <- nums' n, j <- nums' m]

-- | turn some codes into representative pairs
decodeSample :: Nat -> [(Nat, Nat)]
decodeSample n = map decodeToPair (nums' n)

-- | turn some representative pairs into codes
encodeSample :: Nat -> Nat -> [Nat]
encodeSample n m = map pairToCode (pairs' n m)

testDecodeEncode :: Nat -> [Nat]
testDecodeEncode n = map decodeEncodeComparison (nums' n)

testEncodeDecode :: Nat -> Nat -> [Nat]
testEncodeDecode n m = map encodeDecodeComparison (pairs' n m)

inspectEncodeDecode :: Nat -> Nat -> [((Nat, Nat), (Nat, Nat))]
inspectEncodeDecode n m = map encodeDecodeComparisonRaw (pairs' n m)

testLeftRightShift :: Nat -> [Nat]
testLeftRightShift n = map leftRightComparison (nums' n)

testRightLeftShift :: Nat -> [Nat]
testRightLeftShift n = map rightLeftComparison (nums' n)

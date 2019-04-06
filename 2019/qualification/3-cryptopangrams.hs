-- GCJ 2019, Qualification: Problem 3 - Cryptopangrams
--
-- By Matt Snider, 2019-04-06
--
-- NOTE: this is a solution which *should* satisfy the first test set where
-- N <= 10000, but will not be efficient enough for N <= 10^100

import Control.Monad (replicateM, mapM)
import Data.List
import qualified Data.Map   as Map
import Data.Maybe (fromJust)

-- Types
data TestCase = TestCase PrimesLimit Ciphertext

type PrimesLimit = Integer
type Ciphertext  = [Integer]
type Plainprimes = [Integer]
type Plaintext   = String


-- Top level logic
main = do
    t   <- fmap read getLine :: IO Int
    tcs <- replicateM t getTestCase
    mapM (\(tc, i) -> putStrLn ("Case #" ++ show i ++ ": " ++ (solve tc)))
        $ zip tcs [1..t]


getTestCase :: IO TestCase
getTestCase = do
        (primesLimit:xs) <- readInts
        ciphertext <- readInts
        return $ TestCase primesLimit ciphertext
    where
        readInts :: IO [Integer]
        readInts = map read . words <$> getLine


-- Solution
--
-- Step 1: First find out which primes, P1..Pn+1, were used:
--
--   * Notice that each number of the ciphertext Cn, is the multiplication of
--   the previous two primes:
--
--      Cn = Pn * Pn+1
--
--   * This means, once we've factorized the first ciphertext digit C1, into
--   P1 and P2, we can easily calculate:
--
--      P3 = C2 / P2
--      P4 = C3 / P3
--      ...
--      Pn+1 = Cn / Pn
--
-- Step 2: Find mapping from primes P1..Pn+1 to plaintext O1..On+1
--
--   * Notice the crucial hint that the selected primes are sorted in increasing
--   order, which indicates that we simply need to take all of the primes we've
--   found, remove duplicates, sort them and we have our mapping P -> O
--
solve :: TestCase -> Plaintext
solve (TestCase lim ct) =
    let primes = plaintextPrimes ct
        m      = primesMap primes
    in
        map (getPlaintext m) primes
    where
        getPlaintext m c = fromJust $ Map.lookup c m


-- Get the list of plaintext primes from the ciphertext
plaintextPrimes :: Ciphertext -> Plainprimes
plaintextPrimes (c1:cs) =
    let (p1, p2) = factorize c1
    in
        p1 : scanl (\px cx -> cx `quot` px) p2 cs


-- Factorize a ciphertext number into two primes
factorize :: Integer -> (Integer, Integer)
factorize n =
    let f1 = head $ filter (\x -> (n `mod` x) == 0) listOfPrimes
        f2 = n `div` f1
    in (f1, f2)


-- A map from plaintext primes -> A..Z
-- primesMap :: Plainprimes -> Map.Map Integer Char
primesMap primes =
    let primes' = nub . sort $ primes
    in  Map.fromList $ zip primes' ['A'..'Z']


-- An infinite list of primes
listOfPrimes :: [Integer]
listOfPrimes =
    2 : 3 : sieve (tail listOfPrimes) [5, 7..]
  where
    sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
              where (h,~(_:t)) = span (< p*p) xs


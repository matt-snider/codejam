import Control.Monad (replicateM, mapM)


-- Top level logic
main = do
    t   <- fmap read getLine :: IO Int
    tcs <- replicateM t getTestCase
    mapM solve $ zip tcs [1..t]


getTestCase :: IO TestCase
getTestCase = do
        (p:k:[]) <- fmap words getLine
        return   $ TestCase (p) (read k)


solve :: (TestCase, Int) -> IO ()
solve (tc, i) = printResult i (flipPancakes tc 0)


printResult :: Int -> Maybe Int -> IO ()
printResult i flips = do
    case flips of
        Just n  -> print (show n)
        Nothing -> print ("IMPOSSIBLE")
    where print r = putStrLn ("Case #" ++ show i ++ ": " ++ r)


-- Data
-- Pancakes: String of +/- representing happy/blank pancakes
-- FlipperSize: the number of pancakes we can flip at once
data TestCase = TestCase Pancakes FlipperSize deriving Show
type Pancakes = String
type FlipperSize = Int


-- Implementation
-- Sweep through the pancake row until the left edge
-- of the flipper aligns with the first blank pancake.
-- Flip and repeat. If at any point we aren't able
-- to flip a section because there aren't enough
-- pancakes to the right, then we fail.
flip1 :: Char -> Char
flip1 '+' = '-'
flip1 '-' = '+'


flipN :: Int -> Pancakes -> Maybe Pancakes
flipN n p | length p >= n = Just $ mapN flip1 p n
          | otherwise     = Nothing
          where mapN f xs n =
                    let (toMap, rest) = splitAt n xs
                    in (map f toMap) ++ rest


flipPancakes :: TestCase -> Int -> Maybe Int
flipPancakes (TestCase p k) flips =
        case splitConsHappy p of
            (happy, "") -> Just flips
            (happy, blank) ->
                case flipN k blank of
                    Just newP -> flipPancakes (TestCase (happy ++ newP) k) (flips + 1)
                    Nothing -> Nothing
        where splitConsHappy = span(== '+')

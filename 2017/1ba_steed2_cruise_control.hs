-- The problem we need to solve for Annie is
-- the same problem we need to solve for each
-- of the other horses:
--
-- At which speed should we travel to arrive at D
-- at the same time as any horses in front of us?
-- Each horse has a max speed except for Annie, who may
-- travel at any speed (constrained by problem defn limits).
import Control.Monad (replicateM, mapM)
import Data.List (sortOn)


-- Main logic
data Horse = Horse
    { position :: Position
    , speed    :: Speed
    } deriving (Show)

type Speed = Float
type Position = Int

annie :: Horse
annie = Horse 0 (read "Infinity")


idealSpeed :: Horse -> [Horse] -> Position -> Speed
idealSpeed h0 [] _ = speed h0
idealSpeed h0 (h1:hs) d =
        let d0   = fromIntegral $ d  - position h0
            d1   = fromIntegral $ d  - position h1
            t0   = d0 / speed h0
            t1   = d1 / (idealSpeed h1 hs d)
            tMax = maximum [t0, t1]
        in  d0 / tMax


-- Input/Output
data TestCase = TestCase
    { destination :: Int
    , horses      :: [Horse]
    }


main :: IO [()]
main = do
    t   <- fmap read getLine :: IO Int
    tcs <- replicateM t getTestCase
    mapM solve $ zip tcs [1..t]


solve :: (TestCase, Int) -> IO ()
solve (tc, i) =
        let r = idealSpeed annie (horses tc) (destination tc)
        in putStrLn ("Case #" ++ show i ++ ": " ++ show r)


getTestCase :: IO TestCase
getTestCase = do
        (dest:n:[]) <- readInts
        hs <- replicateM n readHorse
        return  $ TestCase dest (sortOn position hs)
    where
        readInts :: IO [Int]
        readInts = map read . words <$> getLine

        readHorse :: IO Horse
        readHorse = do
            (p:s:[]) <- readInts
            return $ Horse p (fromIntegral s)

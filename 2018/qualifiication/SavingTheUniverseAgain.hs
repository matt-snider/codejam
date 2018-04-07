-- Google Code Jam 2018
-- Saving The Universe Again
--
-- Hack alien program to survive by swapping a minimum number of
-- instructions in their attack program.
--
-- Strategy: move Shoot commands to the beginning to reduce the
-- beam strength.
import Control.Monad (foldM, mapM)


-- Models
data TestCase = TestCase Health [Instruction]
data Instruction = Charge | Shoot deriving (Eq, Show)
data State = State BeamStrength Health deriving Show
type BeamStrength = Int
type Health = Int
type Moves = Int


-- IO
main :: IO ()
main = do
    t   <- fmap read getLine :: IO Int
    _ <- mapM run [1..t]
    return ()


run :: Int -> IO ()
run c = do
    test <- readTestCase
    putResult c (solve 0 test)


readTestCase :: IO TestCase
readTestCase = do
        (health:instrs:[]) <- fmap words getLine
        return $ TestCase (read health) (map readInstruction instrs)


readInstruction :: Char -> Instruction
readInstruction 'C' = Charge
readInstruction 'S' = Shoot


putResult :: Int  -> Maybe Int -> IO ()
putResult c (Just i) = putStrLn $ "Case #" ++ show c ++ ": " ++ show i
putResult c Nothing = putStrLn $ "Case #" ++ show c ++ ": IMPOSSIBLE"


-- Main logic
solve :: Moves -> TestCase -> Maybe Moves
solve currMoves tc@(TestCase health instrs) =
    case runAlienProgram (State 1 health) instrs of
        -- Success
        Just _ -> Just currMoves

        -- We died. If possible, hack the alien program and try again
        Nothing -> hackProgram instrs >>= solve'
            where solve' instrs = solve (currMoves + 1) (TestCase health instrs)


hackProgram :: [Instruction] -> Maybe [Instruction]
hackProgram (Charge:Shoot:xs) = Just (Shoot : Charge : xs)
hackProgram (x:xs) = (x:) <$> (hackProgram xs)
hackProgram [] = Nothing


runAlienProgram :: State -> [Instruction] -> Maybe State
runAlienProgram state instrs = foldM exec state instrs


exec :: State -> Instruction -> Maybe State
exec (State beam health) Charge = Just (State (beam*2) health)
exec (State beam health) Shoot
        | health' >= 0 = Just (State beam health')
        | otherwise = Nothing
        where health' = health - beam

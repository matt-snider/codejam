import Control.Monad (foldM, mapM, replicateM)


-- Models
data TestCase = TestCase Health [Instruction]
data Instruction = Charge | Shoot deriving Show
data State = State BeamStrength Health
type Health = Int
type BeamStrength = Int


-- IO
main :: IO ()
main = do
    t   <- fmap read getLine :: IO Int
    _ <- mapM run [1..t]
    return ()
    -- let ans = map solve tcs
    -- mapM putResult $ zip [1..t] ans

run :: Int -> IO ()
run c = do
    test <- readTestCase
    putResult c (solve test)


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
solve :: TestCase -> Maybe Int
solve (TestCase health instrs) =
    case runProgram (State 1 health) instrs of
        -- TODO: this just returns the end health, not num moves
        Just (State x y) -> Just y
        Nothing -> Nothing


runProgram :: State -> [Instruction] -> Maybe State
runProgram state instrs = foldM exec state instrs


exec :: State -> Instruction -> Maybe State
exec (State beam health) Charge = Just (State (beam*2) health)
exec (State beam health) Shoot
        | health' > 0 = Just (State beam health')
        | otherwise = Nothing
        where health' = health - beam

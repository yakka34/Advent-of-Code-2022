data Hand = Rock | Paper | Scissor deriving (Show)
data Outcome = Draw | Win | Loss deriving (Eq, Show)

mapHand :: String -> Hand
mapHand "A" = Rock
mapHand "B" = Paper
mapHand "C" = Scissor
mapHand "Y" = Paper
mapHand "X" = Rock
mapHand "Z" = Scissor

mapOutcome :: String -> Outcome
mapOutcome "Y" = Draw
mapOutcome "X" = Loss
mapOutcome "Z" = Win

instance Read Hand where
    readsPrec _ str = [(mapHand str, "")]

instance Read Outcome where
    readsPrec _ str = [(mapOutcome str, "")]

class Score a where
    score :: a -> Int

instance Score Hand where
    score Rock = 1
    score Paper = 2
    score Scissor = 3

instance Score Outcome where
    score Loss = 0
    score Draw = 3
    score Win = 6

play :: (Hand, Hand) -> Outcome
play (Rock, Rock) = Draw
play (Rock, Paper) = Win
play (Rock, Scissor) = Loss
play (Paper, Rock) = Loss
play (Paper, Paper) = Draw
play (Paper, Scissor) = Win
play (Scissor, Rock) = Win
play (Scissor, Paper) = Loss
play (Scissor, Scissor) = Draw

bruteforceOutcome :: (Hand, Outcome) -> [Hand] -> Hand
bruteforceOutcome (x, y) (z:zs)
    | play (x, z) == y = z
    | otherwise = bruteforceOutcome (x, y) zs

main = do
    file <- readFile "day_2_input.txt"
    let plays = fmap words (lines file)
    let tuples = [((read :: String -> Hand) x, (read :: String -> Outcome) y) | (x:y:_) <- plays]
    let scores = [score (bruteforceOutcome (x, y) [Rock, Paper, Scissor]) + score y | (x, y) <- tuples]
    return $ sum scores
    -- Part 1
    -- let tuples = [((read :: String -> Hand) x, (read :: String -> Hand) y) | (x:y:_) <- plays]
    -- let scores = [score (play (x, y)) + score y | (x, y) <- tuples]
    -- return $ sum scores
    
import System.IO
import Data.List

splitCaloriesList :: [String] -> [[String]]
splitCaloriesList [] = []
splitCaloriesList (x:xs) = sublist : splitCaloriesList (drop (length sublist) xs)
    where
        sublist = takeWhile (not . null) (x:xs)

main = do
    file <- readFile "day_1_input.txt"
    let calories = splitCaloriesList $ lines file
    let sums = [sum (fmap (read :: String -> Int) x) | x <- calories]
    -- Part 1
    --return $ maximum sums
    -- Part 2
    return $ sum $ take 3 $ reverse $ sort sums
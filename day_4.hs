import Data.List
import Data.Maybe

contains :: [Int] -> [Int] -> Bool
contains xs ys = (xs `intersect` ys) == ys

overlaps  :: [Int] -> [Int] -> Bool
overlaps xs ys = not . null $ xs `intersect` ys

rangesContains :: ([Int], [Int]) -> Bool
rangesContains (xs, ys) = contains xs ys || contains ys xs

rangesOverlap :: ([Int], [Int]) -> Bool
rangesOverlap (xs, ys) = overlaps xs ys

parseRow :: String -> (String, String)
parseRow xs = (take seperatorIndex xs, drop (seperatorIndex + 1) xs)
    where
        seperatorIndex = fromJust $ elemIndex ',' xs

mapRange :: String -> [Int]
mapRange xs = [read (take seperatorIndex xs)..read (drop (seperatorIndex + 1) xs)]
    where
        seperatorIndex = fromJust $ elemIndex '-' xs

mapRanges:: (String, String) -> ([Int], [Int])
mapRanges (xs, ys) = (mapRange xs, mapRange ys)

main = do
    file <- readFile "day_4_input.txt"
    let rows = lines file
    let ranges = fmap (mapRanges . parseRow) rows
    -- Part 2
    return $ length $ filter rangesOverlap ranges
    -- Part 1
    --return $ length $ filter rangesContains ranges
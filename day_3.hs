import System.IO
import Data.Maybe
import Data.List
import qualified Data.Map as Map

priorities :: [(Char, Int)]
priorities = zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27.. 52]

splitInHalf :: String -> (String, String)
splitInHalf xs = splitAt (div (length xs) 2) xs

findCommonChar :: (String, String) -> Char
findCommonChar (xs, ys) = head $ xs `intersect` ys

getPriority :: Char -> Int
getPriority x = fromJust (lookup x priorities)

getGroups :: [String] -> [Char]
getGroups [] = []
getGroups (x:y:z:xs) = head (intersect x y `intersect` z) : getGroups xs

main = do
    file <- readFile "day_3_input.txt"
    let rows = lines file
    -- Part 2
    let groups = getGroups rows
    return $ sum (fmap getPriority groups)
    -- Part 1
    -- let commons = fmap (findCommonChar . splitInHalf) rows
    -- return $ sum (fmap getPriority commons)
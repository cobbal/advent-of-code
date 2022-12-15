import System.IO
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe

halve :: [a] -> ([a], [a])
halve l = (take n l, drop n l)
  where n = length l `div` 2

common :: Eq a => [a] -> [a] -> [a]
common xs ys = nub xs `intersect` nub ys

score :: Char -> Int
score c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

main :: IO ()
main = do
  sacks <- (halve <$>) . lines <$> readFile "input-real.txt"
  let commons = head . uncurry common <$> sacks
  putStr "part-1: "
  print $ sum (score <$> commons)
  teams <- chunksOf 3 . lines <$> readFile "input-real.txt"
  let badges = head . foldr1 common <$> teams
  putStr "part-2: "
  print $ sum (score <$> badges)

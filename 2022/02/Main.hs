import System.IO
import Control.Monad
import Data.List
import Data.Char

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split x (y : ys)
  | x == y = [] : res
  | otherwise = (y : head res) : tail res
    where res = split x ys

winScore :: Char -> Char -> Int
winScore them me = 3 * (relative `mod` 3)
  where relative = (ord me - ord 'X') - (ord them - ord 'A') + 1

playScore :: Char -> Int
playScore c = ord c - ord 'X' + 1

score :: (Char, Char) -> Int
score (them, me) = playScore me + winScore them me

reinterpret :: (Char, Char) -> (Char, Char)
reinterpret (them, me) = (them, chr (ord 'X' + (ord them - ord 'A' + delta) `mod` 3))
  where delta = ord me - ord 'Y'

main :: IO ()
main = do
  input <- readFile "input-real.txt"
  let plays = (\l -> (l !! 0, l !! 2)) <$> lines input
  let scores = map score plays
  putStr "part-1: "
  print $ sum scores
  let plays2 = map reinterpret plays
  let scores2 = map score plays2
  putStr "part-2: "
  print $ sum scores2

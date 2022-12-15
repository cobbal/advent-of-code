{-# LANGUAGE TemplateHaskell, LambdaCase, ScopedTypeVariables, GADTs #-}
import System.IO
import Control.Monad
import Data.List

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split x (y : ys)
  | x == y = [] : res
  | otherwise = (y : head res) : tail res
    where res = split x ys

main :: IO ()
main = do
  input <- readFile "input-real.txt"
  let elves :: [[Int]] = map read <$> (split "" (lines input))
  putStr "part-1: "
  print $ maximum (sum <$> elves)
  putStr "part-2: "
  print $ sum $ take 3 $ reverse $ sort (sum <$> elves)

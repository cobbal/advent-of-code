import System.IO
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Control.Lens
import Data.List.Lens

startIndex :: Int -> String -> Int
startIndex _ [] = undefined
startIndex n s
  | (length $ nub $ take n s) == n = n
  | otherwise = 1 + startIndex n (tail s)

main :: IO ()
main = do
  stream <- readFile "input-real.txt"
  putStr "part-1: "
  print $ startIndex 4 stream
  putStr "part-2: "
  print $ startIndex 14 stream

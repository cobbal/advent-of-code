import System.IO
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe

data Range = Range Int Int
  deriving (Eq, Ord, Show)

type Pair = (Range, Range)

parseLine :: String -> Pair
parseLine line = (parseRange range0, parseRange range1)
  where
    (range0, range1) = split ',' line

    parseRange :: String -> Range
    parseRange str = Range (read start) (read end)
      where
        (start, end) = split '-' str

    split :: Char -> String -> (String, String)
    split c s = (take n s, drop (n + 1) s)
      where
        n = fromJust (elemIndex c s)

contains :: Range -> Range -> Bool
contains (Range l0 h0) (Range l1 h1) = l0 <= l1 && h1 <= h0

overlap :: Range -> Range -> Bool
overlap (Range l0 h0) (Range l1 h1) = not (h0 < l1 || h1 < l0)

redundantPair :: Pair -> Bool
redundantPair (r0, r1) = r0 `contains` r1 || r1 `contains` r0

main :: IO ()
main = do
  pairs <- (parseLine <$>) . lines <$> readFile "input-real.txt"
  putStr "part-1: "
  print $ length $ filter redundantPair pairs
  putStr "part-2: "
  print $ length $ filter (uncurry overlap) pairs

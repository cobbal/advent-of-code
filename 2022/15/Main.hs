{-# LANGUAGE TemplateHaskell, LambdaCase, ScopedTypeVariables, GADTs #-}

import System.IO
import Control.Monad
import Control.Applicative
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Control.Lens
import Data.List.Lens
import Control.Monad.State
import Data.Foldable
import Data.Function
import Debug.Trace

type RangeSet = [(Int, Int)]

unions :: [RangeSet] -> RangeSet
unions = merge . L.sort . concat
  where
    merge [] = []
    merge [r] = [r]
    merge ((lo, hi) : (lo', hi') : rest)
      | lo' <= hi + 1 = merge ((lo, max hi hi') : rest)
      | otherwise = (lo, hi) : merge ((lo', hi') : rest)

singleton :: Int -> RangeSet
singleton i = range i i

range :: Int -> Int -> RangeSet
range lo hi = [(lo, hi)]

validRange :: (Int, Int) -> Bool
validRange = uncurry (<=)

(\\) :: RangeSet -> RangeSet -> RangeSet
(\\) = foldl' rangeMinus
  where
    rangeMinus :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    rangeMinus [] _ = []
    rangeMinus (x@(lo, hi) : xs) y@(lo', hi')
      | hi < lo' || hi' < lo = x : rangeMinus xs y
      | otherwise = filter validRange [(lo, lo' - 1), (hi' + 1, hi)] ++ rangeMinus xs y

size :: RangeSet -> Int
size = sum . map (\(lo, hi) -> hi - lo + 1)

type Pt = (Int, Int)

isNum :: Char -> Bool
isNum x = isDigit x || x == '-'

parse :: String -> (Pt, Pt)
parse s = ((read x, read y), (read x', read y'))
  where [_, x, _, y, _, x', _, y'] = L.groupBy ((==) `on` isNum) s

dist :: Pt -> Pt -> Int
dist (x, y) (x', y') = abs (x - x') + abs (y - y')

isnt :: [(Pt, Pt)] -> Int -> RangeSet
isnt l row = unions $ map isnt1 l
  where
    isnt1 (p@(x, y), q) = filter validRange $ range (x - xdist) (x + xdist)
      where xdist = dist p q - abs (y - row)

is :: [(Pt, Pt)] -> Int -> RangeSet
is l i = unions $ map (singleton . fst) $ filter ((== i ) . snd) (map snd l)

main :: IO ()
main = do
  let (file, row, count) = if False
                           then ("ex", 10, 20)
                           else ("real", 2000000, 4000000)
  sensors <- map parse . lines <$> readFile ("input-" ++ file ++ ".txt")
  putStrLn "part-1:"
  print $ size $ isnt sensors row \\ is sensors row
  putStrLn "part-2:"
  [(x, y)] <- pure $ do
    y <- [0..count]
    (,y) . fst <$> range 0 count \\ isnt sensors y
  print $ 4000000 * x + y

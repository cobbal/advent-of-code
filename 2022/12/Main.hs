{-# LANGUAGE TemplateHaskell, LambdaCase #-}

import System.IO
import Control.Monad
import Control.Applicative
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Lens
import Data.List.Lens
import Control.Monad.State
import Data.Foldable
import Debug.Trace
import System.Environment

data Grid = Grid { width :: Int
                 , height :: Int
                 , elevation :: Int -> Int -> Int
                 }

makeGrid :: [String] -> Grid
makeGrid grid = Grid w h index
  where
    w = length $ head grid
    h = length grid
    index :: Int -> Int -> Int
    index x y
      | x < 0 || x >= w = 9001
      | y < 0 || y >= h = 9001
      | otherwise = case grid !! y !! x of
          'S' -> ord 'a' - ord 'a'
          'E' -> ord 'z' - ord 'a'
          c -> ord c - ord 'a'

minimum' :: Ord a => [a] -> Maybe a
minimum' [] = Nothing
minimum' xs = Just (minimum xs)

newtype DistMap = DistMap { unDistMap :: [[Maybe Int]] }
                deriving (Eq)

instance Show DistMap where
  show (DistMap l) = L.intercalate "\n" (map (dispChar =<<) l)
    where
      dispChar Nothing = "  ."
      dispChar (Just x) = reverse $ take 3 $ reverse $ "    " ++ show x

refine :: Grid -> DistMap -> DistMap
refine grid (DistMap map) = DistMap $ imap (\y -> imap (\x _ -> best x y)) map
  where
    best :: Int -> Int -> Maybe Int
    best x y = minimum' possibilities
      where
        possibilities = catMaybes $ (\(x, y, diff) -> (diff +) <$> map !! y !! x) <$> filter neighborReachable neighborsCoords
        self = elevation grid x y
        neighborsCoords = [ (x, y, 0)
                          , (x - 1, y, 1)
                          , (x + 1, y, 1)
                          , (x, y - 1, 1)
                          , (x, y + 1, 1)
                          ]
        neighborReachable (x, y, _) = elevation grid x y <= self + 1

findCoords :: Char -> [String] -> [(Int, Int)]
findCoords c = concat . concat . imap (\y -> imap (\x c' -> if c == c' then [(x, y)] else []))

finiteFix :: (Show a, Eq a) => a -> (a -> a) -> a
finiteFix x f
  | x == x' = x
  | otherwise = finiteFix x' f
  where x' = f x

main :: IO ()
main = do
  target <- fromMaybe "." . listToMaybe <$> getArgs
  rawGrid <- lines <$> readFile (target ++ "/input-real.txt")
  let [start] = findCoords 'S' rawGrid
  let [end] = findCoords 'E' rawGrid
  let bottom = DistMap $ map (map (const Nothing)) rawGrid & ix (snd end) . ix (fst end) .~ Just 0
  let solution = finiteFix bottom $ refine (makeGrid rawGrid)
  let Just distance = unDistMap solution !! snd start !! fst start
  putStrLn "part-1:"
  print distance
  putStrLn "part-2:"
  let starts = catMaybes $ map snd $ filter ((== 'a') . fst) $ zip (concat rawGrid) (concat (unDistMap solution))
  print (minimum starts)

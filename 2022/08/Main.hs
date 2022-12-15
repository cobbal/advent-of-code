{-# LANGUAGE TemplateHaskell, LambdaCase #-}

import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import Control.Lens
import Data.List.Lens
import Control.Monad.State

vizScan :: [[Int]] -> [[Bool]]
vizScan = map (helper (-1))
  where
    helper :: Int -> [Int] -> [Bool]
    helper _ [] = []
    helper prev (row : rows) = (prev < row) : helper (max prev row) rows

rot :: Int -> [[a]] -> [[a]]
rot 0 = id
rot n = L.transpose . map reverse . rot (n - 1)

printGrid :: (a -> String) -> [[a]] -> IO ()
printGrid fn grid = mapM_ (putStrLn . concat . map fn) grid >> putStrLn ""

printBoolGrid :: [[Bool]] -> IO ()
printBoolGrid = printGrid (\x -> if x then "*" else " ")

printIntGrid :: [[Int]] -> IO ()
printIntGrid = printGrid show

inRot :: ([[Int]] -> [[a]]) -> [[Int]] -> Int -> [[a]]
inRot fn grid n = rot (4 - n) $ fn (rot n grid)

viewScore :: Int -> [Int] -> Int
viewScore _ [] = 0
viewScore height (tree : trees) = 1 + if tree < height then viewScore height trees else 0

viewScan :: [[Int]] -> [[Int]]
viewScan = map rowScan
  where
    rowScan :: [Int] -> [Int]
    rowScan row = zipWith viewScore row (tail $ L.tails row)

main :: IO ()
main = do
  grid <- (map (\c -> ord c - ord '0') <$>) . lines <$> readFile "input-real.txt"
  -- printIntGrid grid
  let views = map (inRot vizScan grid) [0..3]
  -- mapM_ printBoolGrid views
  putStrLn "part-1: "
  print $ length $ filter id $ map or $ L.transpose $ map concat views
  putStrLn "part-2: "
  let views' = map (inRot viewScan grid) [0..3]
  print $ head $ L.sortOn negate $ map product $ L.transpose $ map concat views'

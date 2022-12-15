{-# LANGUAGE TemplateHaskell, LambdaCase #-}

import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Lens
import Data.List.Lens
import Control.Monad.Writer
import Data.Foldable

data Dir = R | U | L | D deriving (Eq, Read, Show)

parse :: String -> [Dir]
parse s = case words s of
  [dir, times] -> replicate (read times) (read dir)

data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord, Show)
data Rope = Rope { ropeHead :: Point, ropeTail :: Point } deriving (Eq, Ord, Show)

type Ropoid = [Point]

goDir :: Dir -> Point -> Point
goDir R (Point x y) = Point (x + 1) y
goDir U (Point x y) = Point x (y + 1)
goDir L (Point x y) = Point (x - 1) y
goDir D (Point x y) = Point x (y - 1)

newTail :: Point -> Point -> Point
newTail h t
  | abs (x h - x t) <= 1 && abs (y h - y t) <= 1 = t
  | otherwise = Point (x t + signum (x h - x t)) (y t + signum (y h - y t))

move :: Dir -> Rope -> Rope
move dir (Rope h t) = Rope h' (newTail h' t)
  where h' = goDir dir h

writeMove :: Rope -> Dir -> Writer (S.Set Point) Rope
writeMove rope dir = do
  let rope' = move dir rope
  tell $ S.singleton (ropeTail rope')
  pure rope'

movoid :: Dir -> Ropoid -> Ropoid
movoid _ [] = []
movoid dir (h : t) = moveToward (goDir dir h) t
  where
    moveToward :: Point -> Ropoid -> Ropoid
    moveToward p [] = [p]
    moveToward p (k : t) = p : moveToward (newTail p k) t

writeMovoid :: Ropoid -> Dir -> Writer (S.Set Point) Ropoid
writeMovoid ropoid dir = do
  let ropoid' = movoid dir ropoid
  tell $ S.singleton (last ropoid')
  pure ropoid'

main :: IO ()
main = do
  moves <- (parse <$>) . lines <$> readFile "input-real.txt"
  let tails = execWriter $ foldlM writeMove (Rope (Point 0 0) (Point 0 0)) (concat moves)
  putStr "part-1: "
  print (length tails)
  putStr "part-2: "
  let startRopoid = replicate 10 $ Point 0 0
  let tails' = execWriter $ foldlM writeMovoid startRopoid (concat moves)
  print (length tails')

{-# LANGUAGE TemplateHaskell, LambdaCase #-}

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
import Control.Monad.State.Class
import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef
import Data.Foldable
import Debug.Trace
import System.Environment

-- Locality is for suckers
type Cave = S.Set (Int, Int)

breakGroups :: (a -> Bool) -> [a] -> [[a]]
breakGroups _ [] = []
breakGroups test xs = group : breakGroups test (drop 1 rest)
  where (group, rest) = break test xs

pointsBetween :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pointsBetween (x, y) (x', y')
  | x == x' && y == y' = [(x, y)]
  | otherwise = (x, y) : pointsBetween (x + signum (x' - x), y + signum (y' - y)) (x', y')

newtype CaveState s a = CaveState { unCaveState :: ReaderT (STRef s Cave) (ST s) a }
  deriving (Functor, Applicative, Monad)

instance MonadState Cave (CaveState s) where
  get = CaveState $ lift . readSTRef =<< ask
  put c = CaveState $ lift . flip writeSTRef c =<< ask

execState :: (forall s. CaveState s ()) -> Cave -> Cave
execState s c = snd $ runState s c

runState :: (forall s. CaveState s a) -> Cave -> (a, Cave)
runState s c = runST $ do
  ref <- newSTRef c
  result <- runReaderT (unCaveState s) ref
  (result,) <$> readSTRef ref

drawStructure :: [(Int, Int)] -> CaveState s ()
drawStructure l = helper (head l) l
  where
    helper :: (Int, Int) -> [(Int, Int)] -> CaveState s ()
    helper _ [] = pure ()
    helper start (end : rest) = do
      mapM_ (modify' . S.insert) $ pointsBetween start end
      helper end rest

platoify :: [[(Int, Int)]] -> Cave
platoify l = execState (mapM_ drawStructure l) S.empty

caveRange :: Cave -> ([Int], [Int])
caveRange c = ([minimum xs..maximum xs], [minimum ys..maximum ys])
  where
    xs = map fst $ S.toList c
    ys = map snd $ S.toList c

renderCave :: Cave -> [String]
renderCave c = (flip map ys $ \y -> flip map xs $ \x -> if (x, y) `S.member` c then '#' else '.') ++ [""]
  where (xs, ys) = caveRange c

placeSand :: Int -> CaveState s Bool
placeSand floor = do
  initBlocked <- S.member (500, 0) <$> get
  tried <- helper floor (500, 0)
  pure $ not initBlocked && tried
  where
    helper :: Int -> (Int, Int) -> CaveState s Bool
    helper floor (x, y)
      | y > floor = pure False
      | otherwise = do
          let choices = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
          cave <- get
          case L.find (flip S.notMember cave) choices of
            Nothing -> modify' (S.insert (x, y)) >> pure True
            Just xy' -> helper floor xy'

placeAll :: CaveState s Int
placeAll = helper =<< last . snd . caveRange <$> get
  where
    helper floor = do
      placed <- placeSand floor
      if placed then succ <$> helper floor else pure 0

main :: IO ()
main = do
  raw <- map (filter (/= "->") . words) . lines <$> readFile "input-real.txt"

  let pairRead [x, y] = (read x, read y)
  let structures :: [[(Int, Int)]] =  map (map (pairRead . breakGroups (== ','))) raw
  let cave = platoify structures
  let (count, filledCave) = runState placeAll cave

  -- mapM_ putStrLn $ renderCave cave
  -- mapM_ putStrLn $ renderCave filledCave

  putStrLn "part-1:"
  print $ count
  putStrLn "part-2:"

  let (xRange, yRange) = caveRange cave
  let floor = maximum yRange + 2
  let xMin = minimum xRange - floor
  let xMax = maximum xRange + floor
  let floorCave = execState (drawStructure [(xMin, floor), (xMax, floor)]) cave
  let (floorCount, filledFloorCave) = runState placeAll floorCave

  -- mapM_ putStrLn $ renderCave floorCave
  -- mapM_ putStrLn $ renderCave filledFloorCave

  print floorCount

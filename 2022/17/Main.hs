{-# LANGUAGE TemplateHaskell, LambdaCase, ScopedTypeVariables, GADTs #-}

import System.IO
import Control.Monad
import Control.Applicative
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Set as S
import Control.Lens hiding (children)
import Data.List.Lens
import Control.Monad.State
import Data.Foldable
import Data.Function
import Debug.Trace
import Control.Parallel.Strategies

type Rock = [(Int, Int)]
type Grid = S.Set (Int, Int)

rocks = parseRock . words <$> ["####", ".#. ### .#.", "..# ..# ###", "# # # #", "## ##"]

parseRock :: [String] -> Rock
parseRock l = catMaybes $ concat $ imap (\y -> imap (\x c -> if c == '#' then Just (x, height - y) else Nothing)) l
  where height = length l

data Cave = Cave { _rockIndex :: Int
                 , _rockStop :: Int
                 , _currentRock :: Rock
                 , _stopped :: Grid
                 , _windIndex :: Int
                 , _windMod :: Int
                 , _windStream :: [Bool]
                 , _caveLog :: [String]
                 , _loopTracker :: M.Map Fingerprint (Int, Int)
                 }

data Fingerprint = Fingerprint { _fCap :: [Int]
                               , _fRockIndex :: Int
                               , _fWindIndex :: Int
                               } deriving (Eq, Ord, Show)

makeLenses ''Cave

fingerprint :: CaveState Fingerprint
fingerprint = do
  grid <- use stopped
  let height = gridHeight grid
  let cap = flip map [0..(width - 1)] $ \x -> height - (maximum $ 0 : (map snd $ S.toList $ S.filter ((== x) . fst) grid))
  i <- use rockIndex
  Fingerprint cap (i `mod` 5) <$> use windIndex

type CaveState a = State Cave a

move :: Bool -> (Int, Int) -> (Int, Int)
move True (x, y) = (x - 1, y)
move False (x, y) = (x + 1, y)

fall1 :: (Int, Int) -> (Int, Int)
fall1 (x, y) = (x, y - 1)

width :: Int
width = 7

collides :: Grid -> Rock -> Bool
collides g = any (\(x, y) -> x < 0 || width <= x || y < 0 || (x, y) `S.member` g)

gust :: CaveState ()
gust = do
  left <- head <$> (windStream <<%= tail)
  m <- use windMod
  windIndex %= (`mod` m) . (+ 1)
  rock <- use currentRock
  grid <- use stopped
  let proposedRock = move left <$> rock
  if collides grid proposedRock
  then pure ()
  else currentRock .= proposedRock

fall :: CaveState Bool
fall = do
  rock <- use currentRock
  grid <- use stopped
  let proposedRock = fall1 <$> rock
  if collides grid proposedRock
    then do
    stopped .= grid `S.union` S.fromList rock
    pure True
    else do
    currentRock .= proposedRock
    pure False

gridHeight :: Grid -> Int
gridHeight g = 1 + maximum (-1 : map snd (S.toList g))

startRock :: CaveState Bool
startRock = do
  nextIndex <- rockIndex <<+= 1
  stop <- use rockStop
  if nextIndex >= stop
    then pure False
    else do
    let next = rocks L.!! (nextIndex `mod` 5)
    height <- gridHeight <$> use stopped
    currentRock .= map (\(x, y) -> (x + 2, y + height + 2)) next
    pure True

startCave :: Int -> [Bool] -> Cave
startCave rockCount windPattern = execState startRock protoCave
  where protoCave = Cave 0 rockCount [] S.empty 0 (length windPattern) (cycle windPattern) [] M.empty

traceShowId' :: (Show a, Show b) => b -> a -> a
traceShowId' x y = traceShow (x, y) y

runFast :: CaveState ()
runFast = do
  gust
  didStop <- fall
  if not didStop then runFast else do
    prints <- use loopTracker
    fp <- fingerprint
    height <- gridHeight <$> use stopped
    rockIdx <- use rockIndex
    stop <- use rockStop
    case prints M.!? fp of
      Just (oldHeight, oldRockIdx) -> do
        let deltaY = traceShowId' "dy" $ height - oldHeight
        let deltaR = traceShowId' "dr" $ rockIdx - oldRockIdx
        let bigSteps = max 0 $ traceShowId' "steps" $ (stop - rockIdx) `div` deltaR
        rockIndex += deltaR * bigSteps
        stopped %= S.map (\(x, y) -> (x, y + bigSteps * deltaY))
        startRock
        runSlow
      Nothing -> do
        loopTracker %= M.insert fp (height, rockIdx)
        startRock
        runFast

runSlow :: CaveState ()
runSlow = do
  gust
  didStop <- fall
  if didStop
    then do
    didStart <- startRock
    if didStart then runSlow else pure ()
    else runSlow

logCave :: CaveState ()
logCave = do
  fp <- fingerprint
  grid <- use stopped
  rock <- use currentRock
  let height = maximum $ gridHeight grid : map snd rock
  let lines = flip map (reverse [0..height]) $ \y -> (\s -> "|" ++ s ++ "|") $ flip map [0..(width - 1)] $ \x ->
        if (x, y) `S.member` grid
        then '#'
        else if (x, y) `elem` rock
             then '@'
             else '.'
  caveLog %= (++ lines ++ ["+" ++ replicate 7 '-' ++ "+", show fp, ""])

main :: IO ()
main = do
  wind <- map (== '<') . head . lines <$> readFile "input-real.txt"
  putStrLn "part-1:"
  let result = execState runSlow (startCave 2022 wind)
  print $ gridHeight (result ^. stopped)
  putStrLn "part-2:"
  let result = execState runFast (startCave 1000000000000 wind)
  print $ gridHeight (result ^. stopped)

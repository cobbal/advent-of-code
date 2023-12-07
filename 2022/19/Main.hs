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
import Control.Monad.State.Strict
import Data.Foldable
import Data.Function
import Debug.Trace
import Control.Parallel.Strategies
import Text.Read (readMaybe)

newtype Ore = Ore { unOre :: Int } deriving (Eq, Ord, Num, Show)
newtype Clay = Clay { unClay :: Int } deriving (Eq, Ord, Num, Show)
newtype Obsidian = Obsidian { unObsidian :: Int } deriving (Eq, Ord, Num, Show)
newtype Geode = Geode { unGeode :: Int } deriving (Eq, Ord, Num, Show)

data Blueprint = Blueprint { _oreCost :: Ore
                           , _clayCost :: Ore
                           , _obsidianCost :: (Ore, Clay)
                           , _geodeCost :: (Ore, Obsidian)
                           } deriving (Show)

parse :: String -> Blueprint
parse s = Blueprint (Ore a) (Ore b) (Ore c, Clay d) (Ore e, Obsidian f)
  where [_, a, b, c, d, e, f] = catMaybes $ readMaybe @Int <$> words (filter (/= ':') s)

data Stockpile = Stockpile { _oreHarvest :: Ore
                           , _clayHarvest :: Clay
                           , _obsidianHarvest :: Obsidian
                           , _geodeHarvest :: Geode
                           , _ore :: Ore
                           , _clay :: Clay
                           , _obsidian :: Obsidian
                           , _geode :: Geode
                           } deriving (Eq)

instance Show Stockpile where
  show s = L.intercalate "\n" $ [ "== Minute ? =="
                                , show (unOre $ _oreHarvest s) ++ " ore -> " ++ show (unOre $ _ore s)
                                , show (unClay $ _clayHarvest s) ++ " clay -> " ++ show (unClay $ _clay s)
                                , show (unObsidian $ _obsidianHarvest s) ++ " obsidian -> " ++ show (unObsidian $ _obsidian s)
                                , show (unGeode $ _geodeHarvest s) ++ " geode -> " ++ show (unGeode $ _geode s)
                                , ""
                                ]

makeLenses ''Stockpile
makeLenses ''Blueprint

type RoboState = StateT Stockpile []

strictlyWorse :: Stockpile -> Stockpile -> Bool
strictlyWorse s t = (s /= t) && all (\m -> m s <= m t) metrics
  where metrics = [ unOre . _oreHarvest
                  , unClay . _clayHarvest
                  , unObsidian . _obsidianHarvest
                  , unGeode . _geodeHarvest
                  , unOre . _ore
                  , unClay . _clay
                  , unObsidian . _obsidian
                  , unGeode . _geode
                  ]

pruneBad :: [Stockpile] -> [Stockpile]
pruneBad ss = filter (\x -> not $ any (x `strictlyWorse`) ss) ss

harvest :: RoboState ()
harvest = do
  (ore +=) =<< use oreHarvest
  (clay +=) =<< use clayHarvest
  (obsidian +=) =<< use obsidianHarvest
  (geode +=) =<< use geodeHarvest


checkedDecrement :: (Alternative m, MonadState s m, Num a, Ord a) => Lens' s a -> a -> m ()
checkedDecrement l x = do
  guard . (x <=) =<< use l
  l -= x

step :: Blueprint -> Int -> RoboState ()
step bp tick = do
  join $ lift [buildOre, buildClay, buildObsidian, buildGeode, doNothing]
  where
    buildOre :: RoboState () = do
      -- guard =<< (`elem` []) <$> use tick
      checkedDecrement ore $ _oreCost bp
      harvest
      oreHarvest += Ore 1
    buildClay :: RoboState () = do
      -- guard =<< (`elem` [3, 5, 7, 12]) <$> use tick
      checkedDecrement ore $ _clayCost bp
      harvest
      clayHarvest += Clay 1
    buildObsidian :: RoboState () = do
      -- guard =<< (`elem` [11, 15]) <$> use tick
      let (oreCost, clayCost) = _obsidianCost bp
      checkedDecrement ore oreCost
      checkedDecrement clay clayCost
      harvest
      obsidianHarvest += Obsidian 1
    buildGeode :: RoboState () = do
      -- guard =<< (`elem` [18, 21]) <$> use tick
      let (oreCost, obsidianCost) = _geodeCost bp
      checkedDecrement ore oreCost
      checkedDecrement obsidian obsidianCost
      harvest
      geodeHarvest += Geode 1
    doNothing :: RoboState () = do
      -- guard =<< (`elem` [1, 2, 4, 6, 8, 9, 10, 13, 14, 16, 17, 19, 20, 22, 23, 24]) <$> use tick
      harvest

start ::Stockpile
start = Stockpile 1 0 0 0 0 0 0 0

execListState :: RoboState () -> [Stockpile] -> [Stockpile]
execListState s l = execStateT ((put =<< lift l) >> s) undefined

steps :: Blueprint -> Int -> [Stockpile]
steps bp = help start
  where
    help :: Stockpile -> Int -> [Stockpile]
    help s 0 = [s]
    help s n = execListState (step bp n) $ pruneBad $ help s (n - 1)

main :: IO ()
main = do
  blueprints <- map parse . lines <$> readFile "input-ex.txt"
  let results = steps (head blueprints) 24
  -- mapM_ print results
  print $ S.fromList $ map (unGeode . _geode) $ results
  putStrLn "part-1:"
  putStrLn "part-2:"

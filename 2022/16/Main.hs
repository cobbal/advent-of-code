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

isNum :: Char -> Bool
isNum x = isDigit x || x == '-'

type Graph = M.Map String (Int, [String])

parse :: [String] -> (String, (Int, [String]))
parse s = (s !! 1, (read $ filter isDigit $ s !! 4, map (filter (/= ',')) $ drop 9 s))

data Trie a = Trie { isLeaf :: Bool
                   , children :: [(a, Trie a)]
                   } deriving (Functor, Show)

shortestPath :: forall a. Show a => Trie a -> [a]
shortestPath = reverse . helper . pure . ([],)
  where
    helper :: [([a], Trie a)] -> [a]
    helper l = flip fromMaybe (fst <$> find (isLeaf . snd) l) $ helper $ do
      (path, t) <- l
      (c, t') <- children t
      pure (c : path, t')

minRoute :: Graph -> String -> String -> [String]
minRoute g = \x y -> shortestPath $ help x y
  where
    help :: String -> String -> Trie String
    help start end = Trie (start == end) $ (\next -> (next, help next end)) <$> neighbors
     where
        (_, neighbors) = g M.! start

type DistMap = M.Map (String, String) Int

distanceMap :: Graph -> DistMap
distanceMap g = M.fromList $ do
  start <- M.keys g
  end <- M.keys g
  [((start, end), length $ minRoute g start end)]

solver :: (String -> Int) -> (String -> String -> Int) -> String -> Int -> Int -> S.Set String -> Int
solver weight distance = helper
  where
    helper here time pressure remain
      | time < 0 = pressure
      | otherwise =
        let pressure' = pressure + (time - 1) * weight here
        in maximum $ S.insert pressure' $ flip S.map remain $ \there ->
          let dist = distance here there
              time' = time - 1 - dist
          in helper there time' pressure' (S.delete there remain)

-- from https://hackage.haskell.org/package/combinatorial-0.1.0.1/docs/src/Combinatorics.html#partitions
partitions :: [a] -> [([a],[a])]
partitions = foldr (\x -> concatMap (\(lxs,rxs) -> [(x:lxs,rxs), (lxs,x:rxs)])) [([],[])]

solver' :: (String -> Int) -> (String -> String -> Int) -> S.Set String -> Int
solver' weight distance nodes = maximum $ withStrategy (parListChunk 8 rseq) $ help <$> partitions (S.toList nodes)
  where
    help :: ([String], [String]) -> Int
    help (nodes, eleNodes) =
      solver weight distance "AA" 27 0 (S.fromList nodes) +
      solver weight distance "AA" 27 0 (S.fromList eleNodes)

main :: IO ()
main = do
  g <- M.fromList . map (parse . words) . lines <$> readFile ("input-real.txt")
  let g' = M.filterWithKey (\k v -> k == "AA" || fst v > 0) g
  let relevantNodes = M.keysSet g'
  let dists = M.filterWithKey (\(x, y) _ -> x `S.member` relevantNodes && y `S.member` relevantNodes) $ distanceMap g
  let weight = fst . (g M.!)
  let distance = curry (dists M.!)
  putStrLn "part-1:"
  print $ solver weight distance "AA" 31 0 $ S.delete "AA" relevantNodes
  putStrLn "part-2:"
  print $ solver' weight distance $ S.delete "AA" relevantNodes

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

data Packet = Int Int
            | Lst [Packet]
            deriving (Eq, Show)

eatElse :: Char -> State String a -> State String a -> State String a
eatElse c yes no = get >>= \case
  c' : s -> if c == c' then put s >> yes else no
  [] -> no

traceShowId' :: (Show a, Show b) => b -> a -> a
traceShowId' x y = traceShow (x, y) y

stateTrace :: (Show a, Show b) => a -> State b ()
stateTrace x = put =<< traceShowId' x <$> get

parseItem :: State String Packet
parseItem = eatElse '[' listCase intCase
  where
    listCase = do
      Lst <$> parseList
    intCase = do
      (i, s) <- break (not . isDigit) <$> get
      if null i
        then error ("expected digits, got " ++ show s)
        else put s >> pure (Int (read i))

parseList :: State String [Packet]
parseList = eatElse ']' (pure []) $ do
  h <- parseItem
  eatElse ',' (pure ()) (pure ())
  t <- parseList
  pure (h : t)

parse :: String -> Packet
parse s = result
  where
    (result, "") = runState parseItem s

breakGroups :: (a -> Bool) -> [a] -> [[a]]
breakGroups _ [] = []
breakGroups test xs = group : breakGroups test (drop 1 rest)
  where (group, rest) = break test xs

group2 :: [a] -> [(a, a)]
group2 [] = []
group2 (x : y : l) = (x, y) : group2 l

instance Ord Packet where
  compare (Int l) (Int r) = compare l r
  compare (Lst l) (Int r) = compare (Lst l) (Lst [Int r])
  compare (Int l) (Lst r) = compare (Lst [Int l]) (Lst r)
  compare (Lst []) (Lst []) = EQ
  compare (Lst []) (Lst (_ : _)) = LT
  compare (Lst (_ : _)) (Lst []) = GT
  compare (Lst (l : ls))( Lst (r : rs)) = compare (l, Lst ls) (r, Lst rs)

main :: IO ()
main = do
  target <- fromMaybe "." . listToMaybe <$> getArgs
  packets <- map parse . filter (not . null) . lines <$> readFile (target ++ "/input-real.txt")
  let pairs = group2 packets
  putStrLn "part-1:"
  print $ sum $ succ <$> L.findIndices (uncurry (<)) pairs
  putStrLn "part-2:"
  let dividers = map parse ["[[2]]", "[[6]]"]
  let sorted = L.sort $ packets ++ dividers
  print $ product $ succ <$> L.findIndices (`elem` dividers) sorted

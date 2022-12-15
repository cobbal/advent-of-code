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
import Debug.Trace

data Instr = Noop
           | Add Int
           | AddK Int
           deriving (Eq, Ord, Show)

data Machine = Machine { cycleNum :: Int
                       , register :: Int
                       , program :: [Instr]
                       } deriving (Eq, Ord, Show)

parse :: [String] -> Instr
parse ["noop"] = Noop
parse ["addx", i] = Add (read i)

step :: Machine -> Machine
step (Machine c r []) = Machine (c + 1) r []
step (Machine c r (Noop : program)) = Machine (c + 1) r program
step (Machine c r (Add i : program)) = Machine (c + 1) r (AddK i : program)
step (Machine c r (AddK i : program)) = Machine (c + 1) (r + i) program

traceAll :: Machine -> [Machine]
traceAll m = m : traceAll (step m)

strength :: Machine -> Int
strength m = cycleNum m * register m

interesting :: Machine -> Bool
interesting m = cycleNum m `mod` 40 == 20

lit :: Machine -> Bool
lit m = abs ((cycleNum m - 1) `mod` 40 - register m) <= 1

disp :: Bool -> Char
disp True = '#'
disp False = '.'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

snip :: Machine -> Machine
snip m = m { program = take 1 (program m) }

main :: IO ()
main = do
  instrs <- (parse . words <$>) . lines <$> readFile "input-real.txt"
  let machine = Machine 1 1 instrs
  let trace = takeWhile (not . null . program) $ traceAll machine
  putStr "part-1: "
  print $ sum $ strength <$> filter interesting trace
  putStrLn "part-2: "
  mapM_ putStrLn $ chunksOf 40 $ disp . lit <$> trace

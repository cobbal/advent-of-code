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
import Control.Monad.State
import Data.Foldable
import Debug.Trace
import System.Environment

breakGroups :: (a -> Bool) -> [a] -> [[a]]
breakGroups _ [] = []
breakGroups test xs = group : breakGroups test (drop 1 rest)
  where (group, rest) = break test xs

data Monkey = Monkey { _items :: [Int]
                     , operation :: Op
                     , test :: Int
                     , ifTrue :: Int
                     , ifFalse :: Int
                     , _inspections :: Int
                     } deriving (Eq, Ord, Show)

data Op = Plus Operand Operand
        | Times Operand Operand
        deriving (Eq, Ord, Show)

data Operand = Old
             | Int Int
             deriving (Eq, Ord, Show)

makeLenses ''Monkey

parseMonkey :: [[String]] -> Monkey
parseMonkey [ ["Monkey", _]
            , ("Starting" : "items:" : start)
            , ["Operation:", "new", "=", x0, op, x1]
            , ["Test:", "divisible", "by", d]
            , ["If", "true:", "throw", "to", "monkey", t]
            , ["If", "false:", "throw", "to", "monkey", f]
            ] = Monkey
                (map read (map (filter (/= ',')) start))
                (parseOp op (parseOperand x0) (parseOperand x1))
                (read d)
                (read t)
                (read f)
                0
parseMonkey m = error $ "bad monkey: " ++ show m

parseOp :: String -> Operand -> Operand -> Op
parseOp "+" = Plus
parseOp "*" = Times

parseOperand :: String -> Operand
parseOperand "old" = Old
parseOperand i = Int (read i)

evalOperand :: Operand -> Int -> Int
evalOperand Old = id
evalOperand (Int i) = const i

evalOp :: Op -> Int -> Int
evalOp (Plus x y) old = evalOperand x old + evalOperand y old
evalOp (Times x y) old = evalOperand x old * evalOperand y old

type MState = State [Monkey] ()

playRound :: Int -> Int -> MState
playRound relief modulo = do
  n <- length <$> get
  flip mapM_ [0..n-1] $ \i -> do
    m <- fromJust <$> preuse (ix i)
    xs <- ix i . items <<.= []
    ix i . inspections += length xs
    flip mapM_ xs $ \item -> do
      let newVal = (evalOp (operation m) item `div` relief) `mod` modulo
      let target = if newVal `rem` test m == 0 then ifTrue m else ifFalse m
      ix target . items %= flip snoc newVal

main :: IO ()
main = do
  target <- fromMaybe "." . listToMaybe <$> getArgs
  monkeyStrings <- breakGroups null . lines <$> readFile (target ++ "/input-real.txt")
  let monkeys = map (parseMonkey . map words) monkeyStrings
  let testLCM = foldl lcm 1 (map test monkeys)
  print testLCM
  let run relief rounds = snd $ runState (replicateM_ rounds (playRound relief testLCM)) monkeys
  putStrLn "part-1: "
  print $ product $ take 2 $ reverse $ L.sort (map _inspections (run 3 20))
  putStrLn "part-2: "
  print $ product $ take 2 $ reverse $ L.sort (map _inspections (run 1 10000))

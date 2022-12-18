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

data Vox = Vox { x :: Int, y :: Int, z :: Int }
         deriving (Eq, Ord, Show)

vox :: String -> Vox
vox s = Vox x y z
  where (x, y, z) = read $ "(" ++ s ++ ")"

type MineCraft = S.Set Vox
data Range = Range { lo :: Int, hi :: Int }

rangeList :: Range -> [Int]
rangeList r = [lo r..hi r]

inRange :: Int -> Range -> Bool
inRange i r = lo r <= i && i <= hi r

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

neighbors :: Vox -> [Vox]
neighbors v = [ Vox (x v + 1) (y v) (z v)
              , Vox (x v - 1) (y v) (z v)
              , Vox (x v) (y v + 1) (z v)
              , Vox (x v) (y v - 1) (z v)
              , Vox (x v) (y v) (z v + 1)
              , Vox (x v) (y v) (z v - 1)
              ]

surface :: MineCraft -> Vox -> Int
surface mc v = count (`S.notMember` mc) (neighbors v)

totalSurface :: MineCraft -> Int
totalSurface mc = sum $ surface mc <$> S.toList mc

setBind :: (Ord a, Ord b) => (a -> S.Set b) -> S.Set a -> S.Set b
setBind f s = S.unions $ f <$> S.toList s

finiteFix :: Eq a => a -> (a -> a) -> a
finiteFix x f
  | x == x' = x
  | otherwise = finiteFix x' f
  where x' = f x

interior :: MineCraft -> Vox -> Bool
interior mc = (`S.notMember` outside)
  where
    (xs, ys, zs) = mcBounds mc

    floodable :: Vox -> Bool
    floodable v = inRange (x v) xs && inRange (y v) ys && inRange (z v) zs && v `S.notMember` mc

    expand :: MineCraft -> MineCraft
    expand mc = S.filter floodable $ mc `S.union` setBind (S.fromList . neighbors) mc

    outside :: MineCraft
    outside = finiteFix (S.singleton $ Vox (lo xs) (lo ys) (lo zs)) expand

mcBounds :: MineCraft -> (Range, Range, Range)
mcBounds mc = ( Range (minimum (S.map x mc) - 1) (maximum (S.map x mc) + 1)
              , Range (minimum (S.map y mc) - 1) (maximum (S.map y mc) + 1)
              , Range (minimum (S.map z mc) - 1) (maximum (S.map z mc) + 1)
              )

complement :: MineCraft -> MineCraft
complement mc = S.fromList $ do
  let (xs, ys, zs) = mcBounds mc
  v <- Vox <$> rangeList xs <*> rangeList ys <*> rangeList zs
  if v `S.member` mc then [] else [v]

main :: IO ()
main = do
  mc <- S.fromList <$> map vox . lines <$> readFile "input-real.txt"
  putStrLn "part-1:"
  print $ totalSurface mc
  putStrLn "part-2:"
  let closed = S.filter (interior mc) (complement mc) `S.union` mc
  print $ totalSurface closed

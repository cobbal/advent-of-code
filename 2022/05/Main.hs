import System.IO
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Control.Lens
import Data.List.Lens

skipTake :: Int -> [a] -> [a]
skipTake n = helper 0
  where
    helper _ [] = []
    helper 0 (x : xs) = x : helper (n - 1) xs
    helper i (_ : xs) = helper (i - 1) xs

type Ship = [String]
data Move = Move
  { moveCount :: Int
  , moveFrom :: Int
  , moveTo :: Int
  } deriving (Eq, Ord, Show)

parsePicture :: [String] -> Ship
parsePicture pic = map (filter (/= ' ')) $ skipTake 4 $ tail $ transpose pic

parseMove :: String -> Move
parseMove line = case words line of
  [_, x, _, y, _, z] -> Move (read x) (read y - 1) (read z - 1)

popCrates :: Ship -> Int -> Int -> (String, Ship)
popCrates ship i count =
  ( take count $ fromJust $ firstOf (ix i) ship
  , over (ix i) (drop count) ship
  )

operateCrane :: (String -> String) -> Ship -> Move -> Ship
operateCrane flipper ship move = over (ix (moveTo move)) (flipper crates ++) unloadedShip
  where (crates, unloadedShip) = popCrates ship (moveFrom move) (moveCount move)

main :: IO ()
main = do
  (picture, _ : moveStrings)  <- break null <$> lines <$> readFile "input-real.txt"
  let ship = parsePicture picture
  let moves = map parseMove moveStrings
  putStr "part-1: "
  putStrLn $ map head $ foldl (operateCrane reverse) ship moves
  putStr "part-2: "
  putStrLn $ map head $ foldl (operateCrane id) ship moves

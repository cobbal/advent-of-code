{-# LANGUAGE TemplateHaskell, LambdaCase #-}

import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import Control.Lens
import Data.List.Lens
import Control.Monad.State

type Dir = M.Map String File
data File = File Int
          | Dir Dir
  deriving (Eq, Ord, Show)

data System = System { _pwd :: [String]
                     , _fs :: Dir
                     } deriving (Eq, Ord, Show)

makePrisms ''File
makeLenses ''System

subdir :: [String] -> Lens Dir Dir (Maybe Dir) Dir
subdir [] = \f -> f . Just
subdir (name : path) = subdir path . lens get set
  where
    get :: Maybe Dir -> Maybe Dir
    get dir = do
      Dir dir <- M.lookup name =<< dir
      pure dir
    set :: Maybe Dir -> Dir -> Dir
    set oldDir newDir = M.insert name (Dir newDir) (fromMaybe M.empty oldDir)

insert :: String -> File -> State System ()
insert name file = do
  wd <- use pwd
  fs . subdir wd %= M.insert name file . fromMaybe M.empty

process :: [String] -> State System ()
process ["$", "cd", "/"] = pwd .= []
process ["$", "cd", ".."] = pwd %= tail
process ["$", "cd", dir] = pwd %= (dir :)

process ["$", "ls"] = pure ()
process ["dir", name] = insert name (Dir M.empty)
process [size, name] = insert name (File $ read size)

process unknown = error $ "Unknown command: " ++ unwords unknown

processAll :: [[String]] -> System
processAll commands = execState (mapM_ process commands) (System undefined M.empty)

dirSize :: Dir -> Int
dirSize = M.foldr ((+) . fileSize) 0
  where
    fileSize :: File -> Int
    fileSize (File s) = s
    fileSize (Dir d) = dirSize d

allDirs :: Dir -> [Dir]
allDirs = map snd . namedDirs "/"

namedDirs :: String -> Dir -> [(String, Dir)]
namedDirs name dir = (name, dir) : M.foldrWithKey fileDirs [] dir
  where
    fileDirs _ (File _) acc = acc
    fileDirs name (Dir d) acc = acc ++ namedDirs name d

summarizeDirectory :: (String, Dir) -> (String, Int)
summarizeDirectory (name, dir) = (name, dirSize dir)

main :: IO ()
main = do
  commands <- map words . lines <$> readFile "input-real.txt"
  let fs = _fs $ processAll commands
  let smallDirs = filter (<= 100000) $ map dirSize $ allDirs fs
  putStr "part-1: "
  print $ sum smallDirs

  let total = 70000000
  let needed = 30000000
  let used = dirSize fs
  let deletionNeeded = used - (total - needed)

  let candidates = L.sortOn snd $ filter ((>= deletionNeeded) . snd) $ summarizeDirectory <$> namedDirs "/" fs
  putStr "part-2: "
  print $ snd $ head candidates

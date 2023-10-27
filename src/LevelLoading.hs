module LevelLoading where

import Model
import Grid
import System.Random
import Data.Maybe
import Data.List

loadLevel :: GameState -> Int -> IO GameState --read a level file
loadLevel gstate level = return gstate {maze = levelcontents gstate!!(level - 1), status = Running} --NOG RANDOM CIRCLES VULLEN

circleFiller :: Int -> Tile -> Tile
circleFiller a (Empty coordinate) | a < 7 = Circle coordinate
                                  | otherwise = Empty coordinate
circleFiller _ nonEmpty = nonEmpty

randomRange :: (Int, Int) -> IO Int --generate random int
randomRange (x, y) = getStdRandom $ randomR (x, y)


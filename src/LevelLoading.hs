module LevelLoading where

import Model
import Grid
import System.Random
import Data.Maybe
import Data.List

loadLevel :: GameState -> Int -> [Int] -> GameState --loads a level based on a list of integers
loadLevel gstate level rL = let updatedMaze = levelcontents gstate!!(level - 1) --load the level
                                updatedGrid =  zipWith (curry (circleFiller updatedMaze)) rL (grid updatedMaze)  -- fill the maze with circles
                            in
                                gstate {maze =  updatedMaze {grid = updatedGrid}, status = Running, elapsedTime = 0, lastLevel = level} --set the correct gstate values

circleFiller :: Maze -> (Int, Tile) -> Tile -- fills Tile with 70% chance of having a circle if it is empty (based on a integer it gets a circle or not)
circleFiller m (rand, Empty coordinate) =
                                    let pmCoord = positionToCoord $ position $ pacman m
                                        redCoord = positionToCoord $ enemyposition $ redEnemy m
                                        pinkCoord = positionToCoord $ enemyposition $ pinkEnemy m
                                        orangeCoord = positionToCoord $ enemyposition $ orangeEnemy m
                                        blueCoord = positionToCoord $ enemyposition $ blueEnemy m
                                    in
                                    if rand < 7 &&
                                        pmCoord /= coordinate && --check if it is not on the same place as an enemy or pacman
                                        redCoord /= coordinate &&
                                        pinkCoord /= coordinate &&
                                        orangeCoord /= coordinate &&
                                        blueCoord /= coordinate
                                        then
                                            Circle coordinate
                                    else
                                        Empty coordinate
circleFiller _ (_, nonEmpty) = nonEmpty -- tile is not empty: don't fill it


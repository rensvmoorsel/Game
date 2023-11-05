module LevelLoading where

import Model
import Grid
import System.Random
import Data.Maybe
import Data.List

loadLevel :: GameState -> Int -> IO GameState --loads a level, IO because it containts randomness
loadLevel gstate level = do
                            let updatedMaze = levelcontents gstate!!(level - 1) --load the level
                            updatedGrid <-  mapM (circleFiller updatedMaze) (grid updatedMaze) -- fill the maze with circles
                            return gstate {maze =  updatedMaze {grid = updatedGrid}, status = Running, elapsedTime = 0} --set the correct gstate values

circleFiller :: Maze -> Tile -> IO Tile -- fills Tile with 70% chance of having a circle if it is empty
circleFiller m (Empty coordinate) = do 
                                    rand <- randomRange (0, 10)
                                    let pmCoord = positionToCoord $ position $ pacman m 
                                    let redCoord = positionToCoord $ enemyposition $ redEnemy m
                                    let pinkCoord = positionToCoord $ enemyposition $ pinkEnemy m
                                    let orangeCoord = positionToCoord $ enemyposition $ orangeEnemy m
                                    let blueCoord = positionToCoord $ enemyposition $ blueEnemy m
                                    if rand < 7 &&  
                                        pmCoord /= coordinate && --check if it is not on the same place as an enemy or pacman
                                        redCoord /= coordinate && 
                                        pinkCoord /= coordinate && 
                                        orangeCoord /= coordinate && 
                                        blueCoord /= coordinate 
                                        then
                                            return $ Circle coordinate
                                    else
                                        return $ Empty coordinate
circleFiller _ nonEmpty = return nonEmpty -- tile is not empty: don't fill it

randomRange :: (Int, Int) -> IO Int --generate random int
randomRange (x, y) = getStdRandom $ randomR (x, y)


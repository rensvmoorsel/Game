module LevelLoading where

import Model
import Grid
import System.Random
import Data.Maybe
import Data.List

loadLevel :: GameState -> Int -> IO GameState 
loadLevel gstate level = do
                            let updatedMaze = levelcontents gstate!!(level - 1)
                            updatedGrid <-  mapM (circleFiller updatedMaze) (grid updatedMaze)
                            return gstate {maze =  updatedMaze {grid = updatedGrid}, status = Running, elapsedTime = 0}

circleFiller :: Maze -> Tile -> IO Tile
circleFiller m (Empty coordinate) = do 
                                    rand <- randomRange (0, 10)
                                    let pmCoord = positionToCoord $ position $ pacman m
                                    let redCoord = positionToCoord $ enemyposition $ redEnemy m
                                    let pinkCoord = positionToCoord $ enemyposition $ pinkEnemy m
                                    let orangeCoord = positionToCoord $ enemyposition $ orangeEnemy m
                                    let blueCoord = positionToCoord $ enemyposition $ blueEnemy m
                                    if rand < 7 && 
                                        pmCoord /= coordinate && 
                                        redCoord /= coordinate && 
                                        pinkCoord /= coordinate && 
                                        orangeCoord /= coordinate && 
                                        blueCoord /= coordinate 
                                        then
                                            return $ Circle coordinate
                                    else
                                        return $ Empty coordinate
circleFiller _ nonEmpty = return nonEmpty

randomRange :: (Int, Int) -> IO Int --generate random int
randomRange (x, y) = getStdRandom $ randomR (x, y)


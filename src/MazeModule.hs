module MazeModule(Maze) where
import Update
import Drawing
import Model
import Grid
import Graphics.Gloss
import TileModule
import EnemyModule
import Pacman

instance Updatable Maze where
    updateObject maze dt gs = maze {
                                        pacman = updatedPacman {position = validatePosition nBlock differenceX differenceY updatedPacmanPosition (position updatedPacman) (direction updatedPacman)}, --update pacman
                                        grid = updatedGrid, --update all the tiles in the grid
                                        pinkEnemy = updateObject (pinkEnemy maze) dt gs, --update the enemies
                                        blueEnemy = updateObject (blueEnemy maze) dt gs,
                                        redEnemy = updateObject (redEnemy maze) dt gs,
                                        orangeEnemy = updateObject (orangeEnemy maze) dt gs
                                    }
                                    where
                                        updatedPacman = updateObject (pacman maze) dt gs --the updated pacman
                                        updatedGrid = map (\x -> updateObject x dt gs) (grid maze) --the updated grid
                                        updatedPacmanPosition = position updatedPacman --the position of the updated pacman
                                        nBlock = nextBlock updatedGrid (direction updatedPacman) $ positionToCoord $ position updatedPacman --check the block that pacman is looking at
                                        positionNextBlock = realPosition nBlock --calculate the position of the block
                                        differenceX = abs (x positionNextBlock - x updatedPacmanPosition) --check the x distance
                                        differenceY = abs (y positionNextBlock - y updatedPacmanPosition) -- check the y distance

instance Drawable Maze where
    draw (MkMaze g pm p b o r) = Pictures $ draw pm:draw p:draw b:draw o:draw r:map draw g 

validatePosition :: Tile -> Float -> Float -> Position -> Position -> Direction -> Position --check if the distance is to small, if so: correct it
validatePosition nextBlock _ differenceY updatedpmPosition pos Up | show nextBlock == "Wall" && differenceY < 30 = updatedpmPosition{y = y updatedpmPosition + (30 - differenceY)}
                                                                  | otherwise = pos
validatePosition nextBlock _ differenceY updatedpmPosition pos Down | show nextBlock == "Wall" && differenceY < 30 = updatedpmPosition{y = y updatedpmPosition - (30 - differenceY)}
                                                                    | otherwise = pos
validatePosition nextBlock differenceX _ updatedpmPosition pos Grid.Left | show nextBlock == "Wall" && differenceX < 30 = updatedpmPosition {x = x updatedpmPosition + (30 - differenceX)}
                                                                         | otherwise = pos
validatePosition nextBlock differenceX _ updatedpmPosition pos Grid.Right | show nextBlock == "Wall" && differenceX < 30 = updatedpmPosition {x = x updatedpmPosition - (30 - differenceX)}
                                                                          | otherwise = pos
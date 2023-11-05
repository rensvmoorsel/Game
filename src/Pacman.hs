module Pacman(PacMan) where
import Update
import Drawing
import Model
import Grid

instance Updatable PacMan where --update pacman
    updateObject pacman@(MkPacMan {position = pos, direction = dir}) deltaTime gstate = pacman {position = updatePosition 30 deltaTime pos validatedDir, direction = validatedDir}
                                                                        where
                                                                            expectedPosition = updatePosition 30 deltaTime pos updatedDir --expectedposition
                                                                            coord = positionToCoord expectedPosition --coordinate of the expectedpostion
                                                                            g = grid (maze gstate) --the grid
                                                                            updatedDir = updateDir (pressedKey gstate) dir --calculate the new direction
                                                                            validatedDir = validateDir dir coord g updatedDir --validate the new direction

instance Drawable PacMan where --draw the correct sprite based on mouthstatus and direction of pacman
    draw (MkPacMan (MkPosition x y) _ Closed image _ _ _ _) = moveSprite (x, y) image
    draw (MkPacMan (MkPosition x y) Grid.Left Open _ image _ _ _) = moveSprite (x, y) image
    draw (MkPacMan (MkPosition x y) Grid.Right Open _ _ image _ _) = moveSprite (x, y) image
    draw (MkPacMan (MkPosition x y) Up Open _ _ _ image _) = moveSprite (x, y) image
    draw (MkPacMan (MkPosition x y) Down Open _ _ _ _ image) = moveSprite (x, y) image

updateDir :: Key -> Direction -> Direction --update the direction based on the input
updateDir W _ = Up
updateDir A _ = Grid.Left
updateDir S _ = Down
updateDir D _ = Grid.Right
updateDir _ d = d

validateDir :: Direction -> Coordinate -> Grid -> Direction -> Direction --reset direction if now facing towards wall
validateDir oldDir coord g Up | show (topBlock g coord) == "Wall" = oldDir
                              | otherwise = Up
validateDir oldDir coord g Down | show (bottomBlock g coord) == "Wall" = oldDir
                                | otherwise = Down
validateDir oldDir coord g Grid.Left | show (leftBlock g coord) == "Wall" = oldDir
                                     | otherwise = Grid.Left
validateDir oldDir coord g Grid.Right | show (rightBlock g coord) == "Wall" = oldDir
                                      | otherwise = Grid.Right
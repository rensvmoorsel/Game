module Update where

import Model
import Grid

class Updatable a where
    updateObject :: a -> Float -> GameState -> a

instance Updatable PacMan where
    updateObject pacman@(MkPacMan pos dir mouth) deltaTime gstate = MkPacMan (updatePosition deltaTime pos validatedDir) validatedDir (updateMouth mouth)
                                                                        where
                                                                            updateMouth :: MouthStatus -> MouthStatus --set the correct mouth status
                                                                            updateMouth Open = Closed
                                                                            updateMouth Closed = Open

                                                                            updatePosition :: Float -> Position -> Direction -> Position --update the position based on the direction and deltatime
                                                                            updatePosition dt position@MkPosition{x = x, y = y} Up = position {y = y - dt * 30, x = roundTo30 x } --Deze 4 alleen als het blok erna geen wall is, anders: zet stil op afgeronde positie
                                                                            updatePosition dt position@MkPosition{x = x, y = y} Down = position {y = y + dt * 30, x = roundTo30 x }
                                                                            updatePosition dt position@MkPosition{x = x, y = y} Grid.Right = position {x = x + dt * 30, y = roundTo30 y  }
                                                                            updatePosition dt position@MkPosition{x = x, y = y} Grid.Left = position {x = x - dt * 30, y = roundTo30 y }

                                                                            updatedPosition = updatePosition deltaTime pos updatedDir

                                                                            coord = positionToCoord updatedPosition

                                                                            validateDir :: Direction -> Direction --reset direction if now facing towards wall
                                                                            validateDir Up | show (topBlock g coord) == "Wall" = direction pacman
                                                                                           | otherwise = Up
                                                                            validateDir Down | show (bottomBlock g coord) == "Wall" = direction pacman
                                                                                             | otherwise = Down
                                                                            validateDir Grid.Left | show (leftBlock g coord) == "Wall" = direction pacman
                                                                                                  | otherwise = Grid.Left
                                                                            validateDir Grid.Right | show (rightBlock g coord) == "Wall" = direction pacman
                                                                                                   | otherwise = Grid.Right

                                                                            roundTo30 :: Float -> Float --round to block if direction switched
                                                                            roundTo30 value = fromInteger (round (value / 30) * 30)

                                                                            g = grid (maze gstate)

                                                                            updateDir :: Key -> Direction -> Direction --update the direction based on the input
                                                                            updateDir W _ = Up
                                                                            updateDir A _ = Grid.Left
                                                                            updateDir S _ = Down
                                                                            updateDir D _ = Grid.Right
                                                                            updateDir _ d = d

                                                                            updatedDir = updateDir (pressedKey gstate) dir
                                                                            validatedDir = validateDir updatedDir


instance Updatable Tile where
    updateObject circle@(Circle coordinate) _ gstate
                                        | abs (xPm - x pos) < 24 && abs (yPm - y pos) < 24 = Empty coordinate --the distance is close enough to remove the circle
                                        | otherwise = circle --the distance is to big, so don't remove the circle
                                            where
                                                positionPm = position (pacman (maze gstate)) --get the position of the pacman
                                                xPm = x positionPm
                                                yPm = y positionPm
                                                pos = realPosition circle    --calculate the realposition of the circle
    updateObject x _ _ = x --it isn't a circle so it's a static object, keep it as it is

instance Updatable StatusGame where
    updateObject Paused _ gstate | pressedKey gstate == Esc = Running --the game is paused and esc is pressed: resume
                                 | otherwise = Paused
    updateObject Running _ gstate | pressedKey gstate == Esc = Paused --The game is running and esc is pressed: pause  --CHECK OOK NOG OF FINISHED
                                  | otherwise = Running
    updateObject s _ _ = s


instance Updatable GameState where
    updateObject _ secs gstate  | levelCompleted $ grid (maze gstate) = gstate {status = Complete}
                                | status gstate == Running = gstate {
                                                                        maze = updateObject (maze gstate) secs gstate, --update the maze 
                                                                        elapsedTime = elapsedTime gstate + secs, --update the elapsed time
                                                                        status = updateObject (status gstate) secs gstate, --update the game status
                                                                        pressedKey = None --reset the pressed key
                                                                    }
                                | otherwise = gstate { --game is paused or ended, don't update the maze
                                                            elapsedTime = elapsedTime gstate + secs, --update the elapsed time
                                                            status = updateObject (status gstate) secs gstate, --update the game status
                                                            pressedKey = None --reset the pressed key
                                                        }

levelCompleted :: Grid -> Bool --check if level is completed by checking if there are no circles
levelCompleted = foldr f True
                    where
                        f :: Tile -> Bool -> Bool
                        f Circle{} _ = False
                        f _ b = b


instance Updatable Maze where
    updateObject maze dt gs = maze {
                                        pacman = updatedPacman {position = validatePosition (position updatedPacman) (direction updatedPacman)}, --update pacman
                                        grid = updatedGrid --update all the tiles in the grid
                                    }
                                    where
                                        updatedPacman = updateObject (pacman maze) dt gs --the updated pacman
                                        updatedGrid = map (\x -> updateObject x dt gs) (grid maze) --the updated grid

                                        updatedPacmanPosition = position updatedPacman --the position of the updated pacman

                                        nBlock = nextBlock updatedGrid (direction updatedPacman) $ positionToCoord $ position updatedPacman --check the block that pacman is looking at
                                        positionNextBlock = realPosition nBlock --calculate the position of the block
                                        differenceX = abs (x positionNextBlock - x updatedPacmanPosition) --check the x distance
                                        differenceY = abs (y positionNextBlock - y updatedPacmanPosition) -- check the y distance

                                        validatePosition :: Position -> Direction -> Position --check if the distance is to small, if so: correct it
                                        validatePosition pos Up | show nBlock == "Wall" && differenceY < 30 = updatedPacmanPosition{y = y updatedPacmanPosition + (30 - differenceY)}  
                                                                | otherwise = pos
                                        validatePosition pos Down | show nBlock == "Wall" && differenceY < 30 = updatedPacmanPosition{y = y updatedPacmanPosition - (30 - differenceY)}
                                                                  | otherwise = pos
                                        validatePosition pos Grid.Left | show nBlock == "Wall" && differenceX < 30 = updatedPacmanPosition {x = x updatedPacmanPosition + (30 - differenceX)}  
                                                                       | otherwise = pos
                                        validatePosition pos Grid.Right | show nBlock == "Wall" && differenceX < 30 = updatedPacmanPosition {x = x updatedPacmanPosition - (30 - differenceX)} 
                                                                        | otherwise = pos
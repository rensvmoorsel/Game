module Update where

import Model

class Updatable a where
    updateObject :: a -> Float -> GameState -> a

instance Updatable PacMan where
    updateObject pacman@(MkPacMan pos dir mouth) deltaTime gstate = MkPacMan (updatePosition deltaTime pos updatedDir) updatedDir (updateMouth mouth)
                                                                        where
                                                                            updateMouth :: MouthStatus -> MouthStatus --set the correct mouth status
                                                                            updateMouth Open = Closed
                                                                            updateMouth Closed = Open

                                                                            updatePosition :: Float -> Position -> Direction -> Position --update the position based on the direction and deltatime
                                                                            updatePosition dt position@MkPosition{x = x, y = y} Up = position {y = y - dt * 30, x = fromInteger (round (x / 30) * 30) } --Deze 4 alleen als het blok erna geen wall is, anders: zet stil op afgeronde positie
                                                                            updatePosition dt position@MkPosition{x = x, y = y} Down = position {y = y + dt * 30, x = fromInteger (round (x / 30) * 30) }
                                                                            updatePosition dt position@MkPosition{x = x, y = y} Model.Right = position {x = x + dt * 30, y = fromInteger (round (y / 30) * 30)  }
                                                                            updatePosition dt position@MkPosition{x = x, y = y} Model.Left = position {x = x - dt * 30, y = fromInteger (round (y / 30) * 30)}
                                                                            
                                                                            updateDir :: Key -> Direction -> Direction --update the direction based on the input
                                                                            updateDir W d = Up --deze 4 alleen als blok erna leeg is
                                                                            updateDir A d = Model.Left
                                                                            updateDir S d = Down
                                                                            updateDir D d = Model.Right
                                                                            updateDir _ d = d
                                                                            updatedDir = updateDir (pressedKey gstate) dir

instance Updatable Tile where
    updateObject circle@Circle{ xCoord = xCoord, yCoord = yCoord} _ gstate 
                                        | abs (xPm - x pos) < 24 && abs (yPm - y pos) < 24 = Empty xCoord yCoord --the distance is close enough to remove the circle
                                        | otherwise = circle --the distance is to big, so don't remove the circle
                                            where
                                                positionPm = position (pacman (maze gstate)) --get the position of the pacman
                                                xPm = x positionPm 
                                                yPm = y positionPm
                                                pos = realPosition circle    --calculate the realposition of the circle
    updateObject x _ _ = x --it isn't a circle so it's a static object, keep it as it is

instance Updatable StatusGame where
    updateObject Ended _ _ = Ended
    updateObject Paused _ gstate | pressedKey gstate == Esc = Running --the game is paused and esc is pressed: resume
                                 | otherwise = Paused
    updateObject Running _ gstate | pressedKey gstate == Esc = Paused --The game is running and esc is pressed: pause  --CHECK OOK NOG OF FINISHED
                                  | otherwise = Running


instance Updatable GameState where
    updateObject _ secs gstate  | status gstate == Running = gstate {
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


instance Updatable Maze where
    updateObject maze dt gs = maze { 
                                        pacman = updateObject (pacman maze) dt gs, --update pacman
                                        grid = map (\x -> updateObject x dt gs) (grid maze) --update all the tiles in the grid
                                    }
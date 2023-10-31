{-# OPTIONS_GHC -Wno-missing-fields #-}
module Update where

import Model
import Grid

class Updatable a where
    updateObject :: a -> Float -> GameState -> a

instance Updatable PacMan where
    updateObject pacman@(MkPacMan {position = pos, direction = dir}) deltaTime gstate = pacman {position = updatePosition deltaTime pos validatedDir, direction = validatedDir}
                                                                        where
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
    updateObject Running _ gstate | levelCompleted = Complete
                                  | levelFailed = Failed
                                  | pressedKey gstate == Esc = Paused --The game is running and esc is pressed: pause  --CHECK OOK NOG OF FINISHED
                                  | otherwise = Running
                                  where
                                    m = maze gstate
                                    g = grid m

                                    levelCompleted :: Bool --check if level is completed by checking if there are no circles
                                    levelCompleted = foldr f True g
                                                        where
                                                            f :: Tile -> Bool -> Bool
                                                            f Circle{} _ = False
                                                            f _ b = b

                                    levelFailed :: Bool
                                    levelFailed =  False--check if collision with enemy
    updateObject s _ _ = s


instance Updatable GameState where
    updateObject _ secs gstate  | status gstate == Running = gstate {
                                                                        maze = updateObject (m {pacman = pm {mouthStatus = updateMouthStatus $ elapsedTime gstate }}) secs gstate, --update the maze 
                                                                        elapsedTime = elapsedTime gstate + secs, --update the elapsed time
                                                                        pressedKey = None, --reset the pressed key,
                                                                        status = updatedStatus
                                                                    }

                                | otherwise = gstate { --game is paused or ended, don't update the maze
                                                            elapsedTime = elapsedTime gstate + secs, --update the elapsed time
                                                            status = updateObject (status gstate) secs gstate, --update the game status
                                                            pressedKey = None --reset the pressed key
                                                        }
                                where
                                    m = maze gstate
                                    pm = pacman m
                                    g = grid m

                                    updateMouthStatus :: Float -> MouthStatus
                                    updateMouthStatus time | round (4 * time) `mod` 2 == 1 = Open
                                                           | otherwise = Closed

                                    updatedStatus = updateObject (status gstate) secs gstate


instance Updatable Maze where
    updateObject maze dt gs = maze {
                                        pacman = updatedPacman {position = validatePosition (position updatedPacman) (direction updatedPacman)}, --update pacman
                                        grid = updatedGrid, --update all the tiles in the grid
                                        pinkEnemy = updateObject (pinkEnemy maze) dt gs,
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

                                        validatePosition :: Position -> Direction -> Position --check if the distance is to small, if so: correct it
                                        validatePosition pos Up | show nBlock == "Wall" && differenceY < 30 = updatedPacmanPosition{y = y updatedPacmanPosition + (30 - differenceY)}
                                                                | otherwise = pos
                                        validatePosition pos Down | show nBlock == "Wall" && differenceY < 30 = updatedPacmanPosition{y = y updatedPacmanPosition - (30 - differenceY)}
                                                                  | otherwise = pos
                                        validatePosition pos Grid.Left | show nBlock == "Wall" && differenceX < 30 = updatedPacmanPosition {x = x updatedPacmanPosition + (30 - differenceX)}
                                                                       | otherwise = pos
                                        validatePosition pos Grid.Right | show nBlock == "Wall" && differenceX < 30 = updatedPacmanPosition {x = x updatedPacmanPosition - (30 - differenceX)}
                                                                        | otherwise = pos

instance Updatable Enemy where
    updateObject e dt gstate = e {enemyposition = updatePosition dt (enemyposition e) direction, enemydirection = direction }
                                where
                                    m = maze gstate
                                    g = grid m
                                    pm = pacman m
                                    pmCoordinate = positionToCoord $ position pm
                                    oldDirection = enemydirection e
                                    ePosition = enemyposition e
                                    eCoord | oldDirection == Up = positionToCoord $ ePosition {y = y ePosition + 15}
                                           | oldDirection == Down = positionToCoord $ ePosition {y = y ePosition - 15}
                                           | oldDirection == Grid.Right = positionToCoord $ ePosition {x = x ePosition - 15}
                                           | oldDirection == Grid.Left = positionToCoord $ ePosition {x = x ePosition + 15}
                                    eXCoord = xCoord eCoord
                                    eYCoord = yCoord eCoord
                                    direction = aimDirection

                                    updatePosition :: Float -> Position -> Direction -> Position --update the position based on the direction and deltatime
                                    updatePosition dt position@MkPosition{x = x, y = y} Up = position {y = y - dt * 30, x = roundTo30 x } --Deze 4 alleen als het blok erna geen wall is, anders: zet stil op afgeronde positie
                                    updatePosition dt position@MkPosition{x = x, y = y} Down = position {y = y + dt * 30, x = roundTo30 x }
                                    updatePosition dt position@MkPosition{x = x, y = y} Grid.Right = position {x = x + dt * 30, y = roundTo30 y  }
                                    updatePosition dt position@MkPosition{x = x, y = y} Grid.Left = position {x = x - dt * 30, y = roundTo30 y }

                                    aimDirection :: Direction
                                    aimDirection = bfs (filter (\c -> isEmpty (snd c) g) [(Grid.Left, eCoord {xCoord = eXCoord - 1}), (Grid.Right, eCoord {xCoord = eXCoord + 1}), (Up, eCoord{yCoord = eYCoord - 1}), (Down, eCoord {yCoord = eYCoord + 1})]) [] (aimPosition e m) g -- bfs naar richting met kortste route naar aimposition enemy

                                    xDistance = abs $ xCoord pmCoordinate - eXCoord
                                    yDistance = abs $ yCoord pmCoordinate - eYCoord
                                    distanceToPacman = sqrt $ (fromIntegral xDistance ^ 2) + (fromIntegral yDistance ^ 2)

                                    aimPosition :: Enemy -> Maze -> Coordinate
                                    aimPosition MkEnemy {enemycolor = Red} m = pmCoordinate--red: kortste route pacman
                                    aimPosition MkEnemy {enemycolor = Pink} m = pmCoordinate --nblocksInFrontCoordinate 4 (direction pm) pmCoordinate--pink: kortste route 4 plekken voor pacman (als die er is, anders minder)
                                    aimPosition MkEnemy {enemycolor = Orange} m = pmCoordinate -- | distanceToPacman > 8 = pmCoordinate --if not close enough: move to pacman
                                                                                -- | otherwise = nblocksInFrontCoordinate 32 Down $ nblocksInFrontCoordinate 28 Grid.Left eCoord --otherwise move to left bottom
                                    aimPosition MkEnemy {enemycolor = Blue} m = pmCoordinate--blue: trek een lijn van rood naar 2 plekken voor pacman, trek deze 2 keer de afstand van rood naar pacman door

                                    nblocksInFrontCoordinate :: Int -> Direction -> Coordinate -> Coordinate
                                    nblocksInFrontCoordinate n d coordinate | show (g!!coordToGridIndex coordinate) == "Wall"
                                                                                || xNewCoordinate < 1 || xNewCoordinate > 28
                                                                                || yNewCoordinate < 1 || yNewCoordinate > 32
                                                                                     = nblocksInFrontCoordinate (n - 1) d coordinate
                                                                            | otherwise = newCoordinate
                                                                                where
                                                                                    newCoordinate = translateCoordinate n d coordinate
                                                                                    xNewCoordinate = xCoord newCoordinate
                                                                                    yNewCoordinate = yCoord newCoordinate

                                    translateCoordinate :: Int -> Direction -> Coordinate -> Coordinate
                                    translateCoordinate n Up (MkCoordinate x y) = MkCoordinate x (y + n)
                                    translateCoordinate n Down (MkCoordinate x y) = MkCoordinate x (y - n)
                                    translateCoordinate n Grid.Left (MkCoordinate x y) = MkCoordinate (x - n) y
                                    translateCoordinate n Grid.Right (MkCoordinate x y) = MkCoordinate (x + n) y

bfs :: [(Direction, Coordinate)] -> [Coordinate] -> Coordinate -> Grid -> Direction
bfs queue@(x:xs) alreadyChecked target grid | target == coord = dir
                                            | otherwise = bfs (xs ++ zip (replicate (length neighbours) dir) neighbours) (coord:alreadyChecked) target grid
                                where
                                    dir = fst x
                                    coord = snd x
                                    neighbours = filter (`notElem` alreadyChecked) $ emptyNeighbours (snd x) grid

emptyNeighbours :: Coordinate -> Grid -> [Coordinate]
emptyNeighbours MkCoordinate {xCoord = x, yCoord = y} grid = filter f [MkCoordinate x (y + 1), MkCoordinate x (y -1), MkCoordinate (x + 1) y, MkCoordinate (x - 1) y]
                                                            where
                                                                f :: Coordinate -> Bool
                                                                f c = isEmpty c grid
isEmpty :: Coordinate -> Grid -> Bool
isEmpty c grid = show (grid!!coordToGridIndex c) /= "Wall"


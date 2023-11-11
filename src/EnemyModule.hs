module EnemyModule(Enemy) where
import Update
import Drawing
import Grid
import Model
import Graphics.Gloss

instance Drawable Enemy where --draw the correct image based on the direction of the enemy
    draw e@(MkEnemy {enemyposition = MkPosition {x = x, y = y}, enemydirection = Grid.Left, enemyspriteLeft = image}) = moveSprite (x, y) image            
    draw e@(MkEnemy {enemyposition = MkPosition {x = x, y = y}, enemydirection = Grid.Right, enemyspriteRight = image}) = moveSprite (x, y) image               
    draw e@(MkEnemy  {enemyposition = MkPosition {x = x, y = y}, enemydirection = Up, enemyspriteUp = image}) = moveSprite (x, y) image               
    draw e@(MkEnemy  {enemyposition = MkPosition {x = x, y = y}, enemydirection = Down, enemyspriteDown = image}) = moveSprite (x, y) image

instance Updatable Enemy where
    updateObject e dt gstate = e {enemyposition = updatePosition 20 dt (enemyposition e) aimDirection, enemydirection = aimDirection } --update the enemy
                                where
                                    m = maze gstate --the maze
                                    g = grid m --the grid
                                    pm = pacman m --pacman
                                    pmPosition = position pm --position of pacman
                                    pmCoordinate = positionToCoord pmPosition --coordinat in the grid of pacman
                                    oldDirection = enemydirection e --the original direction of the enemy
                                    ePosition = enemyposition e --the original position of the enemy
                                    eCoord | oldDirection == Up = positionToCoord $ ePosition {y = y ePosition + 15} --calculate corrected coordinate of the enemy
                                           | oldDirection == Down = positionToCoord $ ePosition {y = y ePosition - 15}
                                           | oldDirection == Grid.Right = positionToCoord $ ePosition {x = x ePosition - 15}
                                           | oldDirection == Grid.Left = positionToCoord $ ePosition {x = x ePosition + 15}
                                    
                                    eXCoord = xCoord eCoord -- the x and y values of the coordinate
                                    eYCoord = yCoord eCoord

                                    --the new direction of the enemy
                                    aimDirection = findShortestRoute (filter (\c -> isEmpty (snd c) g) [(Grid.Left, eCoord {xCoord = eXCoord - 1}), (Grid.Right, eCoord {xCoord = eXCoord + 1}), (Up, eCoord{yCoord = eYCoord - 1}), (Down, eCoord {yCoord = eYCoord + 1})]) (aimPosition e m) g -- bfs naar richting met kortste route naar aimposition enemy

                                    --calculate the distance to pacman
                                    xDistance = abs $ fromIntegral (xCoord pmCoordinate) - fromIntegral eXCoord
                                    yDistance = abs $ fromIntegral (yCoord pmCoordinate) - fromIntegral eYCoord
                                    distanceToPacman = sqrt $ xDistance ^ 2 + yDistance ^ 2

                                    --calculate the position the enemy must aim for
                                    aimPosition MkEnemy {enemycolor = Red} m = pmCoordinate--red: targets pacman
                                    aimPosition MkEnemy {enemycolor = Pink} m | distanceToPacman > 4 = nblocksInFrontCoordinate g 4 (direction pm) pmCoordinate--pink: targets the block 4 blocks in front of pacman, 
                                                                                | otherwise = pmCoordinate --except when it is closer then that, then it follows pacman
                                    aimPosition MkEnemy {enemycolor = Orange} m | distanceToPacman > 8 = pmCoordinate --if further then 8 blocks from pacman: move to pacman
                                                                                | otherwise = nblocksInFrontCoordinate g 32 Down $ nblocksInFrontCoordinate g 28 Grid.Left eCoord --otherwise: its scared for pacman and moves to bottom left
                                    aimPosition MkEnemy {enemycolor = Blue} m = findNearestEmpty g destinationCoord--blue: trek een lijn van rood naar 2 plekken voor pacman, trek deze 2 keer de afstand van rood naar pacman door
                                                                                where
                                                                                    redPosition = enemyposition $ redEnemy m --NOG VALIDATEN OF HET RESULTAAT GEEN WALL IS, ALS DAT ZO IS: ZOEK DICHTSBIJZIJNDE LEGE
                                                                                    xDistancePacmanToRed = abs $ x pmPosition - x redPosition
                                                                                    yDistancePacmanToRed = abs $ y pmPosition - y redPosition
                                                                                    destinationVector = MkVector2 (x pmPosition) (y pmPosition) + MkVector2 (2 * xDistancePacmanToRed) (2 * yDistancePacmanToRed)
                                                                                    destinationCoord = positionToCoord $ MkPosition (vecX destinationVector) (vecY destinationVector)

findNearestEmpty :: Grid -> Coordinate -> Coordinate -- find the nearest empty coordinate with bfs
findNearestEmpty g c = findNearestEmpty' [validateCoordinate c] [] --set the alreadychecked to an empty list
                                where
                                    findNearestEmpty' ::  [Coordinate] -> [Coordinate] -> Coordinate
                                    findNearestEmpty' queue'@(x:xs) alreadyChecked | show (g!!coordToGridIndex x) /= "Wall" = x --if it is not a wall, its empty: return the coordinate
                                                                                    | otherwise = findNearestEmpty' (xs ++ n) (x:alreadyChecked) -- otherwise: add the neighbours to the queue and add x to the alreadychecked list
                                                                                    where
                                                                                        n = filter (\neighbour -> validateCoordinate neighbour == neighbour && neighbour `notElem` alreadyChecked) $ neighbours x --get all neighbours that are on a right coordinate, and are not checked already

findShortestRoute :: [(Direction, Coordinate)] -> Coordinate -> Grid -> Direction -- find direction for the shortest route to coordinate with bfs
findShortestRoute queue target grid = findShortestRoute' queue [] --set the alreadychecked to an empty list
                                where
                                    findShortestRoute' ::  [(Direction, Coordinate)] -> [Coordinate] -> Direction
                                    findShortestRoute' queue'@(x:xs) alreadyChecked | target == coord = dir --if target is the coordinate that is checked: return the direction that the enemy started with for this route
                                                                                    | otherwise = findShortestRoute' (xs ++ zip (replicate (length n) dir) n) (coord:alreadyChecked) --otherwise, add all empty neighbours to the queue and add this coordinate to the alreadychecked list
                                                                                        where
                                                                                            dir = fst x --the direction that started the route
                                                                                            coord = snd x --the coordinate 
                                                                                            n = filter (`notElem` alreadyChecked) $ emptyNeighbours (snd x) grid --the empty neighbours that arent checked already

emptyNeighbours :: Coordinate -> Grid -> [Coordinate] --returns the empty neighbour tiles
emptyNeighbours c grid = filter f $ neighbours c --filters all the neighbours that are empty
                        where
                            f :: Coordinate -> Bool -- checks if the tile is empty
                            f c = isEmpty c grid

neighbours :: Coordinate -> [Coordinate] --returns all neighbours of a coordinate
neighbours (MkCoordinate x y) = [MkCoordinate x (y + 1), MkCoordinate x (y - 1), MkCoordinate (x + 1) y, MkCoordinate (x - 1) y]

isEmpty :: Coordinate -> Grid -> Bool --checks if tile is not a wall
isEmpty c grid = show (grid!!coordToGridIndex c) /= "Wall"
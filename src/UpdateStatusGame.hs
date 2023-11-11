module UpdateStatusGame(StatusGame) where
import Update
import Model
import Grid

instance Updatable StatusGame where
    updateObject Paused _ gstate | pressedKey gstate == Esc = Running --the game is paused and esc is pressed: resume
                                 | otherwise = Paused
    updateObject Running _ gstate | levelCompleted g = Complete
                                  | levelFailed m = Failed
                                  | pressedKey gstate == Esc = Paused --The game is running and esc is pressed: pause  --CHECK OOK NOG OF FINISHED
                                  | otherwise = Running
                                  where
                                    m = maze gstate
                                    g = grid m
    updateObject s _ _ = s

levelCompleted :: Grid -> Bool --check if level is completed by checking if there are no circles
levelCompleted g = foldr f True g
                    where
                        f :: Tile -> Bool -> Bool
                        f Circle{} _ = False
                        f _ b = b

levelFailed :: Maze -> Bool --check if level is failed (check if there is an enemy that collides with pacman)
levelFailed m = foldr f False [redEnemy m, blueEnemy m, pinkEnemy m, orangeEnemy m]
                where
                    f e b = b || enemyCollides (pacman m) e

enemyCollides :: PacMan -> Enemy -> Bool --check if pacman is too close to an enemy
enemyCollides pm e = distanceToPacman < 24.5
        where
            xDistance = abs $ x (position pm) - x (enemyposition e)
            yDistance = abs $ y (position pm) - y (enemyposition e)
            distanceToPacman = sqrt $  xDistance ^ 2 + yDistance ^ 2
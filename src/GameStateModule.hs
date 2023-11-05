module GameStateModule(GameState) where
import Drawing
import Update
import Graphics.Gloss
import Model
import MazeModule
import UpdateStatusGame

instance Updatable GameState where
    updateObject _ secs gstate  | status gstate == Running = gstate { -- game is running: update the maze, time and status and reset the pressed key
                                                                        maze = updateObject (m {pacman = pm {mouthStatus = updateMouthStatus $ elapsedTime gstate }}) secs gstate, --update the maze 
                                                                        elapsedTime = elapsedTime gstate + secs, --update the elapsed time
                                                                        pressedKey = None, --reset the pressed key,
                                                                        status = updatedStatus --update the game status
                                                                    }

                                | otherwise = gstate { --game is paused or ended, don't update the maze
                                                            elapsedTime = elapsedTime gstate + secs, --update the elapsed time
                                                            status = updateObject (status gstate) secs gstate, --update the game status
                                                            pressedKey = None --reset the pressed key
                                                        }
                                where
                                    m = maze gstate -- the maze
                                    pm = pacman m --pacman
                                    g = grid m --the grid
                                    updatedStatus = updateObject (status gstate) secs gstate --the updated status

instance Drawable GameState where
    draw GameState {status = Running, maze = maze} = draw maze --the game is running: draw the maze
    draw GameState {status = Paused, maze = maze} = Color red $ Pictures [draw maze, scale 0.2 0.2 $ translate (-100) 0 $ text "Paused "] --the game is paused: draw the maze and "paused" above it
    draw GameState {unlockedLevels = levels} = Color white $ scale 0.2 0.2 $ Pictures --the game is not active: show the levels that are unlocked
                                                                                    [translate (-2000) 0 $ text $ "You have unlocked levels: " ++ foldr (\level oldString -> show level ++ " " ++ oldString) "" levels,
                                                                                        translate (-2000) (-200) $ text "Hit the number keys to select level" ] 

updateMouthStatus :: Float -> MouthStatus -- update the mouthstatus based on the gametime to get an animation
updateMouthStatus time | round (4 * time) `mod` 2 == 1 = Open
                        | otherwise = Closed
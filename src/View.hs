-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Maybe (mapMaybe)
import Drawing

view :: GameState -> IO Picture --draw the impure parts
view gameState@(GameState maze status input elapsedTime prevKey lastLevel) | status == Paused 
                                                                        = do --the game is paused, draw the state with pause screen over it
                                                                            gameScreen <- view (GameState maze Running input elapsedTime prevKey lastLevel)
                                                                            pauseScreen <- loadBMP "src\\Images\\PauseScreen.bmp"
                                                                            return $ Pictures [gameScreen, pauseScreen]
                                                                    | status == Running
                                                                        = do --draw the gamestate
                                                                            pacman <- drawBMP $ pacman maze
                                                                            p <- drawBMP $ pinkEnemy maze
                                                                            b <- drawBMP $ blueEnemy maze
                                                                            o <- drawBMP $ orangeEnemy maze
                                                                            r <- drawBMP $ redEnemy maze
                                                                            return $ Pictures [pacman, p, b, o, r, viewPure gameState]
                                                                    | otherwise = loadBMP "src\\Images\\StartScreen.bmp"

viewPure :: GameState -> Picture --draw the pure parts of the game (the grid)
viewPure gstate = Pictures $ map draw $ grid $ maze gstate

-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Maybe (mapMaybe)
import Drawing

view :: GameState -> IO Picture --draw the impure parts
view gameState@(GameState maze Ended input elapsedTime prevKey) = loadBMP "src\\Images\\StartScreen.bmp" --the game is ended, draw the start screen
view gameState@(GameState maze Paused input elapsedTime prevKey) = do --the game is paused, draw the state with pause screen over it
                                                        gameScreen <- view (GameState maze Running input elapsedTime prevKey)
                                                        pauseScreen <- loadBMP "src\\Images\\PauseScreen.bmp"
                                                        return $ Pictures [gameScreen, pauseScreen]
view gameState@(GameState maze  _ _ _ _) = do --draw the gamestate
                                        pacman <- drawBMP $ pacman maze
                                        p <- drawBMP $ pinkEnemy maze
                                        b <- drawBMP $ blueEnemy maze
                                        o <- drawBMP $ orangeEnemy maze
                                        r <- drawBMP $ redEnemy maze
                                        return $ Pictures [pacman, p, b, o, r, viewPure gameState]

viewPure :: GameState -> Picture --draw the pure parts of the game (the grid)
viewPure (GameState maze _ _ _ _) = Pictures $ map draw (grid maze)

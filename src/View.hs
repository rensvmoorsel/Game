-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Maybe (mapMaybe)
import Drawing

view :: GameState -> IO Picture
view gameState@(GameState maze Ended) = do
                                                        gameScreen <- view (GameState maze Running)
                                                        pauseScreen <- loadBMP "src\\Images\\StartScreen.bmp"
                                                        return $ Pictures [gameScreen, pauseScreen]
view gameState@(GameState maze Paused) = do
                                                        gameScreen <- view (GameState maze Running)
                                                        pauseScreen <- loadBMP "src\\Images\\PauseScreen.bmp"
                                                        return $ Pictures [gameScreen, pauseScreen]
view gameState@(GameState (MkMaze 
                            {pacman = pacman, 
                            pinkEnemy = pinkEnemy, 
                            blueEnemy = blueEnemy, 
                            orangeEnemy = orangeEnemy, 
                            redEnemy = redEnemy}) 
                                _) = do
                                        pacman <- drawBMP pacman
                                        p <- drawBMP pinkEnemy
                                        b <- drawBMP blueEnemy
                                        o <- drawBMP orangeEnemy
                                        r <- drawBMP redEnemy
                                        return $ Pictures [pacman, p, b, o, r, viewPure gameState]

viewPure :: GameState -> Picture
viewPure (GameState (MkMaze walls _ _ _ _ _) _) = Pictures $ map draw walls

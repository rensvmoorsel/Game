-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = viewPure

viewPure :: GameState -> IO Picture
viewPure (GameState (MkMaze walls circles pacman pink blue orange red) _) = do
                                                                                pm <- drawPacMan pacman
                                                                                return $ Pictures $ pm:(map drawWall walls ++ map drawCircle circles)

drawWall :: Wall -> Picture
drawWall (MkLine (MkPosition x1 y1) (MkPosition x2 y2))  = Color white $ Line [(x1, y1), (x2, y2)]

drawCircle :: Circle -> Picture
drawCircle (MkPosition x y) = Color white $ Translate x y $ ThickCircle 3 6

drawPacMan :: PacMan -> IO Picture
drawPacMan (MkPacMan (MkPosition x y) Model.Left Open) = do
                                                            pacManImage <- loadBMP "PacmanOpenLeft.png"
                                                            return $ Translate x y pacManImage

--drawEnemy :: Enemy -> Picture
--drawEnemy (MkEnemy color (MkPosition x y) Up) = Color color $ Translate x y $ 
--drawEnemy (MkEnemy color (MkPosition x y) Down) = Color color $ Translate x y $ 
--drawEnemy (MkEnemy color (MkPosition x y) Left) = Color color $ Translate x y $ 
--drawEnemy (MkEnemy color (MkPosition x y) Right) = Color color $ Translate x y $ 

-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gameState@(GameState maze@(MkMaze {}) Paused) = view (GameState maze Running) --EN NOG EEN PAUZESCHERM EROVERHEEN
view gameState@(GameState (MkMaze _ pm pink blue orange red) _) = do
                                                            pacman <- drawPacMan pm
                                                            return $ Pictures [pacman, viewPure gameState]

viewPure :: GameState -> Picture
viewPure (GameState (MkMaze walls _ _ _ _ _) _) = Pictures $ map drawTile walls

--drawWall :: Wall -> Picture
--drawWall (MkLine (MkPosition x1 y1) (MkPosition x2 y2))  = Color white $ Line [(x1, y1), (x2, y2)]

--drawCircle :: Circle -> Picture
--drawCircle (MkPosition x y) = Color white $ Translate x y $ ThickCircle 3 6

drawTile :: Tile -> Picture
drawTile (Empty row column) = undefined
drawTile (Wall row column) = Color white $ polygon (tiletoPath (Wall row column))
drawTile (Model.Circle row column) = Color yellow $ polygon (tiletoPath (Wall row column))

tiletoPath :: Tile -> Path
tiletoPath (Empty x y) = let p1 = ((-30 + 30 * x) - 420, (15 - 30 * y) + 480)
                             p2 = ((-30 + 30 * x) - 420, (-15 - 30 * y) + 480)
                             p3 = ((0 + 30 * x) - 420, (15 - 30 * y) + 480)
                             p4 = ((0 + 30 * x) - 420, (-15 - 30 * y) + 480)
                         in [p1, p2, p3, p4]
tiletoPath (Wall x y) = let p1 = ((-30 + 30 * x) - 420, (15 - 30 * y) + 480)
                            p2 = ((-30 + 30 * x) - 420, (-15 - 30 * y) + 480)
                            p3 = ((0 + 30 * x) - 420, (15 - 30 * y) + 480)
                            p4 = ((0 + 30 * x) - 420, (-15 - 30 * y) + 480)
                         in [p1, p2, p3, p4]
tiletoPath (Model.Circle x y) = let p1 = ((-30 + 30 * x) - 420, (15 - 30 * y) + 480)
                                    p2 = ((-30 + 30 * x) - 420, (-15 - 30 * y) + 480)
                                    p3 = ((0 + 30 * x) - 420, (15 - 30 * y) + 480)
                                    p4 = ((0 + 30 * x) - 420, (-15 - 30 * y) + 480)
                                in [p1, p2, p3, p4]



drawPacMan :: PacMan -> IO Picture
drawPacMan (MkPacMan (MkPosition x y) _ Closed) = do
                                        pacManImage <- loadBMP "src\\Images\\PacmanClosed.bmp"
                                        return $ Translate x y pacManImage
drawPacMan (MkPacMan (MkPosition x y) Model.Up Open) = do
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenUp.bmp"
                                                            return $ Translate x y pacManImage
drawPacMan (MkPacMan (MkPosition x y) Model.Down Open) = do
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenDown.bmp"
                                                            return $ Translate x y pacManImage
drawPacMan (MkPacMan (MkPosition x y) Model.Left Open) = do
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenLeft.bmp"
                                                            return $ Translate x y pacManImage
drawPacMan (MkPacMan (MkPosition x y) Model.Right Open) = do
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenRight.bmp"
                                                            return $ Translate x y pacManImage

--drawEnemy :: Enemy -> Picture
--drawEnemy (MkEnemy color (MkPosition x y) Up) = Color color $ Translate x y $ 
--drawEnemy (MkEnemy color (MkPosition x y) Down) = Color color $ Translate x y $ 
--drawEnemy (MkEnemy color (MkPosition x y) Left) = Color color $ Translate x y $ 
--drawEnemy (MkEnemy color (MkPosition x y) Right) = Color color $ Translate x y $ 

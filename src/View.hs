-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Maybe (mapMaybe)
import Text.Read (Lexeme(String))

view :: GameState -> IO Picture
view gameState@(GameState maze@(MkMaze {}) Ended) = do
                                                        gameScreen <- view (GameState maze Running)
                                                        pauseScreen <- loadBMP "src\\Images\\StartScreen.bmp"
                                                        return $ Pictures [gameScreen, pauseScreen]
view gameState@(GameState maze@(MkMaze {}) Paused) = do
                                                        gameScreen <- view (GameState maze Running)
                                                        pauseScreen <- loadBMP "src\\Images\\PauseScreen.bmp"
                                                        return $ Pictures [gameScreen, pauseScreen]
view gameState@(GameState (MkMaze _ pm pink blue orange red) _) = do
                                                            pacman <- drawPacMan pm
                                                            p <- drawEnemy pink
                                                            b <- drawEnemy blue
                                                            o <- drawEnemy orange
                                                            r <- drawEnemy red
                                                            return $ Pictures [pacman, p, viewPure gameState]

viewPure :: GameState -> Picture
viewPure (GameState (MkMaze walls _ _ _ _ _) _) = Pictures $ mapMaybe drawTile walls

drawTile :: Tile -> Maybe Picture
drawTile (Empty row column) = Prelude.Nothing
drawTile wall@(Wall row column) = Just $ Color white $ polygon (tiletoPath wall)
drawTile circle@(Model.Circle row column) = let
                                                coordinates = head (tiletoPath circle)
                                            in
                                                Just $ Color white $ uncurry Translate coordinates $ ThickCircle 3 6

tiletoPath :: Tile -> Path
tiletoPath (Wall x y) = let p1 = ((-30 + 30 * x) - 420, (15 - 30 * y) + 480)
                            p2 = ((-30 + 30 * x) - 420, (-15 - 30 * y) + 480)
                            p3 = (30 * x - 420, (-15 - 30 * y) + 480)
                            p4 = (30 * x - 420, (15 - 30 * y) + 480)
                         in [p1, p2, p3, p4]
tiletoPath (Model.Circle x y) = [((-15 + 30 * x) - 420, (-30) * y + 480)]



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

drawEnemy :: Enemy -> IO Picture
drawEnemy e@(MkEnemy _ (MkPosition x y) _) = do
                                                            image <- loadBMP $ "src\\Images\\" ++ enemyImageName e
                                                            return $ Translate x y image

enemyImageName :: Enemy -> String
enemyImageName (MkEnemy col _ dir) = show col ++ enemyImageName' dir
    where 
        enemyImageName' :: Direction -> String
        enemyImageName' Model.Up = "Up.bmp"
        enemyImageName' Model.Left = "Left.bmp"
        enemyImageName' Model.Right = "Right.bmp"
        enemyImageName' Model.Down = "Down.bmp"

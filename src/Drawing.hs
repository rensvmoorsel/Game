module Drawing where

import Graphics.Gloss
import Model
import Data.Maybe (mapMaybe)

class Drawable a where
    draw :: a -> Picture

class IODrawable a where
    drawBMP :: a -> IO Picture

instance Drawable Tile where
    draw (Empty _ _) = polygon []
    draw wall@(Wall row column) = Color white $ polygon (tiletoPath wall)
    draw circle@(Model.Circle row column) = let
                                                coordinates = head (tiletoPath circle)
                                            in
                                                Color white $ uncurry Translate coordinates $ ThickCircle 3 6
                                                    

instance IODrawable PacMan where
    drawBMP (MkPacMan (MkPosition x y) _ Closed) = do
                                        pacManImage <- loadBMP "src\\Images\\PacmanClosed.bmp"
                                        return $ Translate (x - 405) (-y + 450) pacManImage
    drawBMP (MkPacMan (MkPosition x y) Model.Up Open) = do
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenUp.bmp"
                                                            return $ Translate (x - 405) (-y + 450) pacManImage
    drawBMP (MkPacMan (MkPosition x y) Model.Down Open) = do
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenDown.bmp"
                                                            return $ Translate (x - 405) (-y + 450) pacManImage
    drawBMP (MkPacMan (MkPosition x y) Model.Left Open) = do
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenLeft.bmp"
                                                            return $ Translate (x - 405) (-y + 450) pacManImage
    drawBMP (MkPacMan (MkPosition x y) Model.Right Open) = do
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenRight.bmp"
                                                            return $ Translate (x - 405) (-y + 450) pacManImage

instance IODrawable Enemy where
    drawBMP e@(MkEnemy _ (MkPosition x y) _) = do
                                                    image <- loadBMP $ "src\\Images\\" ++ enemyImageName e
                                                    return $ Translate (x - 405) (-y + 450) image
                                                    where
                                                            enemyImageName' :: Direction -> String
                                                            enemyImageName' Model.Up = "Up.bmp"
                                                            enemyImageName' Model.Left = "Left.bmp"
                                                            enemyImageName' Model.Right = "Right.bmp"
                                                            enemyImageName' Model.Down = "Down.bmp"
                                                            enemyImageName :: Enemy -> String
                                                            enemyImageName (MkEnemy col _ dir) = show col ++ enemyImageName' dir                
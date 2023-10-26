module Drawing where

import Graphics.Gloss
import Model
import Data.Maybe (mapMaybe)
import Grid

class Drawable a where --for drawable objects
    draw :: a -> Picture

class IODrawable a where --for drawable objects that use IO
    drawBMP :: a -> IO Picture

instance Drawable Tile where 
    draw (Empty _) = polygon [] --draw nothing if it is an empty tile
    draw wall@(Wall _) = Color white $ polygon (tiletoPath wall) --draw a polygon with the 4 corners of the tile
    draw circle@(Grid.Circle _) = let --draw a circle with the center of the tile
                                                coordinates = head (tiletoPath circle)
                                            in
                                                Color white $ uncurry Translate coordinates $ ThickCircle 3 6
                                                    

instance IODrawable PacMan where
    drawBMP (MkPacMan (MkPosition x y) _ Closed) = do -- load the closed mouth pacman bmp and convert the image to the left upper corner + position of pacman
                                        pacManImage <- loadBMP "src\\Images\\PacmanClosed.bmp"
                                        return $ Translate (x - 405) (-y + 450) pacManImage
    drawBMP (MkPacMan (MkPosition x y) Up Open) = do-- load the up looking pacman bmp and convert the image to the left upper corner + position of pacman
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenUp.bmp"
                                                            return $ Translate (x - 405) (-y + 450) pacManImage
    drawBMP (MkPacMan (MkPosition x y) Down Open) = do-- load the down looking pacman bmp and convert the image to the left upper corner + position of pacman
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenDown.bmp"
                                                            return $ Translate (x - 405) (-y + 450) pacManImage
    drawBMP (MkPacMan (MkPosition x y) Grid.Left Open) = do-- load the left looking pacman bmp and convert the image to the left upper corner + position of pacman
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenLeft.bmp"
                                                            return $ Translate (x - 405) (-y + 450) pacManImage
    drawBMP (MkPacMan (MkPosition x y) Grid.Right Open) = do-- load the right looking pacman bmp and convert the image to the left upper corner + position of pacman
                                                            pacManImage <- loadBMP "src\\Images\\PacmanOpenRight.bmp"
                                                            return $ Translate (x - 405) (-y + 450) pacManImage

instance IODrawable Enemy where
    drawBMP e@(MkEnemy _ (MkPosition x y) _) = do
                                                    image <- loadBMP $ "src\\Images\\" ++ enemyImageName e --determine the full path
                                                    return $ Translate (x - 405) (-y + 450) image
                                                    where
                                                            enemyImageName' :: Direction -> String --determine the end of the file name
                                                            enemyImageName' Up = "Up.bmp"
                                                            enemyImageName' Grid.Left = "Left.bmp"
                                                            enemyImageName' Grid.Right = "Right.bmp"
                                                            enemyImageName' Down = "Down.bmp"
                                                            enemyImageName :: Enemy -> String --determine the full file name
                                                            enemyImageName (MkEnemy col _ dir) = show col ++ enemyImageName' dir                
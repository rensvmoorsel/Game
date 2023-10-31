module Drawing where

import Graphics.Gloss
import Model
import Data.Maybe (mapMaybe)
import Grid

class Drawable a where --for drawable objects
    draw :: a -> Picture

instance Drawable Tile where 
    draw (Empty _) = polygon [] --draw nothing if it is an empty tile
    draw wall@(Wall _) = Color white $ polygon (tiletoPath wall) --draw a polygon with the 4 corners of the tile
    draw circle@(Grid.Circle _) = let --draw a circle with the center of the tile
                                                coordinates = head (tiletoPath circle)
                                            in
                                                Color white $ uncurry Translate coordinates $ ThickCircle 3 6
                                                    

instance Drawable PacMan where
    draw (MkPacMan (MkPosition x y) _ Closed image _ _ _ _) = moveSprite (x, y) image
    draw (MkPacMan (MkPosition x y) Grid.Left Open _ image _ _ _) = moveSprite (x, y) image
    draw (MkPacMan (MkPosition x y) Grid.Right Open _ _ image _ _) = moveSprite (x, y) image
    draw (MkPacMan (MkPosition x y) Up Open _ _ _ image _) = moveSprite (x, y) image
    draw (MkPacMan (MkPosition x y) Down Open _ _ _ _ image) = moveSprite (x, y) image

instance Drawable Enemy where
    draw e@(MkEnemy {enemyposition = MkPosition {x = x, y = y}, enemydirection = Grid.Left, enemyspriteLeft = image}) = moveSprite (x, y) image               
    draw e@(MkEnemy {enemyposition = MkPosition {x = x, y = y}, enemydirection = Grid.Right, enemyspriteRight = image}) = moveSprite (x, y) image               
    draw e@(MkEnemy  {enemyposition = MkPosition {x = x, y = y}, enemydirection = Up, enemyspriteUp = image}) = moveSprite (x, y) image               
    draw e@(MkEnemy  {enemyposition = MkPosition {x = x, y = y}, enemydirection = Down, enemyspriteDown = image}) = moveSprite (x, y) image  

instance Drawable Maze where
    draw (MkMaze g pm p b o r) = Pictures $ draw pm:draw p:draw b:draw o:draw r:map draw g 

instance Drawable GameState where
    draw GameState {status = Running, maze = maze} = draw maze
    draw GameState {status = Paused, maze = maze} = Color red $ Pictures [draw maze, scale 0.2 0.2 $ translate (-100) 0 $ text "Paused "]   
    draw GameState {unlockedLevels = levels} = Color white $ scale 0.2 0.2 $ Pictures [translate (-2000) 0 $ text $ "You have unlocked levels: " ++ foldr (\level oldString -> show level ++ " " ++ oldString) "" levels,
                                                                                                                        translate (-2000) (-200) $ text "Hit the number keys to select level" ] 

moveSprite :: (Float, Float) -> Picture -> Picture
moveSprite (x, y) = Translate (x - 405) (-y + 450)
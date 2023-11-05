module TileModule(Tile) where
import Graphics.Gloss
import Update
import Drawing
import Grid
import Model

instance Updatable Tile where
    updateObject circle@(Grid.Circle coordinate) _ gstate
                                        | abs (xPm - x pos) < 24 && abs (yPm - y pos) < 24 = Empty coordinate --the distance is close enough to remove the circle
                                        | otherwise = circle --the distance is to big, so don't remove the circle
                                            where
                                                positionPm = position (pacman (maze gstate)) --get the position of the pacman
                                                xPm = x positionPm
                                                yPm = y positionPm
                                                pos = realPosition circle    --calculate the realposition of the circle
    updateObject x _ _ = x --it isn't a circle so it's a static object, keep it as it is

instance Drawable Tile where 
    draw (Empty _) = polygon [] --draw nothing if it is an empty tile
    draw wall@(Wall _) = Color white $ polygon (tiletoPath wall) --draw a polygon with the 4 corners of the tile
    draw circle@(Grid.Circle _) = let --draw a circle with the center of the tile
                                    coordinates = head (tiletoPath circle)
                                  in
                                    Color white $ uncurry Translate coordinates $ ThickCircle 3 6
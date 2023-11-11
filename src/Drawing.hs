module Drawing where

import Graphics.Gloss
import Model
import Data.Maybe (mapMaybe)
import Grid

class Drawable a where --for drawable objects
    draw :: a -> Picture  

moveSprite :: (Float, Float) -> Picture -> Picture --move a sprite a certain amount of pixels
moveSprite (x, y) = Translate (x - 407) (-y + 450)
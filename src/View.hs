-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Maybe (mapMaybe)
import Drawing
import LevelLoading
import GameStateModule

view :: GameState -> IO Picture --draw the impure parts
view gstate = return $ viewPure gstate

viewPure :: GameState -> Picture --draw the pure parts of the game (the grid)
viewPure = draw

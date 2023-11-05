{-# OPTIONS_GHC -Wno-missing-fields #-}
module Update where

import Model
import Grid

class Updatable a where
    updateObject :: a -> Float -> GameState -> a

updatePosition :: Float -> Float -> Position -> Direction -> Position --update a position based on the speed, deltaTime and direction
updatePosition speed dt position@MkPosition{x = x, y = y} Up = position {y = y - dt * speed, x = roundTo30 x } 
updatePosition speed dt position@MkPosition{x = x, y = y} Down = position {y = y + dt * speed, x = roundTo30 x }
updatePosition speed dt position@MkPosition{x = x, y = y} Grid.Right = position {x = x + dt * speed, y = roundTo30 y  }
updatePosition speed dt position@MkPosition{x = x, y = y} Grid.Left = position {x = x - dt * speed, y = roundTo30 y }
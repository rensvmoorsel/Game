-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Update

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as G
import System.Random
import Data.Char (isDigit, ord)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState --IO WANT RANDOM GEDEELTE KOMT ERIN
step secs gstate | status gstate == Ended && pressedKey gstate == L1 || pressedKey gstate == L2 = loadLevel $ keyToInt $ pressedKey gstate
                 | otherwise = return $ updateObject gstate secs gstate

keyToInt :: Model.Key -> Int --convert a level key to the int of the level
keyToInt L1 = 1
keyToInt L2 = 2

-- | Handle user input
input :: Event -> GameState -> IO GameState --set the input
input e gstate = return $ inputKey e gstate


inputKey :: Event -> GameState -> GameState --check which button is pressed
inputKey (EventKey (SpecialKey KeyEsc) G.Down _ _) gstate = gstate { pressedKey = Esc }
inputKey (EventKey (Char 'w') G.Down _ _) gstate = gstate { pressedKey = W }
inputKey (EventKey (Char 'a') G.Down _ _) gstate = gstate { pressedKey = A }
inputKey (EventKey (Char 's') G.Down _ _) gstate = gstate { pressedKey = S }
inputKey (EventKey (Char 'd') G.Down _ _) gstate = gstate { pressedKey = D }
inputKey (EventKey (Char '1') G.Down _ _) gstate = gstate { pressedKey = L1 }
inputKey (EventKey (Char '2') G.Down _ _) gstate = gstate { pressedKey = L2 }
inputKey _ gstate = gstate { pressedKey = None }
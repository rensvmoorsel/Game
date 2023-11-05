-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Update

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as G
import System.Random
import Data.Char (isDigit, ord)
import System.Directory
import LevelLoading
import Data.List
import GameStateModule

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | status gstate == Complete && (lastLevel gstate + 1) `notElem` unlockedLevels gstate && elem (lastLevel gstate + 1) (levels gstate) = do --add level to the completed levels file if not already done
                                                        let unlockedLevel = lastLevel gstate + 1
                                                        appendFile "src\\levels\\UnlockedLevels.txt" $ ' ':show unlockedLevel
                                                        return $ (updateObject gstate secs gstate) {unlockedLevels = sort $ unlockedLevel : unlockedLevels gstate}
                  | (status gstate == Failed || status gstate == Complete) --if game isn't running and a number is pressed and the level is unlocked: load the level
                        && key /= None
                        && key /= Esc
                        && keyToInt key `elem` unlockedLevels gstate =
                            loadLevel gstate $ keyToInt key
                  | otherwise = return $ updateObject gstate secs gstate --otherwise: the game is running/paused, update the game state
                    where key = pressedKey gstate

keyToInt :: Model.Key -> Int --convert a level key to the int of the level
keyToInt L1 = 1
keyToInt L2 = 2
keyToInt L3 = 3
keyToInt L4 = 4
keyToInt L5 = 5
keyToInt L6 = 6
keyToInt L7 = 7
keyToInt L8 = 8
keyToInt L9 = 9
keyToInt _ = -1

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
inputKey (EventKey (Char '3') G.Down _ _) gstate = gstate { pressedKey = L3 }
inputKey (EventKey (Char '4') G.Down _ _) gstate = gstate { pressedKey = L4 }
inputKey (EventKey (Char '5') G.Down _ _) gstate = gstate { pressedKey = L5 }
inputKey (EventKey (Char '6') G.Down _ _) gstate = gstate { pressedKey = L6 }
inputKey (EventKey (Char '7') G.Down _ _) gstate = gstate { pressedKey = L7 }
inputKey (EventKey (Char '8') G.Down _ _) gstate = gstate { pressedKey = L8 }
inputKey (EventKey (Char '9') G.Down _ _) gstate = gstate { pressedKey = L9 }
inputKey _ gstate = gstate { pressedKey = None }
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

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState --IO WANT RANDOM GEDEELTE KOMT ERIN
step secs gstate | (status gstate == Failed || status gstate == Complete)
                        && (pressedKey gstate == L1 || pressedKey gstate == L2) = do
                                                                                    levels <- unlockedLevels
                                                                                    let levelInt = keyToInt (pressedKey gstate)
                                                                                    if levelInt `elem` levels then loadLevel levelInt else return $ updateObject gstate secs gstate
                 | status gstate == Complete = do --add level to the completed levels file if not already done
                                                levels <- unlockedLevels
                                                let nextLevel = lastLevel gstate + 1 --find next level
                                                levelExists <- isLevel nextLevel
                                                if nextLevel `notElem` levels && levelExists then --if level exists and not unlocked yet: unlock it
                                                    do
                                                        appendFile "src\\levels\\UnlockedLevels.txt" $ ' ':show nextLevel 
                                                        return $ updateObject gstate secs gstate
                                                else return $ updateObject gstate secs gstate
                 | otherwise = return $ updateObject gstate secs gstate

keyToInt :: Model.Key -> Int --convert a level key to the int of the level
keyToInt L1 = 1
keyToInt L2 = 2

isLevel :: Int -> IO Bool --check if level exists
isLevel level = doesFileExist $ "src\\levels\\" ++ show level ++ ".txt"

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
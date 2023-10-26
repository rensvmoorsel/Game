module LevelLoading where

import Model
import Grid
import System.Random
import Data.Maybe
import Data.List

loadLevel :: Int -> IO GameState --read a level file
loadLevel level = do
                    levelContent <- readFile $ "src\\levels\\"++ show level ++ ".txt" --read the file
                    let levelLines = lines levelContent --split the lines
                    loadedGrid <- loadLines levelLines 1
                    return GameState { --create the gamestate
                        maze = MkMaze
                        {
                            grid = loadedGrid,
                            pacman = MkPacMan
                            {
                                position = pacmanPosition levelLines ,
                                direction = Up,
                                mouthStatus = Open
                            },
                            redEnemy = MkEnemy Red (redPosition levelLines) Up,
                            pinkEnemy = MkEnemy Pink (pinkPosition levelLines) Up,
                            orangeEnemy = MkEnemy Orange (orangePosition levelLines) Up,
                            blueEnemy = MkEnemy Blue (bluePosition levelLines) Up
                        },
                        status = Running,
                        pressedKey = None,
                        elapsedTime = 0,
                        previousKey = None,
                        lastLevel = level
                    }
                    where
                        loadLine :: String -> Coordinate -> IO Grid --convert a line into a list of tiles
                        loadLine [] _ = return []
                        loadLine (item:items) coordinate  | item == 'W' = do
                                                                      nextItems <- loadLine items $ MkCoordinate (xCoord coordinate + 1) (yCoord coordinate)
                                                                      return $ Wall coordinate:nextItems
                                                   | otherwise = do --If empty block, give 50% change to have a circle
                                                                    random <- randomRange (0, 10) --get random number between 0 and 10
                                                                    nextItems <- loadLine items $ MkCoordinate (xCoord coordinate + 1) (yCoord coordinate) --load the next items in the row
                                                                    if random < 6 && item == ' ' --if the number is lower than 6 and the block is empty: place circle, otherwise place empty
                                                                    then return $ Grid.Circle coordinate:nextItems
                                                                    else return $ Empty coordinate:nextItems                                                                    

                        loadLines :: [String] -> Int -> IO Grid -- load all lines
                        loadLines [] _ = return []
                        loadLines (line:lines) y = do
                                                      loadedLine <- loadLine line $ MkCoordinate 1 y
                                                      nextLines <- loadLines lines (y + 1)
                                                      return $ loadedLine ++ nextLines

                        orangePosition :: [String] -> Position --find the position of orange, starting by the first row
                        orangePosition a = orangePosition' a 1

                        redPosition :: [String] -> Position --find the position of red, starting by the first row
                        redPosition a = redPosition' a 1

                        bluePosition :: [String] -> Position --find the position of blue, starting by the first row
                        bluePosition a = bluePosition' a 1

                        pinkPosition :: [String] -> Position --find the position of pink, starting by the first row
                        pinkPosition a = pinkPosition' a 1

                        pacmanPosition :: [String] -> Position --find the position of pacman, starting by the first row
                        pacmanPosition a = pacmanPosition' a 1

                        orangePosition' :: [String] -> Int -> Position --check each row if it has the correct char, determine the location with the row it is in and the index in the row
                        orangePosition' (x:xs) y | 'o' `elem` x = MkPosition (fromIntegral (fromMaybe 0 (elemIndex 'o' x)) * 30) (fromIntegral y * 30 - 30)
                                                | otherwise = orangePosition' xs (y + 1)

                        redPosition' :: [String] -> Int -> Position--check each row if it has the correct char, determine the location with the row it is in and the index in the row
                        redPosition' (x:xs) y | 'r' `elem` x = MkPosition (fromIntegral (fromMaybe 0 (elemIndex 'r' x)) * 30) (fromIntegral y * 30 - 30)
                                                | otherwise = redPosition' xs (y + 1)

                        bluePosition' :: [String] -> Int -> Position--check each row if it has the correct char, determine the location with the row it is in and the index in the row
                        bluePosition' (x:xs) y | 'b' `elem` x = MkPosition (fromIntegral (fromMaybe 0 (elemIndex 'b' x)) * 30) (fromIntegral y * 30 - 30)
                                                | otherwise = bluePosition' xs (y + 1)

                        pinkPosition' :: [String] -> Int -> Position--check each row if it has the correct char, determine the location with the row it is in and the index in the row
                        pinkPosition' (x:xs) y | 'p' `elem` x = MkPosition (fromIntegral (fromMaybe 0 (elemIndex 'p' x)) * 30) (fromIntegral y * 30 - 30)
                                                | otherwise = pinkPosition' xs (y + 1)

                        pacmanPosition' :: [String] -> Int -> Position--check each row if it has the correct char, determine the location with the row it is in and the index in the row
                        pacmanPosition' (x:xs) y | 'P' `elem` x = MkPosition (fromIntegral (fromMaybe 0 (elemIndex 'P' x)) * 30) (fromIntegral y * 30 - 30)
                                                | otherwise = pacmanPosition' xs (y + 1)

randomRange :: (Int, Int) -> IO Int --generate random int
randomRange (x, y) = getStdRandom $ randomR (x, y)

unlockedLevels :: IO [Int]
unlockedLevels = do
                    content <- readFile "src\\levels\\UnlockedLevels.txt"
                    return $ map read $ words content

                    
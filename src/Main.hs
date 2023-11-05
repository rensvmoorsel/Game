module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import System.Directory
import Control.Monad
import Grid
import Data.Maybe
import Data.List

main :: IO ()
main = do --load all IO and inject it into the gamestate, then start the game
            pacmanClosed <- loadBMP "src\\images\\PacmanClosed.bmp"
            pacmanLeft <- loadBMP "src\\images\\PacmanOpenLeft.bmp"
            pacmanRight <- loadBMP "src\\images\\PacmanOpenRight.bmp"
            pacmanUp <- loadBMP "src\\images\\PacmanOpenUp.bmp"
            pacmanDown <- loadBMP "src\\images\\PacmanOpenDown.bmp"
            pinkLeft <- loadBMP "src\\images\\PinkLeft.bmp"
            pinkRight <- loadBMP "src\\images\\PinkRight.bmp"
            pinkUp <- loadBMP "src\\images\\PinkUp.bmp"
            pinkDown <- loadBMP "src\\images\\PinkDown.bmp"
            orangeLeft <- loadBMP "src\\images\\OrangeLeft.bmp"
            orangeRight <- loadBMP "src\\images\\OrangeRight.bmp"
            orangeUp <- loadBMP "src\\images\\OrangeUp.bmp"
            orangeDown <- loadBMP "src\\images\\OrangeDown.bmp"
            blueLeft <- loadBMP "src\\images\\BlueLeft.bmp"
            blueRight <- loadBMP "src\\images\\BlueRight.bmp"
            blueUp <- loadBMP "src\\images\\BlueUp.bmp"
            blueDown <- loadBMP "src\\images\\BlueDown.bmp"
            redLeft <- loadBMP "src\\images\\RedLeft.bmp"
            redRight <- loadBMP "src\\images\\RedRight.bmp"
            redUp <- loadBMP "src\\images\\RedUp.bmp"
            redDown <- loadBMP "src\\images\\RedDown.bmp"
            unlockedlevels <- loadUnlockedLevels
            lev <- loadLevels
            let newInitialState = initialState {maze = (maze initialState) {pacman = (pacman (maze initialState)) {spriteclosed = pacmanClosed, spriteleft = pacmanLeft, spriteright = pacmanRight, spriteup = pacmanUp, spritedown = pacmanDown}
                                                        , pinkEnemy = (pinkEnemy (maze initialState)) {enemyspriteLeft = pinkLeft, enemyspriteRight = pinkRight, enemyspriteDown = pinkDown, enemyspriteUp = pinkUp}
                                                        , blueEnemy = (blueEnemy (maze initialState)) {enemyspriteLeft = blueLeft, enemyspriteRight = blueRight, enemyspriteDown = blueDown, enemyspriteUp = blueUp}
                                                        , orangeEnemy = (orangeEnemy (maze initialState)) {enemyspriteLeft = orangeLeft, enemyspriteRight = orangeRight, enemyspriteDown = orangeDown, enemyspriteUp = orangeUp}
                                                        , redEnemy = (redEnemy (maze initialState)) {enemyspriteLeft = redLeft, enemyspriteRight = redRight, enemyspriteDown = redDown, enemyspriteUp = redUp}
                                                    }
                                                    , unlockedLevels = unlockedlevels
                                                    , levels = lev
                                                }
            levcontents <- loadMazes 1 newInitialState
            playIO (InWindow "Pacman" (840, 930) (0, 0)) -- Or FullScreen
                black            -- Background color
                10               -- Frames per second
                newInitialState {levelcontents = levcontents}     -- Initial state
                view             -- View function
                input            -- Event function
                step             -- Step function

loadUnlockedLevels :: IO [Int] --read the unlockedLevels file
loadUnlockedLevels = do
                    content <- readFile "src\\levels\\UnlockedLevels.txt"
                    return $ map read $ words content

loadLevels :: IO [Int] --read the level files
loadLevels = filterM (\levelNumber -> doesFileExist $ "src\\levels\\" ++ show levelNumber ++ ".txt") [1, 2, 3, 4, 5, 6, 7, 8, 9]

loadMazes :: Int -> GameState -> IO [Maze] --add the mazes into the levels of the gamestate
loadMazes level gstate = do
                            levelExists <- doesFileExist ("src\\levels\\" ++ show level ++ ".txt")
                            if levelExists
                            then
                                do
                                    content <- readFile $ "src\\levels\\" ++ show level ++ ".txt"
                                    nextLevels <- loadMazes (level + 1) gstate
                                    return $ stringToMaze content gstate:nextLevels
                            else 
                                return []

stringToMaze :: String -> GameState -> Maze --convert maze string into a maze
stringToMaze levelContent gstate =
                                let 
                                    levelLines = lines levelContent --split the lines
                                    loadedGrid = loadLines levelLines 1
                                    pm = pacman $ maze gstate
                                    r = redEnemy $ maze gstate
                                    o = orangeEnemy $ maze gstate
                                    b = blueEnemy $ maze gstate
                                    p = pinkEnemy $ maze gstate
                                in
                                    MkMaze
                                        {
                                            grid = loadedGrid,
                                            pacman = MkPacMan
                                            {
                                                position = pacmanPosition levelLines ,
                                                direction = Grid.Up,
                                                mouthStatus = Open,
                                                spriteclosed = spriteclosed pm,
                                                spritedown = spritedown pm,
                                                spriteup = spriteup pm,
                                                spriteleft = spriteleft pm,
                                                spriteright = spriteright pm
                                            },
                                            redEnemy = MkEnemy Red (redPosition levelLines) Grid.Up (enemyspriteLeft r) (enemyspriteRight r) (enemyspriteUp r) (enemyspriteDown r),
                                            pinkEnemy = MkEnemy Pink (pinkPosition levelLines) Grid.Up (enemyspriteLeft p) (enemyspriteRight p) (enemyspriteUp p) (enemyspriteDown p),
                                            orangeEnemy = MkEnemy Orange (orangePosition levelLines) Grid.Up (enemyspriteLeft o) (enemyspriteRight o) (enemyspriteUp o) (enemyspriteDown o),
                                            blueEnemy = MkEnemy Blue (bluePosition levelLines) Grid.Up (enemyspriteLeft b) (enemyspriteRight b) (enemyspriteUp b) (enemyspriteDown b)
                                        }
                                where
                                    loadLine :: String -> Coordinate -> Grid --convert a line into a list of tiles
                                    loadLine [] _ = []
                                    loadLine (item:items) coordinate  | item == 'W' =
                                                                                        let 
                                                                                            nextItems = loadLine items $ MkCoordinate (xCoord coordinate + 1) (yCoord coordinate)
                                                                                        in
                                                                                            Wall coordinate:nextItems
                                                                      | otherwise =   
                                                                            let 
                                                                                nextItems = loadLine items $ MkCoordinate (xCoord coordinate + 1) (yCoord coordinate)
                                                                            in
                                                                                Empty coordinate:nextItems                                                                    

                                    loadLines :: [String] -> Int -> Grid -- load all lines
                                    loadLines [] _ = []
                                    loadLines (line:lines) y = 
                                                                let 
                                                                    loadedLine = loadLine line $ MkCoordinate 1 y
                                                                    nextLines = loadLines lines (y + 1)
                                                                in
                                                                    loadedLine ++ nextLines

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
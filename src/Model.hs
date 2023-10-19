-- | This module contains the data types
--   which represent the state of the game
module Model where
import Graphics.Gloss

data Maze = MkMaze [Wall] [Circle] PacMan Enemy Enemy Enemy Enemy
type Width = Float
type Height = Float
type Wall = Line

data PacMan = MkPacMan Position Direction MouthStatus
data MouthStatus = Open | Closed
data Enemy = MkEnemy Color Position Direction

data Position = MkPosition Float Float
data Line = MkLine Position Position
type Circle = Position

data StatusGame = Running | Paused | Ended
data Direction = Left | Right | Up | Down
data Key = W | A | S | D | Esc
data Input = Key | Nothing

data GameState = GameState {
                   maze  :: Maze
                 , status :: StatusGame
                 }

initialState :: GameState
initialState = let listofwalls = [MkLine (MkPosition 100 490) (MkPosition 150 490), MkLine (MkPosition 400 300) (MkPosition 400 100)]
                   listofcircles = [MkPosition 100 2, MkPosition 8 2]
                   pacman = MkPacMan (MkPosition 100 200) Up Closed
                   p = MkEnemy (makeColor 255 192 203 255) (MkPosition 49 252) Up
                   b = MkEnemy blue (MkPosition 643 45) Up
                   o = MkEnemy orange (MkPosition 64 32) Up
                   r = MkEnemy blue (MkPosition 563 257) Up
               in GameState (MkMaze listofwalls listofcircles pacman p b o r) Running

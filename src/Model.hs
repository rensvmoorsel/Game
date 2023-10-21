-- | This module contains the data types
--   which represent the state of the game
module Model where
import Graphics.Gloss

data Maze = MkMaze [Tile] PacMan Enemy Enemy Enemy Enemy
type Width = Float
type Height = Float

data Tile = Empty Float Float | Wall Float Float | Circle Float Float 

instance Eq Tile where
  (Wall x y) == (Empty a b) = a == x && b == y
  (Model.Circle x y) == (Empty a b) = a == x && b == y

data PacMan = MkPacMan Position Direction MouthStatus
data MouthStatus = Open | Closed
data Enemy = MkEnemy Color Position Direction

data Position = MkPosition Float Float
data Line = MkLine Position Position

data StatusGame = Running | Paused | Ended
data Direction = Left | Right | Up | Down
data Key = W | A | S | D | Esc
data Input = Key | Nothing

data GameState = GameState {
                   maze  :: Maze
                 , status :: StatusGame
                 }

createGrid :: [Tile] -> [Tile] -- fills empty spots of grid with empty tiles
createGrid list = [ Empty x y | x <- [1 .. 28], y <- [1 .. 31], notElem (Empty x y) list] ++ list

initialState :: GameState
initialState = let grid = createGrid [Wall 1 1, Wall 4 2, Wall 14 20]
                   pacman = MkPacMan (MkPosition 100 200) Up Open
                   p = MkEnemy (makeColor 255 192 203 255) (MkPosition 49 252) Up
                   b = MkEnemy blue (MkPosition 643 45) Up
                   o = MkEnemy orange (MkPosition 64 32) Up
                   r = MkEnemy blue (MkPosition 563 257) Up
               in GameState (MkMaze grid pacman p b o r) Running

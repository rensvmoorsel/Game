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
  (Empty a b) == (Wall x y) = a == x && b == y
  (Empty a b) == (Model.Circle x y) = a == x && b == y

data PacMan = MkPacMan Position Direction MouthStatus
data MouthStatus = Open | Closed
data Enemy = MkEnemy EnemyColor Position Direction
data EnemyColor = Red | Orange | Pink | Blue

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
createGrid list = [ Empty x y | x <- [1 .. 28], y <- [1 .. 31], Empty x y `notElem` list] ++ list

initialState :: GameState
initialState = let grid = createGrid [Wall 1 1, Wall 4 2, Wall 14 20, Model.Circle 1 2]
                   pacman = MkPacMan (MkPosition 10 200) Down Open
                   p = MkEnemy Pink (MkPosition 49 252) Up
                   b = MkEnemy Blue (MkPosition 643 45) Up
                   o = MkEnemy Orange (MkPosition 64 32) Up
                   r = MkEnemy Red (MkPosition 563 257) Up
               in GameState (MkMaze grid pacman p b o r) Running

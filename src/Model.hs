-- | This module contains the data types
--   which represent the state of the game
module Model where
import Graphics.Gloss
import Data.List (elemIndex)
import Data.Maybe
import Data.Char (ord)
import Grid

--GameState objects
data Maze = MkMaze {
  grid :: Grid,
  pacman :: PacMan,
  pinkEnemy :: Enemy,
  blueEnemy :: Enemy,
  orangeEnemy :: Enemy,
  redEnemy :: Enemy
 }
type Width = Float
type Height = Float
data PacMan = MkPacMan {
                        position :: Position
                        , direction :: Direction
                        , mouthStatus :: MouthStatus
                        , spriteclosed :: Picture
                        , spriteleft :: Picture
                        , spriteright :: Picture
                        , spriteup :: Picture
                        , spritedown :: Picture
                        }
data MouthStatus = Open | Closed
data Enemy = MkEnemy {
                        enemycolor :: EnemyColor
                        , enemyposition :: Position
                        , enemydirection :: Direction
                        , enemyspriteLeft :: Picture
                        , enemyspriteRight :: Picture
                        , enemyspriteUp :: Picture
                        , enemyspriteDown :: Picture
                      }
data EnemyColor = Red | Orange | Pink | Blue
data Line = MkLine Position Position
data StatusGame = Running | Paused | Complete | Failed
data Key = W | A | S | D | Esc | None | L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8 | L9

--Instances for the datatypes
instance Show EnemyColor where
  show Red = "Red"
  show Orange = "Orange"
  show Pink = "Pink"
  show Blue = "Blue"

instance Eq StatusGame where
  Running == Running = True
  Paused == Paused = True
  Complete == Complete = True
  Failed == Failed = True
  _ == _ = False

instance Eq Key where
  W == W = True
  A == A = True
  S == S = True
  D == D = True
  Esc == Esc = True
  None == None = True
  L1 == L1 = True
  L2 == L2 = True
  _ == _ = False

instance Eq Direction where
  Grid.Right == Grid.Right = True
  Grid.Left == Grid.Left = True
  Up == Up = True
  Down == Down = True
  _ == _ = False

--Gamestate
data GameState = GameState {
                   maze  :: Maze
                 , status :: StatusGame
                 , pressedKey :: Key
                 , elapsedTime :: Float
                 , previousKey :: Key
                 , lastLevel :: Int
                 , unlockedLevels :: [Int]
                 , levels :: [Int]
                 , levelcontents :: [Maze]
                 }

initialState :: GameState --The gamestate when nothing has happened yet
initialState = let grid = []
                   emptyPicture = polygon []
                   pacman = MkPacMan (MkPosition 30 30) Down Open emptyPicture emptyPicture emptyPicture emptyPicture emptyPicture
                   p = MkEnemy Pink (MkPosition 60 90) Up emptyPicture emptyPicture emptyPicture emptyPicture
                   b = MkEnemy Blue (MkPosition 90 60) Up emptyPicture emptyPicture emptyPicture emptyPicture
                   o = MkEnemy Orange (MkPosition 120 120) Up emptyPicture emptyPicture emptyPicture emptyPicture
                   r = MkEnemy Red (MkPosition 300 120) Up emptyPicture emptyPicture emptyPicture emptyPicture
               in GameState (MkMaze grid pacman p b o r) Failed None 0 None 1 [] [] []


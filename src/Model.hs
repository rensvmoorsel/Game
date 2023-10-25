-- | This module contains the data types
--   which represent the state of the game
module Model where
import Graphics.Gloss
import Data.List (elemIndex)
import Data.Maybe

--GameState objects
data Maze = MkMaze {
  grid :: [Tile],
  pacman :: PacMan,
  pinkEnemy :: Enemy,
  blueEnemy :: Enemy,
  orangeEnemy :: Enemy,
  redEnemy :: Enemy
 }
type Width = Float
type Height = Float
data Tile = Empty {xCoord :: Int, yCoord :: Int} | Wall {xCoord :: Int, yCoord :: Int} | Circle { xCoord :: Int, yCoord :: Int}
data PacMan = MkPacMan {
                        position :: Position
                        , direction :: Direction
                        , mouthStatus :: MouthStatus
                        }
data MouthStatus = Open | Closed
data Enemy = MkEnemy EnemyColor Position Direction
data EnemyColor = Red | Orange | Pink | Blue
data Position = MkPosition {
  x :: Float
  , y :: Float
}
data Line = MkLine Position Position
data StatusGame = Running | Paused | Ended
data Direction = Left | Right | Up | Down
data Key = W | A | S | D | Esc | None | L1 | L2

--Instances for the datatypes
instance Eq Tile where
  (Wall x y) == (Empty a b) = a == x && b == y
  (Model.Circle x y) == (Empty a b) = a == x && b == y
  (Empty a b) == (Wall x y) = a == x && b == y
  (Empty a b) == (Model.Circle x y) = a == x && b == y
  Empty a b == Empty x y = a == x && b == y
  Model.Circle x y == Model.Circle a b = a == x && b == y
  Wall x y == Wall a b = x == a && y == b
  Wall a b == Model.Circle x y = x == a && y == b
  Model.Circle a b == Wall x y = a == x && b == y


instance Show EnemyColor where
  show Red = "Red"
  show Orange = "Orange"
  show Pink = "Pink"
  show Blue = "Blue"

instance Eq StatusGame where
  Running == Running = True
  Paused == Paused = True
  Ended == Ended = True
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

--Gamestate
data GameState = GameState {
                   maze  :: Maze
                 , status :: StatusGame
                 , pressedKey :: Key
                 , elapsedTime :: Float
                 , previousKey :: Key
                 }

initialState :: GameState --The gamestate when nothing has happened yet
initialState = let grid = createGrid []
                   pacman = MkPacMan (MkPosition 30 30) Down Open
                   p = MkEnemy Pink (MkPosition 60 90) Up
                   b = MkEnemy Blue (MkPosition 90 60) Up
                   o = MkEnemy Orange (MkPosition 120 120) Up
                   r = MkEnemy Red (MkPosition 300 120) Up
               in GameState (MkMaze grid pacman p b o r) Ended None 0 None

tiletoPath :: Tile -> Path --convert a tile to the positions of the tile
tiletoPath (Wall x y) = let p1 = ((-30 + 30 * fromIntegral x) - 420, (15 - 30 * fromIntegral y) + 480)
                            p2 = ((-30 + 30 * fromIntegral x) - 420, (-15 - 30 * fromIntegral y) + 480)
                            p3 = (30 * fromIntegral x - 420, (-15 - 30 * fromIntegral y) + 480)
                            p4 = (30 * fromIntegral x - 420, (15 - 30 * fromIntegral y) + 480)
                        in
                            [p1, p2, p3, p4]
tiletoPath (Model.Circle x y) = [((-15 + 30 * fromIntegral x) - 420, (-30) * fromIntegral y + 480)]

realPosition :: Tile -> Position --convert tile coordinate to the pixel positions
realPosition (Model.Circle x y) = MkPosition (fromIntegral x * 30 - 30) (fromIntegral y * 30 - 30)
realPosition (Empty x y) = MkPosition (fromIntegral x * 30 - 30) (fromIntegral y * 30 - 30)
realPosition (Wall x y) = MkPosition (fromIntegral x * 30 - 30) (fromIntegral y * 30 - 30)

loadLevel :: Int -> IO GameState
loadLevel level = do
                    levelContent <- readFile $ "src\\levels\\"++ show level ++ ".txt"
                    let levelLines = lines levelContent
                    return GameState {
                        maze = MkMaze
                        {
                            grid = createGrid $ loadLines levelLines 1,
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
                        previousKey = None
                    }
                    where
                        loadLine :: String -> Int -> Int -> [Tile]
                        loadLine [] _ _ = []
                        loadLine (item:items) x y | item == 'W' = Wall x y:loadLine items (x + 1) y
                                                   | otherwise = Empty x y: loadLine items (x + 1) y
                        loadLines :: [String] -> Int -> [Tile]
                        loadLines [] _ = []
                        loadLines (line:lines) y = loadLine line 1 y ++ loadLines lines (y + 1)

                        orangePosition :: [String] -> Position
                        orangePosition a = orangePosition' a 1

                        redPosition :: [String] -> Position
                        redPosition a = redPosition' a 1

                        bluePosition :: [String] -> Position
                        bluePosition a = bluePosition' a 1

                        pinkPosition :: [String] -> Position
                        pinkPosition a = pinkPosition' a 1

                        pacmanPosition :: [String] -> Position
                        pacmanPosition a = pacmanPosition' a 1

                        orangePosition' :: [String] -> Int -> Position
                        orangePosition' (x:xs) y | 'o' `elem` x = MkPosition (fromIntegral (fromMaybe 0 (elemIndex 'o' x)) * 30) (fromIntegral y * 30)
                                                | otherwise = orangePosition' xs (y + 1)

                        redPosition' :: [String] -> Int -> Position
                        redPosition' (x:xs) y | 'r' `elem` x = MkPosition (fromIntegral (fromMaybe 0 (elemIndex 'r' x)) * 30) (fromIntegral y * 30)
                                                | otherwise = redPosition' xs (y + 1)

                        bluePosition' :: [String] -> Int -> Position
                        bluePosition' (x:xs) y | 'b' `elem` x = MkPosition (fromIntegral (fromMaybe 0 (elemIndex 'b' x)) * 30) (fromIntegral y * 30)
                                                | otherwise = bluePosition' xs (y + 1)

                        pinkPosition' :: [String] -> Int -> Position
                        pinkPosition' (x:xs) y | 'p' `elem` x = MkPosition (fromIntegral (fromMaybe 0 (elemIndex 'p' x)) * 30) (fromIntegral y * 30)
                                                | otherwise = pinkPosition' xs (y + 1)

                        pacmanPosition' :: [String] -> Int -> Position
                        pacmanPosition' (x:xs) y | 'P' `elem` x = MkPosition (fromIntegral (fromMaybe 0 (elemIndex 'P' x)) * 30) (fromIntegral y * 30)
                                                | otherwise = pacmanPosition' xs (y + 1)


createGrid :: [Tile] -> [Tile] -- fills empty spots of grid with empty tiles
createGrid list = [ Empty x y | x <- [1 .. 28], y <- [1 .. 31], Empty x y `notElem` list] ++ list
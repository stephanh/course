module TicTacToe (
  Board
, Player (..)
, Position
, Outcome
, start
, move
, whoWon
, takeBack
, playerAt
, main
, eval
, eval'
) where

import           Test.QuickCheck
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.List

data Player = Red | Blue deriving (Eq, Ord)

data EmptyBoard = EmptyBoard deriving (Eq, Show)

data Board = Board Grid Player [Position] deriving (Eq)

data FinishedBoard = FinishedBoard (Map Position Player) (Maybe Player) [Position] deriving (Eq)

type Grid = Map Position Player

data Position = NW | N | NE | W | C | E | SW | S | SE deriving (Eq, Show, Ord, Enum, Bounded)

data Outcome = NotStarted | InvalidMove | ValidMove Board | Finished FinishedBoard deriving (Eq, Show)

start :: Position -> Outcome
start posn = move NotStarted posn

move :: Outcome -> Position -> Outcome
move NotStarted posn = move' (Left EmptyBoard) posn
move (ValidMove board) posn = move' (Right board) posn
move x _ = x

move' :: Either EmptyBoard Board -> Position -> Outcome
move' (Right (Board grid _ _)) posn | M.member posn grid = InvalidMove
move' (Right (Board grid player moves)) posn = move'' (M.insert posn player grid) player (posn : moves)
move' (Left _) posn = ValidMove (Board (M.singleton posn Red) Blue [posn])

move'' :: Grid -> Player -> [Position] -> Outcome
move'' grid player moves | winner (M.filter (== player) grid) = Finished (FinishedBoard grid (Just player) moves)
move'' grid _ moves | (length moves) == 9  = Finished (FinishedBoard grid Nothing moves)
move'' grid player moves = ValidMove (Board grid (swapPlayer player) moves)

whoWon :: Outcome -> Maybe Player
whoWon (Finished board) = whoWon' board
whoWon _ = Nothing

whoWon' :: FinishedBoard -> Maybe Player
whoWon' (FinishedBoard _ player _) = player

takeBack :: Outcome -> Outcome
takeBack (ValidMove board) = takeBackHelper (takeBack' (Left board))
takeBack (Finished board) = takeBackHelper (takeBack' (Right board))
takeBack x = x

takeBack' :: Either Board FinishedBoard -> Either EmptyBoard Board
takeBack' (Left (Board _ _ [_])) = Left EmptyBoard
takeBack' (Left (Board grid player (posn:moves))) = Right (Board (M.delete posn grid) (swapPlayer player) moves)
takeBack' (Right (FinishedBoard grid _ (posn:moves))) = Right (Board (M.delete posn grid) (grid  M.! posn) moves)
takeBack' _ = error "Reached an impossible board state"

takeBackHelper :: Either EmptyBoard Board -> Outcome
takeBackHelper (Left _) = NotStarted
takeBackHelper (Right board) = ValidMove board

playerAt :: Outcome -> Position -> Maybe Player
playerAt (ValidMove board) posn = playerAt' (Left board) posn
playerAt (Finished board) posn = playerAt' (Right board) posn
playerAt _ _  = Nothing

playerAt' :: Either Board FinishedBoard -> Position -> Maybe Player
playerAt' (Left (Board grid _ _)) posn = M.lookup posn grid
playerAt' (Right (FinishedBoard grid _ _)) posn = M.lookup posn grid


swapPlayer :: Player -> Player
swapPlayer Red = Blue
swapPlayer Blue = Red

winner :: Grid -> Bool
winner grid = any (\x -> all (\y -> M.member y grid) x) winningMoves

winningMoves :: [[Position]]
winningMoves = [[NW, N, NE], [W, C, E], [SW, S, SE],
                [NW, W, SW], [N, C, S], [NE, E, SE],
                [NW, C, SE], [NE, C, SW]]


eval :: Outcome -> [Position] -> Outcome
eval outcome posns = foldl move outcome posns

eval' :: [Position] -> Outcome
eval' (h:t) = eval (start h) t
eval' _ = NotStarted

instance Arbitrary Position where
  arbitrary = elements [NW, N, NE, W, C, E, SW, S, SE]

instance Arbitrary Player where
  arbitrary = elements [Red, Blue]

instance Arbitrary Board where
  arbitrary = do 
    posn <- arbitrary
    positions <- arbitrary
    return $ foldr propell (start' posn) positions

start':: Position -> Board
start' posn = Board (M.singleton posn Red) Red [posn]

propell :: Position -> Board -> Board
propell p b =
  case move' (Right b) p of
    ValidMove b' -> b'
    _ -> b

-- Two moves to the same position are invalid
testInvalidMove2 :: Board -> Position -> Bool
testInvalidMove2 board posn = case (move' (Right board) posn) of
  x @ (ValidMove _) -> move x posn == InvalidMove
  _ -> True

split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = (take n xs) : (split n (drop n xs))

cells :: Map Position Player -> [Maybe Player]
cells grid = map (\x -> M.lookup x grid) (enumFrom NW)

cellsToChar :: [Maybe Player] -> [String]
cellsToChar xs = map helper xs
  where
    helper Nothing = " "
    helper (Just Red) = "X"
    helper (Just Blue) = "O"


ppGrid :: Grid -> String
ppGrid grid = intercalate "\n--|---|--\n" (map (intercalate " | ") (split 3 (cellsToChar (cells grid))))

instance Show Board where
  show (Board grid Red _) = "\n" ++ (ppGrid grid) ++ "\nO's move"
  show (Board grid Blue _) = "\n" ++ (ppGrid grid) ++ "\nX's move"

instance Show FinishedBoard where
  show (FinishedBoard grid (Just Red) _) = "\n" ++ (ppGrid grid) ++ "\nX won"
  show (FinishedBoard grid (Just Blue) _) = "\n" ++ (ppGrid grid) ++ "\nO won"
  show (FinishedBoard grid Nothing _) = "\n" ++ (ppGrid grid) ++ "\nDraw"
                                                

instance Show Player where
  show Red = "X"
  show Blue = "O"

propSwapPlayer :: Player -> Bool
propSwapPlayer player = swapPlayer player /= player

main :: IO ()
main = do
  quickCheck propSwapPlayer
  quickCheck testInvalidMove2

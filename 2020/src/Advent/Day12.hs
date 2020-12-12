module Advent.Day12 where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Data.Text as DT
import Data.Void

type Parser = P.Parsec Void DT.Text


data Action = N Int | S Int | E Int | W Int | F Int | L Int | R Int
  deriving (Eq, Show)

data Facing = North | East | South | West
  deriving (Eq, Show)
toDegree :: Facing -> Int
toDegree North = 0
toDegree East = 90
toDegree South = 180
toDegree West = 270
fromDegree :: Int -> Facing
fromDegree  0 = North
fromDegree  90 = East
fromDegree  180 = South
fromDegree  270 = West
fromDegree  x = error "bad degree"

data State = State (Int, Int) Facing
  deriving (Eq, Show)
initialState :: State
initialState = State (0,0) East
getManhattan :: State -> Int
getManhattan (State (x,y) _) = (abs x) + (abs y)


parseDirection :: Parser Action
parseDirection = do
  action <- P.choice
              [ N <$ P.char 'N'
              , S <$ P.char 'S'
              , E <$ P.char 'E'
              , W <$ P.char 'W'
              , F <$ P.char 'F'
              , L <$ P.char 'L'
              , R <$ P.char 'R'
              ]
  value <- read <$> P.some P.digitChar
  return $ action value

parser :: Parser [Action]
parser = parseDirection `P.sepEndBy` P.newline

move :: (Int, Int) -> Int -> Facing -> (Int, Int)
move (x,y) distance North = (x, y + distance)
move (x,y) distance East = (x + distance, y)
move (x,y) distance South = (x, y - distance)
move (x,y) distance West = (x - distance, y)

changeFacing :: Facing -> Int -> Facing
changeFacing facing change = fromDegree $ (toDegree facing + change) `mod` 360

next :: State -> Action -> State
next (State (x, y) facing) action =
  case action of
          (N v) -> (State (move (x, y) v North) facing)
          (E v) -> (State (move (x, y) v East) facing)
          (S v) -> (State (move (x, y) v South) facing)
          (W v) -> (State (move (x, y) v West) facing)
          (F v) -> (State (move (x, y) v facing) facing)
          (L v) -> (State (x, y) (changeFacing facing (-v)))
          (R v) -> (State (x, y) (changeFacing facing v))

solve :: [Action] -> Int
solve xs = getManhattan $ foldl (next) initialState xs

data StateTwo = StateTwo (Int, Int) (Int, Int)
  deriving (Eq, Show)
initialStateTwo :: StateTwo
initialStateTwo = StateTwo (0,0) (10,1)
getManhattanTwo :: StateTwo -> Int
getManhattanTwo (StateTwo (x,y) _) = (abs x) + (abs y)

moveToWayPoint :: StateTwo -> Int -> StateTwo
moveToWayPoint (StateTwo (x, y) (wx, wy)) speed =
  StateTwo (x + (wx * speed), y + (wy * speed)) (wx, wy)

rotateWayPoint :: StateTwo -> Int -> StateTwo
rotateWayPoint (StateTwo (x,y) (wx,wy)) 0 = (StateTwo (x,y) (wx,wy))
rotateWayPoint (StateTwo (x,y) (wx,wy)) r =
  rotateWayPoint (StateTwo (x,y) (wy,(-wx))) (r - 90)


nextTwo :: StateTwo -> Action -> StateTwo
nextTwo s@(StateTwo (x, y) (wx, wy)) action =
  case action of
          (N v) -> (StateTwo (x,y) (move (wx,wy) v North))
          (E v) -> (StateTwo (x,y) (move (wx,wy) v East))
          (S v) -> (StateTwo (x,y) (move (wx,wy) v South))
          (W v) -> (StateTwo (x,y) (move (wx,wy) v West))
          (F v) -> moveToWayPoint s v
          (L v) -> rotateWayPoint s (360 - v)
          (R v) -> rotateWayPoint s v

solveTwo :: [Action] -> Int
solveTwo xs = getManhattanTwo $ foldl (nextTwo) initialStateTwo xs



main :: IO ()
main = do
  input <- DT.pack <$> readFile "Day12.txt"

  print $ moveToWayPoint initialStateTwo (10)

  case P.parse parser "" input
    of Right parsed -> do 
        print $ solve parsed
        print $ solveTwo parsed

-- $> main

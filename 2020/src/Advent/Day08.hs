{-# Language DeriveFunctor #-}
{-# Language OverloadedStrings #-}
module Advent.Day08 where
import Debug.Trace
import Text.Megaparsec as P hiding (State)
import Text.Megaparsec.Char as P
import Data.Void
import Data.Text hiding (length, zip, find)
import Data.Foldable
import Data.Maybe
import Data.Set
import qualified Data.Map as DM (fromList, Map, lookup)

-- parse input
type Parser = Parsec Void Text

data Operation = Acc Int | Jmp Int | Nop Int
  deriving Show

data Sign = Plus | Neg deriving Eq

parseOperation :: Parser Operation
parseOperation = do
  operationType <- choice
            [ Acc <$ string "acc"
            , Jmp <$ string "jmp"
            , Nop <$ string "nop"
            ]
  _ <- space
  sign <- Plus <$ char '+' <|> Neg <$ char '-'
  num <- read <$> some digitChar
  pure $ operationType (if sign == Plus then num else -num)

parseInput :: Parser [Operation]
parseInput = parseOperation `sepEndBy` newline

-- solution
-- position -> visited ->
newtype Position a = Position a
  deriving (Eq, Show, Functor, Ord)
unwrapP :: Position Int -> Int
unwrapP (Position a) = a
newtype Accumulator a = Accumulator a
  deriving (Eq, Show, Functor)
newtype State = State (Position Int, Accumulator Int)
initialState :: State
initialState = State (Position 0, Accumulator 0)
getPos :: State -> Position Int
getPos (State (p, _)) = p

type Visited = Set (Position Int)
solution :: State -> Visited -> [Operation] -> Accumulator Int
solution s@(State (position, accumulator)) visited ops =
  if member position visited 
    then accumulator
    else solution 
           (moveState s (ops !! unwrapP position))
           (insert position visited)
           ops

moveState :: State -> Operation -> State
moveState (State (position, accumulator)) op =
  case op of
    Acc x -> 
        (State (((+ 1) <$> position), (+ x) <$> accumulator))
    Jmp x ->
        (State (((+ x) <$> position), accumulator))
    Nop _ ->
      (State (((+ 1) <$> position), accumulator))


  
switchJmpAndNop :: Operation -> Operation
switchJmpAndNop (Acc x) = Acc x
switchJmpAndNop (Jmp x) = Nop x
switchJmpAndNop (Nop x) = Jmp x

switchOpByIndex :: Int -> [Operation] -> [Operation]
switchOpByIndex i xs =
  let (start,x:end) = Prelude.splitAt i xs
  in start ++ (switchJmpAndNop x) : end

getEnd :: Position Int -> State -> Visited -> [Operation] -> Maybe (Accumulator Int)
getEnd end s@(State (position, accumulator)) visited ops =
  if member position visited 
    then Nothing
    else if position == end 
           then Just accumulator
           else getEnd end
                  (moveState s (ops !! unwrapP position))
                  (insert position visited)
                  ops

solutionTwo ops = 
  let endIndex = length ops
  in getEnd' (Position endIndex) initialState Data.Set.empty ops

getEnd' :: Position Int -> State -> Visited -> [Operation] -> Maybe (Accumulator Int)
getEnd' end s@(State (position, accumulator)) visited ops =
  if member position visited 
    then Nothing
    else if position == end 
           then Just accumulator
           else let swappedCurrentOps = switchOpByIndex (unwrapP position) ops
                in getEnd' end
                  (moveState s (ops !! unwrapP position))
                  (insert position visited)
                  ops
                <|>
                getEnd end
                  (moveState s ((swappedCurrentOps !! unwrapP position))
                  (insert position visited)
                  swappedCurrentOps


main :: IO ()
main = do
  file <- readFile "Day08.txt"
  case parse parseInput "" $ pack file of
    Right parsed -> do
      print $ solution initialState Data.Set.empty parsed
      print $ solutionTwo parsed
  print ""


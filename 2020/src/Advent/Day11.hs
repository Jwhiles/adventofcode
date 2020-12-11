{-# Language OverloadedStrings #-}
module Advent.Day11 where
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Data.Void
import qualified Data.Text as DT
import qualified Data.Map.Strict as M
import Debug.Trace
import Data.Maybe (catMaybes)

type Parser = P.Parsec Void DT.Text

data Square = Occupied | Empty | Floor
  deriving (Eq,Show)

parseSquare :: Parser Square
parseSquare = P.choice [ Occupied <$ P.char '#'
                       , Empty <$ P.char 'L'
                       , Floor <$ P.char '.'
                       ]

parseLine :: Parser [Square]
parseLine = P.some parseSquare

parse :: Parser [[Square]]
parse = parseLine `P.sepEndBy` P.newline

type Grid = M.Map (Int,Int) Square

buildGrid :: [[Square]] -> Grid
buildGrid map = foldr 
  (\(i, rows) acc -> foldr (\(j, x) acc' -> M.insert (i, j) x acc') acc (zip [0,1..] rows)) 
  M.empty (zip [0,1..] map)

getNeighbours :: (Int,Int) -> Grid -> [Square]
getNeighbours (x,y) grid = 
  let neighbours = [(x',y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1]]
  in catMaybes $ (flip M.lookup) grid  <$> neighbours

     
  

iterateSquare :: Square -> [Square] -> Square
iterateSquare Floor _ = Floor

iterateSquare Empty neighbours =
  if (length $ filter (== Occupied) neighbours) == 0
     then Occupied
     else Empty

iterateSquare Occupied neighbours =
  if (length $ filter (== Occupied) neighbours) < 4
     then Occupied
     else Empty
           
iter :: Grid -> Grid
iter grid = M.mapWithKey f grid
  where f coords value = 
                   let neighbours = getNeighbours coords grid
                   in iterateSquare value neighbours

countOccupied :: Grid -> Int
countOccupied = foldr 
  (\square count -> if square == Occupied then count + 1 else count) 0

solve :: [[Square]] -> Int
solve input = countOccupied $ go $ buildGrid input
  where go grid = 
            let newGrid = iter grid
            in if newGrid == grid then grid else go newGrid


-- part two
add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x,y) (x',y') = (x + x', y + y')

getVisible :: (Int,Int) -> Grid -> [Square]
getVisible coord grid = 
  let directions = [(x',y') | x' <- [(-1),0,1], y' <- [(-1),0,1], (x',y') /= (0,0)]
  in catMaybes $ lookupInDirection grid coord <$> directions

lookupInDirection :: Grid -> (Int, Int) -> (Int, Int) -> Maybe Square
lookupInDirection grid coordinate vector =
  M.lookup (coordinate `add` vector) grid >>= (\square -> 
    if square == Floor 
      then lookupInDirection grid (coordinate `add` vector) vector
      else Just square)

iterateSquare' :: Square -> [Square] -> Square
iterateSquare' Floor _ = Floor

iterateSquare' Empty neighbours =
  if (length $ filter (== Occupied) neighbours) == 0
     then Occupied
     else Empty

iterateSquare' Occupied neighbours =
  if (length $ filter (== Occupied) neighbours) < 5
     then Occupied
     else Empty
           
iter' :: Grid -> Grid
iter' grid = M.mapWithKey f grid
  where f coords value = 
                   let visible = getVisible coords grid
                   in iterateSquare' value visible

solve2 :: [[Square]] -> Int
solve2 input = countOccupied $ go $ buildGrid input
  where go grid = 
            let newGrid = iter' grid
            in if newGrid == grid then grid else go newGrid

main :: IO ()
main = do
  file <- DT.pack <$> readFile "Day11.txt"

  case P.parse parse "" file of
    Right parsed -> do 
      print $ solve2 parsed


    

-- $> main

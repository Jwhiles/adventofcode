{-# Language OverloadedStrings #-}

module Day03 where

import Core
import Data.Text as DT
import Data.Set as Set
import Data.Map.Strict as Map

parser :: Parser (Dirs,Dirs)
parser txt = let (one:two:_) = DT.lines txt
             in (p one, p two)
               where p t = (\(direction:int) -> (getDir direction, read int)) . DT.unpack 
                            <$> (DT.splitOn "," t)

data Direction = UP | DOWN | RIGHT | LEFT
  deriving (Show, Eq)
getDir :: Char -> Direction
getDir 'U' = UP
getDir 'D' = DOWN
getDir 'R' = RIGHT
getDir 'L' = LEFT
getDir _   = error "bad input"

type Coord = (Int, Int)
type Dirs = [(Direction, Int)]

getVisited :: Coord -> Dirs -> Set Coord
getVisited _ [] = Set.empty
getVisited start ((d, distance):rest) = Set.union 
                                          (Set.fromList $ (getNewVisited start d distance))
                                          (getVisited (updateCoords start d distance) rest)

updateCoords :: Coord -> Direction -> Int -> Coord
updateCoords (x, y) UP move = (x, y + move)
updateCoords (x, y) DOWN move = (x, y - move)
updateCoords (x, y) RIGHT move = (x + move, y)
updateCoords (x, y) LEFT move = (x - move, y)

getNewVisited :: Coord -> Direction -> Int -> [Coord]
getNewVisited (x, y) UP move = [(x, newY) | newY <- [y .. (y+move)]]
getNewVisited (x, y) DOWN move = [(x, newY) | newY <- [(y-move) .. y]]
getNewVisited (x, y) RIGHT move = [(newX, y) | newX <- [x .. (x+move)]]
getNewVisited (x, y) LEFT move = [(newX, y) | newX <- [(x-move) .. x]]


getDistance :: Coord -> Int
getDistance (x, y) = abs x + abs y

solve :: (Dirs,Dirs) -> Int
solve (xs,ys) = 
  let pathOne = getVisited (0,0) xs
      pathTwo = getVisited (0,0) ys
  in Set.findMin $ Set.filter (/=0) $ Set.map getDistance $ Set.intersection pathOne pathTwo


getCoordsAndDistance :: Int -> Coord -> Dirs -> [(Coord, Int)]
getCoordsAndDistance _ _ [] = []
getCoordsAndDistance total (x,y) ((_, 0):rest) = getCoordsAndDistance total (x,y) rest
getCoordsAndDistance total (x,y) ((UP, move):rest) =
               [((x, y + 1), total + 1)] <> getCoordsAndDistance (total+1) (x,y+1) ((UP, move-1):rest)
getCoordsAndDistance total (x,y) ((DOWN, move):rest) =
               [((x, y - 1), total + 1)] <> getCoordsAndDistance (total+1) (x,y-1) ((DOWN, move-1):rest)
getCoordsAndDistance total (x,y) ((LEFT, move):rest) =
               [((x-1, y), total + 1)] <> getCoordsAndDistance (total+1) (x-1,y) ((LEFT, move-1):rest)
getCoordsAndDistance total (x,y) ((RIGHT, move):rest) =
               [((x+1, y), total + 1)] <> getCoordsAndDistance (total+1) (x+1,y) ((RIGHT, move-1):rest)

-- I think my approach for part two is generally nicer. Than what I did for part 1

solve' :: (Dirs,Dirs) -> Int
solve' (xs,ys) = 
  let pathOne = Map.fromList $ getCoordsAndDistance 0 (0,0) xs
      pathTwo = Map.fromList $ getCoordsAndDistance 0 (0,0) ys
  in Set.findMin $ Set.fromList $ Map.elems $ Map.intersectionWith (+) pathOne pathTwo


examples :: [(Dirs,Dirs)]
examples = 
  [ parser "R8,U5,L5,D3\nU7,R6,D4,L4"
  , parser "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" 
  , parser "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
  ]
solutions = [6, 159, 135, 1]

examplesTwo :: [(Dirs,Dirs)]
examplesTwo = 
  [ parser "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
  , parser "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
  ]
s2 = [610,410]

main :: IO ()
main = do
  problemOne <- runHarness $
    Harness parser "./day03.txt" solve examples solutions
  print problemOne

  problemTwo <- runHarness $
    Harness parser "./day03.txt" solve' examplesTwo s2
  print $ "problem two: " <> show problemTwo


-- $> main




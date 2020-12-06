
{-# Language OverloadedStrings #-}
module Advent.Day03 where
import System.IO
import Data.List
import Advent.Core
import qualified Data.Text as DT

parser :: Parser [String]
parser = fmap (DT.unpack) .  DT.lines

example = [[
  "..##.......",
  "#...#...#..",
  ".#....#..#.",
  "..#.#...#.#",
  ".#...##..#.",
  "..#.##.....",
  ".#.#.#....#",
  ".#........#",
  "#.##...#...",
  "#...##....#",
  ".#..#...#.#" ]]
partOneSolutions = [7]
partTwoSolutions = [336]


partOne map = travel (length $ map !! 0) (3, 1) map

partTwo treeMap = 
 let slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]
     width = length (treeMap !! 0)
     hits = fmap (\slope -> travel width slope treeMap) slopes
 in foldr (\x y -> x * y) 1 hits




range :: Int -> Int -> [Int]
range xvec yvec = go 0
  where go x = take yvec (repeat x) <> go (x + xvec)

travel :: Int -> (Int, Int) -> [String] -> Int
travel width (xvec, yvec) map = 
  let positions = zip3 (range xvec yvec) [0,1..] map
  in length $ filter id $ fmap (\(x, y, submap) -> if (y `mod` yvec) == 0 
                                then submap !! (x `mod` width) == '#'
                                else False) positions


main :: IO ()
main = do
  problemOne <- runHarness $
    Harness parser "./day03.txt" partOne example partOneSolutions

  print problemOne

  problemTwo <- runHarness $
    Harness parser "./day03.txt" partTwo example partTwoSolutions 

  print problemTwo

--
-- $> main

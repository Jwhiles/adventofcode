module Day01 where
import System.IO
import Data.List
import Core
import Debug.Trace
import qualified Data.Text as DT


partOne :: [Int] -> Int
partOne xs = 
  case [(x,y) | x <- xs, y <- xs, y + x == 2020] of
    (x,y):rest -> x * y
    _ -> error "no match in input"
    



partOneEx        :: [[Int]]
partOneEx        = [[1721, 979, 366, 299, 675, 1456]]
partOneSolutions = [514579]


partTwo :: [Int] -> Int
partTwo xs = 
  case [(x,y,z) | x <- xs, y <- xs, z <- xs, y + x + z == 2020] of
    (x,y,z):rest -> x * y * z
    _ -> error "no match in input"

partTwoEx        :: [[Int]]
partTwoEx        = [[1721, 979, 366, 299, 675, 1456]]
partTwoSolutions = [241861950]

parser :: Parser [ Int ]
parser = fmap (read . DT.unpack) . DT.lines
main :: IO ()
main = do
  input <- getInput parser "./day01.txt"
  problemOne <- runHarness $
    Harness parser "./day01.txt" partOne partOneEx partOneSolutions

  print problemOne

  problemTwo <- runHarness $
    Harness parser "./day01.txt" partTwo partTwoEx partTwoSolutions 

  print problemTwo

--
-- $> main

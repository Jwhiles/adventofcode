{-# Language OverloadedStrings #-} 
module Advent.Day09 where
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Data.Text as DT
import Data.List (inits)
import Data.Void

type Parser = P.Parsec Void DT.Text

parser :: Parser [Int]
parser = (read <$> P.some P.digitChar) `P.sepEndBy` P.newline

solve1 :: [Int] -> Int
solve1 xs =
  let (start, x:_) = splitAt 25 xs
  in case [(a,b) | a <- start, b <- start, a + b == x]
    of ((a,b):_) -> solve1 $ tail xs
       []        -> x

findContiguous ::  [Int] -> [Int] -> Int -> [Int]
findContiguous count xs x = 
  case compare (sum count) x
    of EQ -> count
       LT -> findContiguous (count ++ [head xs]) (tail xs) x
       GT -> findContiguous (tail count) xs x

solve2 :: [Int] -> Int
solve2 xs = 
  let contiguous = findContiguous [] xs (solve1 xs)
  in minimum contiguous + maximum contiguous


main :: IO ()
main = do
  file <- DT.pack <$> readFile "Day09.txt"
  case P.parse parser "" file of
    Right parsed -> do 
      print "problem one: "
      print $ solve1 parsed
      print " problem two: "
      print $ solve2 parsed
    Left e -> error $ show e

-- $> main

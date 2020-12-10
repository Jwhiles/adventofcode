{-# Language OverloadedStrings #-}
module Advent.Day10 where
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Data.Text as DT
import Data.List (inits)
import Data.Void
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe
import Debug.Trace

type Parser = P.Parsec Void DT.Text

parser :: Parser [Int]
parser = (read <$> P.some P.digitChar) `P.sepEndBy` P.newline

--                          volts  count
type Differences = M.Map Int    Int

multiplyVolts :: Differences -> Int
multiplyVolts ds =
  case (M.lookup 1 ds, M.lookup 3 ds)
    of (Just ones, Just threes) -> ones * threes
       _ -> error "oh no"

findDifferences :: [Int] -> Differences
findDifferences vs =
  let differences = M.empty
  in go differences vs
  where go differences (x:y:rest) = go (M.insertWith (+) (y - x) 1 differences)  (y:rest)
        go differences (x:[]) = (M.insertWith (+) 3 1 differences)

solve1 :: [Int] -> Int
solve1 xs =
  let sorted = L.sort xs
  in multiplyVolts $ findDifferences (0:sorted)


solve2 xs =
  let
    sorted = L.sort xs
    paths = foldl (\acc x ->
                    M.insert x (howManyWaysCouldIGetHere acc x) acc) (start sorted) sorted
  in howManyWaysCouldIGetHere paths (last sorted + 3)

start xs =  M.fromList $ zip (takeWhile (<= 3) xs) [1,1..]

howManyWaysCouldIGetHere :: M.Map Int Int -> Int -> Int
howManyWaysCouldIGetHere m x =
  let ways = filter (>0) [x-3..x]
  in sum $ catMaybes $ (\k -> M.lookup k m) <$> ways


main :: IO ()
main = do
  file <- DT.pack <$> readFile "Day10.txt"
  case P.parse parser "" file of
    Right parsed -> do
      print "problem one: "
      -- print $ solve1 parsed

      print $ solve2 parsed
    Left e -> error $ show e

-- $> main


{-# Language OverloadedStrings #-}
module Day02 where
import System.IO
import Data.List
import Core
import Debug.Trace
import qualified Data.Text as DT



data PWDef = PWDef Int Int Char
  deriving Show

parser :: Parser [ (PWDef, String) ]
parser = fmap  (get . DT.splitOn ":") . DT.lines
  where get [def, pw] = 
            let [count, char] = DT.splitOn " " def
                [min, max] = read . DT.unpack <$> DT.splitOn "-" count
            in (PWDef min max (DT.head char), DT.unpack $ DT.tail pw)
        get  e = error $ show (fmap DT.unpack e)

partOneEx :: [[(PWDef, String)]]
partOneEx = [[
    (PWDef 1 3 'a', "abcde"),
    (PWDef 1 3 'b', "cdefg"),
    (PWDef 2 9 'c', "ccccccccc")
    ]]
partOneSolutions = [2]

partOne :: [(PWDef, String)] -> Int
partOne = length . filter id . map validatePair

validatePair :: (PWDef, String) -> Bool
validatePair (PWDef min max char, password) =
  let charOccurences =
       foldr (\pwChar count -> if pwChar == char then count + 1 else count) 0 password
  in charOccurences >= min && charOccurences <= max


partTwoEx :: [[(PWDef, String)]]
partTwoEx = [[
    (PWDef 1 3 'a', "abcde"),
    (PWDef 1 3 'b', "cdefg"),
    (PWDef 2 9 'c', "ccccccccc")
    ]]
partTwoSolutions = [1]

partTwo :: [(PWDef, String)] -> Int
partTwo = length . filter id . map validatePwTwo

validatePwTwo :: (PWDef, String) -> Bool
validatePwTwo (PWDef min max char, password) =
  let charOne = password !! (min - 1) 
      charTwo = password !! (max - 1) 
  in (length $ filter (== char) [charOne, charTwo]) == 1

main :: IO ()
main = do
  input <- getInput parser "./day02.txt"
  problemOne <- runHarness $
    Harness parser "./day02.txt" partOne partOneEx partOneSolutions

  print problemOne

  problemTwo <- runHarness $
    Harness parser "./day02.txt" partTwo partTwoEx partTwoSolutions 

  print problemTwo

--
-- $> main

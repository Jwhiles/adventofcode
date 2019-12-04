module Day04 where
import Data.List

range :: [Int] 
range = [146810..612564]

test :: ([Char] -> Bool) -> Int -> [Int] -> Int
test pred count (x:rest) = 
  let x'@(a:b:c:d:e:f:[]) = show x
      notDecreasing = a <= b && b <= c && c <= d && d <= e && e <= f
      digits = pred x'
  in if notDecreasing && digits 
      then test pred (count + 1) rest
      else test pred count rest
test _ count _ = count

containsTwoRepeatedDigits :: Eq a => [a] -> Bool
containsTwoRepeatedDigits xs = not . null . filter (\x -> length x == 2) $ group xs

partOnePred :: Eq a => [a] -> Bool
partOnePred (a:b:c:d:e:f:[]) = a == b || b == c || c == d || d == e || e == f
partOnePred _ = False

main :: IO ()
main = do
  let partOne = test partOnePred
  let partTwo = test containsTwoRepeatedDigits
  print $ partOne 0 [111111] == 1
  print $ partOne 0 [223450] == 0
  print $ partOne 0 [123789] == 0

  print $ partTwo 0 [112233] == 1
  print $ partTwo 0 [123444] == 0
  print $ partTwo 0 [111122] == 1

  print $ partOne 0 range
  print $ partTwo 0 range


-- $> main

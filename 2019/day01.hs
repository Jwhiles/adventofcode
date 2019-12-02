module Day01 where
import System.IO
import Data.List
import Data.Monoid
import Core
import Data.Text as DT

getFuelRequirement :: Int -> Int
getFuelRequirement mass =
  (mass `div` 3) - 2

sumFuleReq :: [ Int ] -> Int
sumFuleReq xs = getSum $
  foldMap (Sum . getFuelRequirement) xs

getFuelRequirementWithFuel :: Int -> Int
getFuelRequirementWithFuel x =
  if fuel <= 0 then 0 else fuel + getFuelRequirementWithFuel fuel
  where fuel = getFuelRequirement x

sumFuelReqWithFuel :: [ Int ] -> Int
sumFuelReqWithFuel xs = getSum $
  foldMap (Sum. getFuelRequirementWithFuel) xs

parser :: Parser [ Int ]
parser = fmap (read . DT.unpack) . DT.lines

partOneEx        :: [[Int]]
partOneEx        = pure <$> [12,14,1969, 100756]
partOneSolutions = [2, 2, 654,  33583 ]

main :: IO ()
main = do
  input <- getInput parser "./day01.txt"
  problemOne <- runHarness $
    Harness parser "./day01.txt" sumFuleReq partOneEx partOneSolutions

  print problemOne

  problemTwo <- runHarness $
    Harness parser "./day01.txt" sumFuelReqWithFuel [] []

  print problemTwo

--
-- $> main

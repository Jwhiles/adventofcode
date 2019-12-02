module Day01 where
import System.IO
import Data.List
import Data.Monoid

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

main :: IO ()
main = do
  input <- (fmap read . lines) <$> readFile "./day01.txt"

  print $ getFuelRequirement 12 == 2
  print $ getFuelRequirement 14 == 2
  print $ getFuelRequirement 1969 == 654 
  print $ getFuelRequirement 100756 == 33583

  print $ sumFuleReq input
  print $ sumFuelReqWithFuel input

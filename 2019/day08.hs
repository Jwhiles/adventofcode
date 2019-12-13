{-# Language ScopedTypeVariables #-}
module Day08 where

import Core
import Data.Text (Text)
import Data.Char (digitToInt)
import Data.List (sortBy)

imageHeight :: Int
imageHeight = 6
imageWidth :: Int
imageWidth = 25
pixelsPerImage :: Int
pixelsPerImage = imageHeight * imageWidth

splitIntoLayersBy :: Int -> [Int] -> [[Int]]
splitIntoLayersBy by input =
  if length input < by 
  then []
  else 
    let (currentChunk, rest) = splitAt by input
    in [currentChunk] <> (splitIntoLayersBy by rest)

howMany :: Int -> [Int] -> Int
howMany what xs = length $ filter (==what) xs


findFewestZeroes :: [[Int]] -> [(Int, [Int])]
findFewestZeroes xs = (\xs -> (howMany 0 xs, xs)) <$> xs

combinePixel :: Int -> Int -> Int
combinePixel 2 r = r
combinePixel l _ = l

combineLayer :: [Int] -> [Int] -> [Int]
combineLayer = zipWith combinePixel

combineLayers :: [[Int]] -> [Int]
combineLayers layers = foldr1 combineLayer layers

  
partTwo :: [Int] -> [[Int]]
partTwo = splitIntoLayersBy 25 . combineLayers . splitIntoLayersBy pixelsPerImage

partOne :: [Int] -> Int
partOne input =
  let sorted = sortBy 
                 (\(a,_) (b,_) -> compare a b) 
                 (findFewestZeroes $ splitIntoLayersBy pixelsPerImage input)
      chosenLayer = snd $ sorted !! 0
  in (howMany 1 chosenLayer) * (howMany 2 chosenLayer)


main :: IO ()
main = do
  input  <- readFile "day08.txt"
  let (parsed :: [Int]) = digitToInt <$> input 
  let result = partOne parsed
  let resultTwo = partTwo parsed

  print result
  pure ()
  
-- $> main

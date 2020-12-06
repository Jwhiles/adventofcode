{-# Language GADTs #-}
{-# LANGUAGE RecordWildCards   #-}
{-# Language OverloadedStrings #-}
module Advent.Day06 where

import Debug.Trace
import Control.Applicative
import Control.Monad
import Data.Text (Text, pack)
import Data.Void
import Data.Char
import qualified Data.Set as DS
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T

main :: IO ()
main = do 
  file <- readFile "./Day06.txt"
  case parse parser "Day06.txt" (pack file) of
    Right parsed -> do 
      print $ "part one:"
      print $ partOne parsed
      print $ "part two:"
      print $ partTwo parsed
    Left e -> error $ show e

type Group = DS.Set Char

makeGroup :: [String] -> Group
makeGroup xs = foldr (<>) DS.empty (map DS.fromList xs)

partOne input = sum (map (DS.size . makeGroup) input)

makeGroupIntersection :: [String] -> Group
makeGroupIntersection xs = foldr1 DS.intersection (map DS.fromList xs)

partTwo input = sum (map (DS.size . makeGroupIntersection) input)

-- Parse Input
type Parser = Parsec Void Text


parseIndividual ::  Parser String
parseIndividual = do 
  r <- Text.Megaparsec.someTill letterChar (char '\n')
  return r

parseGroup :: Parser [ String ]
parseGroup = Text.Megaparsec.many parseIndividual 

parser :: Parser [ [String] ]
parser = parseGroup  `sepBy` char '\n'

-- $> Advent.Day06.main


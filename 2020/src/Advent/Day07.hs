{-# LANGUAGE RecordWildCards   #-}
{-# Language OverloadedStrings #-}
module Advent.Day07 where

import Debug.Trace
import Control.Applicative
import Control.Monad
import Data.Text (Text, pack)
import Data.List
import Data.Void
import Data.Char
import Data.Maybe
import qualified Data.Set as DS
import Text.Megaparsec as P hiding (State) 
import Text.Megaparsec.Char as P
import qualified Data.Text as T

main :: IO ()
main = do 
  file <- readFile "./Day07.txt"

  case parse parser "Day07.txt" (pack file) of
    Right parsed -> do 
      print "partone result: "
      print $ solutionOne (Colour "shiny gold") parsed
      print "parttwo result: "
      print $ solutionTwo (Colour "shiny gold") parsed
    Left e -> error $ show e
    
solutionOne :: Colour -> [Bag] -> Int
solutionOne colour bags = length $ nub $ map head $ resolveBag colour bags

resolveBag :: Colour -> [Bag] -> [[Bag]]
resolveBag colour bags = go colour bags
  where
  go :: Colour -> [Bag] -> [[Bag]]
  go colour' (bag:rest) =
    if canContain colour' bag
      then let bagColour :: Colour
               bagColour = getColour bag
           in [[bag]]
                <> zipWith (<>) (resolveBag bagColour bags) (repeat [bag])
                <> (go colour' rest) 
      else go colour' rest
  go _ [] = []

canContain :: Colour -> Bag -> Bool
canContain colour (Bag _ contents) =
  foldr (\(BagContent c _) acc -> c == colour || acc) False contents 

solutionTwo :: Colour -> [Bag] -> Int
solutionTwo colour allBags =
  let mstartingBag = find (\b -> getColour b == colour) allBags 
  in  maybe 0 (howManyBagsInThisBag allBags) mstartingBag

howManyBagsInThisBag :: [Bag] -> Bag -> Int
howManyBagsInThisBag _ (Bag _ [])  = 0
howManyBagsInThisBag allBags (Bag (Colour colour) contents) = 
  let resolvedContents = foldMap (getBagsInABag allBags) contents
  in length resolvedContents + (sum $ map (howManyBagsInThisBag allBags) resolvedContents)

getBagsInABag :: [Bag] -> BagContent -> [Bag]
getBagsInABag bags (BagContent c number) = 
  take number $ cycle $ maybeToList $ find (\b -> c == getColour b) bags 
  
-- Parse Input
type Parser = Parsec Void Text

newtype Colour = Colour String
  deriving (Eq, Show)
parseColour :: Parser Colour
parseColour = do
  c <- someTill asciiChar (string " bags contain ")
  return $ Colour c

data BagContent = BagContent Colour Int
  deriving (Eq, Show)
parseBagContentItem :: Parser BagContent
parseBagContentItem = do
  number <- read <$> P.some digitChar
  _ <- space
  colour <- someTill asciiChar (string " bag" >> optional (char 's'))
  return $ BagContent (Colour colour) number

parseEmptyBag :: Parser [ BagContent ]
parseEmptyBag = do
  _ <- string "no other bags"
  return mempty

parseBagContents :: Parser [ BagContent ]
parseBagContents = parseEmptyBag <|> (parseBagContentItem `sepBy` string ", ")
  
data Bag = Bag Colour [BagContent]
  deriving (Eq, Show)
getColour :: Bag -> Colour
getColour (Bag colour _) = colour

parseBag :: Parser Bag
parseBag = do
  colour <- parseColour
  content <- parseBagContents
  pure $ Bag colour content

parser :: Parser [Bag]
parser = parseBag  `sepEndBy` string ".\n"

-- $> Advent.Day07.main

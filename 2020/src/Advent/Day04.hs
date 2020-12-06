{-# Language GADTs #-}
{-# LANGUAGE RecordWildCards   #-}
{-# Language OverloadedStrings #-}
module Advent.Day04 where

import Debug.Trace
import Control.Applicative
import Control.Monad
import Data.Text (Text, pack)
import Data.Void
import Data.Char
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do 
  file <- readFile "./Day04.txt"
  case parse parser "Day04.txt" (pack file) of
    Right parsed -> do 
      print $ solutionOne parsed
      print '\n'
      print $ solutionTwo parsed
    Left e -> error $ show e

-- Parse Input
type Parser = Parsec Void Text


data FieldType
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColour
  | EyeColour
  | PassportID
  | CountryId
  deriving (Eq, Show)

data Field = Field
  { fieldType :: FieldType
  , value :: String
  } deriving (Eq, Show)
schemeParser :: Parser FieldType
schemeParser =  choice
  [ BirthYear <$ "byr"
  , IssueYear <$  "iyr"
  , ExpirationYear <$ "eyr"
  , Height <$ "hgt"
  , HairColour <$ "hcl"
  , EyeColour <$  "ecl"
  , PassportID <$ "pid"
  , CountryId <$ "cid"
  ]

pField :: Parser Field
pField = do
  fieldType <- schemeParser
  _ <- char ':'
  value <- Text.Megaparsec.many (alphaNumChar <|>  char '#')
  return (Field fieldType value)

spaceOrNewline :: Parser Char
spaceOrNewline = char ' ' <|> char '\n'

parseField ::  Parser Field
parseField = do 
  r <- pField
  _ <- spaceOrNewline
  return r

group :: Parser [ Field ]
group = Text.Megaparsec.many parseField

parser :: Parser [ [Field] ]
parser = group `sepEndBy` char '\n'

-- partOne
removeCid :: [Field] -> [Field]
removeCid = filter (\t -> fieldType t /= CountryId)

solutionOne ::  [[Field]] -> Int
solutionOne fields = 
  length $ filter (>= 7) $ map (length . removeCid) fields

-- parttwo


solutionTwo :: [[Field]] -> Int 
solutionTwo passports = length 
                       $ filter (\ts -> all validate ts && length ts == 7)  
                       $ map removeCid passports

validate :: Field -> Bool
validate t = case (fieldType t, value t) of
   (BirthYear, y) -> let year = read y
                in year >= 1920 && year <= 2002
   (IssueYear, y) -> let year = read y
                in year >= 2010 && year <= 2020
   (ExpirationYear, y) -> let year = read y
                     in year >= 2020 && year <= 2030
   (Height, h) -> case (last . init $ h, last h) of
                         ('i', 'n') -> let height = read $ init . init $ h 
                                       in height >= 59 && height <= 76
                         ('c', 'm') -> let height = read $ init . init $ h 
                                       in  height >= 150 && height <= 193
                         _ -> False
          
   (HairColour, ('#':hex)) -> length hex == 6 && all isHexDigit hex
   (EyeColour, colour) -> validateEyeColour colour
   (PassportID, number) -> length number == 9 && all isDigit number
   (CountryId, _) -> True
   _ -> False
  
validateEyeColour :: String -> Bool
validateEyeColour "amb" = True
validateEyeColour "blu" = True
validateEyeColour "brn" = True
validateEyeColour "gry" = True
validateEyeColour "grn" = True
validateEyeColour "hzl" = True
validateEyeColour "oth" = True
validateEyeColour _    = False

 

-- $> 

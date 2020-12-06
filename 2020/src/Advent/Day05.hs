module Advent.Day05 where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import Data.Void
import Data.List

main :: IO ()
main = do

  putStrLn "should equal 357"
  putStrLn $ show $ makeSeatId (Row 44) (Column 5)

  putStrLn "should equal 567, 119, 820"
  case parse parser "" (pack "BFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL\n")
    of Right p -> putStrLn $ show $ map getSeatId p
       Left _ -> print "oh no"

  file <- readFile "./Day05.txt"
  case parse parser "Day04.txt" (pack file) of
       Right p -> do
         putStrLn $ show $ solutionOne p
         putStrLn $ show $ solutionTwo p
       Left  _ -> print "oh no"

solutionTwo :: [SeatData] -> Int
solutionTwo seatData =
  let sortedSeats = sort $ map getSeatId seatData
      pairs = zip sortedSeats (tail sortedSeats)
      pair  = find (\(lowerId, higherId) -> higherId - 2 == lowerId) pairs
  in case pair of Just (oneBelowMe, _) -> oneBelowMe + 1
                  _ -> error "found no space!"


solutionOne :: [SeatData] -> Int
solutionOne seatData =
                   let seatIds = map getSeatId seatData
                   in maximum seatIds

getSeatId :: SeatData -> Int
getSeatId (SeatData (rowData, columnData)) =
  makeSeatId (getRow rowData) (getColumn columnData)

makeSeatId :: Row -> Column -> Int
makeSeatId (Row r) (Column c) = (r * 8) + c

getRow :: [RowData] -> Row
getRow rowData = Row $ go 0 127 rowData
  where go min max [] = max
        go min max (F:rows) = go min (min + ((max - min) `div` 2))  rows
        go min max (B:rows) = go (min + ((max - min) `div` 2)) max rows

getColumn :: [ColumnData] -> Column
getColumn columnData = Column $ go 0 7 columnData
  where go min max [] = max
        go min max (L:columns) = go min (min + ((max - min) `div` 2))  columns
        go min max (R:columns) = go (min + ((max - min) `div` 2)) max columns

newtype Row = Row Int
newtype Column = Column Int

data RowData = F | B
data ColumnData = L | R
newtype SeatData = SeatData ( [RowData], [ColumnData] )

-- Parsing
rowParser :: Parser RowData
rowParser = choice [ F <$ char 'F', B <$ char 'B' ]
columnParser :: Parser ColumnData
columnParser = choice [ L <$ char 'L', R <$ char 'R' ]
seatDataParser :: Parser SeatData
seatDataParser = do
  rows <- many rowParser
  columns <- many columnParser
  _ <- char '\n'
  return $ SeatData (rows, columns)

parser :: Parser [SeatData]
parser = many seatDataParser

type Parser = Parsec Void Text

-- $> Advent.Day05.main

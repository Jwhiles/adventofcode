{-# Language OverloadedStrings #-}
import Data.Array
import System.IO
import Data.List
import qualified Data.Text as DT
import Core

parser :: Parser (Array Int Int)
parser txt = toListArr $ fmap (read . DT.unpack) $ DT.splitOn "," txt

data Position = Position { getPosition :: Int }

initialPosition :: Position
initialPosition = Position 0

nextPosition :: Instruction -> Position -> Position
nextPosition (Finish) pos = 
  Position $ getPosition pos + 4
nextPosition (Add _ _ _) pos = 
  Position $ getPosition pos + 4
nextPosition (Mult _ _ _) pos = 
  Position $ getPosition pos + 4
nextPosition (Save _) pos = 
  Position $ getPosition pos + 2
nextPosition (Output _) pos = 
  Position $ getPosition pos + 2


data Instruction =
  Finish | Add Int Int Int | Mult Int Int Int | Save Int | Output Int


getInstruction :: Position -> Array Int Int  -> Instruction
getInstruction position input =
  let pos = getPosition position
  in case (reverse $ tails $ show $ input ! pos) !! 2 of
    "99" -> Finish

    "01" ->
      Add
        (input ! (input ! (pos + 1)))
        (input ! (input ! (pos + 2))) 
        (input ! (pos + 3))

    "02" ->
      Mult
        (input ! (input ! (pos + 1))) 
        (input ! (input ! (pos + 2))) 
        (input ! (pos + 3))

    "03"  ->
      Save
        (input ! (input ! pos + 1))

    "04"  ->
      Output
        (input ! (input ! pos + 1))
    other ->
      error ("Found invalid op code: " <> show other <> ". At position: " <> show pos)


handleInstruction :: Instruction -> Array Int Int -> Position -> Array Int Int
handleInstruction Finish input pos =
  input

handleInstruction (Add x y loc) input pos =
  step (nextPosition pos) $ input // [(loc, x + y)]

handleInstruction (Mult x y loc) input pos =
  step (nextPosition pos) $ input // [(loc, x * y)]


step :: Position -> Array Int Int -> Array Int Int
step pos input =
  let instruction = getInstruction pos input
  in handleInstruction instruction input pos

run :: Array Int Int -> Array Int Int
run =
  step initialPosition

-- Noun Verb Res
data Result = Result Int Int Int
  deriving Show

toListArr xs = 
  let end = Data.List.length xs - 1
  in listArray (0, end) xs

main :: IO ()
main = do
  input <- getInput parser "./day02.txt"

  -- PART ONE
  let resultOne =  run $ input  // [(1, 12), (2, 2)]
  print $ resultOne ! 0

  -- PART TWO
  let nounVerbPairs = [(noun, verb) | noun <- [1..100], verb <- [1..100]]

  let possibilities =
        fmap (\(noun, verb) ->     
            let result = run $ input // [(1, noun), (2, verb)]
            in Result noun verb (result ! 0) ) nounVerbPairs


  let resultTwo = find (\(Result n v x) -> x == 19690720) possibilities

  print resultTwo

-- $> main

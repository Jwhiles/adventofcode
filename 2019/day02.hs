{-# Language OverloadedStrings #-}
import Data.Array
import System.IO
import Data.List
import qualified Data.Text as DT
import Debug.Trace



data Position = Position { getPosition :: Int }

initialPosition :: Position
initialPosition = Position 0

nextPosition :: Position -> Position
nextPosition pos = Position $ getPosition pos + 4

data Instruction =
  Finish | Add Int Int Int | Mult Int Int Int


getInstruction :: Position -> Array Int Int  -> Instruction
getInstruction position input =
  let pos = getPosition position
  in case input ! pos of
    99 -> Finish

    1 ->
      Add
        (input ! (input ! (pos + 1)))
        (input ! (input ! (pos + 2))) 
        (input ! (pos + 3))

    2 ->
      Mult
        (input ! (input ! (pos + 1))) 
        (input ! (input ! (pos + 2))) 
        (input ! (pos + 3))

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

start :: Array Int Int -> Array Int Int
start = 
  step initialPosition

-- Noun Verb Res
data Result = Result Int Int Int
  deriving Show

toListArr xs = 
  let end = Data.List.length xs - 1
  in listArray (0, end) xs

main :: IO ()
main = do
  inputText <- DT.pack <$> readFile "./day02.txt"
  let parsed =
        toListArr $ fmap (read . DT.unpack) $ DT.splitOn "," inputText


  -- PART ONE
  let resultOne =  start $ parsed  // [(1, 12), (2, 2)]
  print $ resultOne ! 0


  -- PART TWO
  let nounVerbPairs = [1..100] >>= (\n ->
                        let verbs = [1..100]
                        in fmap (\v -> (n,v)) verbs )

  let possibilities =
        fmap (\(noun, verb) ->     
            let result = start $ parsed // [(1, noun), (2, verb)]
            in Result noun verb (result ! 0) ) nounVerbPairs

  let resultTwo = find (\(Result n v x) -> x == 19690720) possibilities

  print resultTwo

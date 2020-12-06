{-# Language NamedFieldPuns #-}
{-# Language RecordWildCards #-}
module Advent.Core where
import Data.Text (Text, pack)

type Parser i = Text -> i

getInput :: Parser input -> String -> IO input
getInput parser path =
  parser <$> pack <$> readFile path

data Harness i o = Harness
  { parse   :: Parser i
  , filePath :: String
  , solution :: i -> o
  , exampleInput :: [ i ]
  , expectedOutput :: [ o ]
  }

runHarness :: (Show i, Show o, Eq o) => Harness i o -> IO o
runHarness Harness {..} = do
  input <- getInput parse filePath



  let x = zipWith (\input expected ->
              let result = solution input
              in if result == expected
                  then show input <> " was ok. \n got " <> show expected
                  else error $
                    "failed on example: " <> show input <>
                    "\n got: " <> show result <>
                    "\n but expected: " <> show expected)
                    exampleInput expectedOutput

  traverse putStrLn x

  pure $ solution input



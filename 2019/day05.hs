{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language NamedFieldPuns #-}
module Day05 where


import Control.Monad.Trans.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as DT

newtype Machine a = 
  Machine { runMachine :: State MachineState a }
  deriving (Functor, Applicative, Monad)

data MachineState =
  MachineState
    { position :: Int
    , program :: Map Int Int
    }

loadTape :: [Int] -> Machine ()
loadTape tape = Machine $
  modify $ \m -> m { program = M.fromList (zip [0..] tape) }

-- evalMachine :: Machine () -> Map Int Int
-- evalMachine (Machine { runMachine }) = program $ execState runMachine M.empty


-- I need to use string because of the frickin opcode stuff
parseProgram :: Text -> [String]
parseProgram txt = DT.unpack <$> DT.splitOn "," txt
  

-- THIS IS LARGELY COPIED FROM FRASER
-- https://github.com/intolerable/advent19/blob/master/src/Advent/Intcode.hs

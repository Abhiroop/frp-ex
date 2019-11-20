module Prog1 where

import Control.SF

type Index     = Int
type EvenIndex = Int
type OddIndex  = Int

indexSignal :: SF Int (Index, Int)
indexSignal = scanrS (\x (i,_) -> (i + 1, x)) (-1, -1)

split :: SF (Index, Int)  ((EvenIndex,Int), (OddIndex, Int))
split = (filterS (\(index, val) -> even index)) &&& (filterS (\(index, val) -> odd index))

removeIndices :: SF  ((EvenIndex,Int), (OddIndex, Int)) (Int, Int)
removeIndices = mapS (\((_, a), (_, b)) -> (a, b))

finalProgram :: SF Int (Int, Int)
finalProgram = indexSignal >>> split >>> removeIndices

program :: Run Int (Int, Int) ()
program = liftSignal finalProgram [ch1] [ch2]
  where
    ch1 = ReadInt
    ch2 = WriteIntInt

_main :: IO ()
_main = do
  run program initBuffers
  return ()

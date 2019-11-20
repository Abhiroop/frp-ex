module Control.SF
( SF
, (>>>)
, (&&&)
, mapS
, scanrS
, filterS
, liftSignal
, InputChannel (..)
, OutputChannel (..)
, Run
, initBuffers
, run
  )
where

import Control.Arrow
import Control.Category
import Control.Monad.State.Lazy as S

import Data.Dynamic
import Data.Vector

type Signal a = a

newtype SF a b = SF {sf :: Signal a -> Signal b}

instance Category SF where
  id  = SF {sf = Prelude.id}
  (SF f) . (SF g) = SF {sf = f Prelude.. g}


instance Arrow SF where
  arr = SF
  (SF f) *** (SF g) = SF (\(b,b') -> (f b, g b'))

type InputBuffer a  = [Dynamic]

data IOBuffer a b = IOBuffer { input  :: Vector (InputBuffer a)}

initBuffers :: (Typeable a, Typeable b) => IOBuffer a b
initBuffers = IOBuffer {input = empty}

type Run a b c = StateT (IOBuffer a b) IO c

run = runStateT

mapS :: (a -> b) -> SF a b
mapS = arr

scanrS :: (a -> b -> b) -> b -> SF a b
scanrS f init = SF {sf = sf'}
  where
    sf' = \val -> f val init

filterS :: (a -> Bool) -> SF a a
filterS pred = SF {sf = sf'}
 where
   sf' = \val -> if pred val
                 then val
                 else undefined

-- This is the synchronous logical clock
logicalTimeStep = 2

-- push based implementation
liftSignal :: ( Typeable a
              , Typeable b
              , Show b -- for demonstration purposes
              )
           => SF a b
           -> [InputChannel]
           -> [OutputChannel]
           -> Run a b ()
liftSignal (SF sf) [inpCh] [outpCh]
  | inpCh == ReadInt && outpCh == WriteIntInt
  = do
      -- setup buffers
      inp <- gets input
      let iBuf0 = inp ! 0
      x <- lift $ getLine
      let cast = read x :: Int
      if Prelude.length iBuf0 < logicalTimeStep
      then do
        let foo = toDyn cast : iBuf0
        S.modify (\s -> s {input = fromList $ [foo]})
        liftSignal (SF sf) [inpCh] [outpCh]
      else do
        let (a:b:[]) = iBuf0
        let (Just a_int) = fromDynamic a
        let (Just b_int) = fromDynamic b
        let ret  = sf a_int
        let ret2 = sf b_int
        lift $ print ret
        S.modify (\s -> s {input = fromList $ []})
        liftSignal (SF sf) [inpCh] [outpCh]
liftSignal sf xs ys = undefined

data InputChannel
  = ReadInt
  | ReadBool
  | ReadIntInt
  | ReadIntBool
  | ReadBoolInt
  | ReadBoolBool
  deriving Eq

data OutputChannel
  = WriteInt
  | WriteBool
  | WriteIntInt
  | WriteIntBool
  | WriteBoolInt
  | WriteBoolBool
  deriving Eq

defaultValue = minBound :: Int

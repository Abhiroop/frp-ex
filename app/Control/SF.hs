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

  )
where

import Control.Arrow
import Control.Category

type T = Double

type Signal a = a

newtype SF a b = SF {sf :: Signal a -> Signal b}

instance Category SF where
  id  = SF {sf = Prelude.id}
  (SF f) . (SF g) = SF {sf = f Prelude.. g}


instance Arrow SF where
  arr = SF
  (SF f) *** (SF g) = SF (\(b,b') -> (f b, g b'))


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

liftSignal :: SF a b -> [InputChannel] -> [OutputChannel] -> IO ()
liftSignal sf inpChs outpChs = undefined

data InputChannel
  = ReadInt
  | ReadBool
  | ReadIntInt
  | ReadIntBool
  | ReadBoolInt
  | ReadBoolBool

data OutputChannel
  = WriteInt
  | WriteBool
  | WriteIntInt
  | WriteIntBool
  | WriteBoolInt
  | WriteBoolBool

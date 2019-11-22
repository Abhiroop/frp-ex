{-# LANGUAGE TypeSynonymInstances #-}

module FRP where

import Control.Category
import Control.Arrow

type Signal a = a

newtype SF a b = SF { sf :: Signal a -> Signal b}

instance Category SF where
  id  = SF {sf = Prelude.id}
  (SF f) . (SF g) = SF {sf = f Prelude.. g}

instance Arrow SF where
  arr = SF
  (SF f) *** (SF g) = SF (\(b,b') -> (f b, g b'))

map :: (a -> b) -> Signal a -> Signal b
map f a = f a

scan :: (s -> a -> b) -> (s -> s) -> s -> Signal a -> (s, Signal b)
scan f sf init var = (sf init , f init var)






----------------------------------------------------

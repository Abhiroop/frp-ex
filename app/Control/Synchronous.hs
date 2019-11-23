{-# LANGUAGE FlexibleInstances #-}

module Synchronous where

import Control.Applicative
import qualified Data.Bits as B

import Prelude hiding (not, or)

type Clock = Bool

defaultClkValue = False

newtype Stream a =
  Stream
    { st :: [(a, Bool)]
    }

instance Functor Stream where
  fmap f s = Stream $ fmap (\(val, clk) -> (f val, clk)) (st s)

class Nil t where
  nil :: t

-- writing instances for the base types to
-- avoid unnecessary use of undecidable instances
instance Nil Int where
  nil = minBound

instance Nil Bool where
  nil = minBound

instance (Bounded a, Nil a) => Nil (a, Clock) where
  nil = (minBound, minBound)

fby :: Nil a => Stream a -> Stream a -> Stream a
fby s1 s2 = Stream $ go (st s1) (st s2)
  where
    go (x:_) ys = x : ys -- s ->> pre ys

pre :: (Bounded a, Nil a) => Stream a -> Stream a
pre xs = Stream $ nil : st xs

(->>) :: Stream a -> Stream a -> Stream a
s1 ->> s2 = Stream $ go (st s1) (st s2)
  where
    go (x:_) (_:ys) = x : ys

arr :: (a -> b) -> Stream a -> Stream b
arr = fmap

-- assuming clk1 and clk2 are the same
zipWSameClk :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWSameClk f s1 s2 =
  Stream $
  zipWith (\(val1, clk1) (val2, clk2) -> (f val1 val2, clk1)) (st s1) (st s2)

-- Example
xor :: Stream Bool -> Stream Bool -> Stream Bool
xor = zipWSameClk B.xor

(&) :: Stream Bool -> Stream Bool -> Stream Bool
(&) = zipWSameClk (B..&.)

or :: Stream Bool -> Stream Bool -> Stream Bool
or = zipWSameClk (B..|.)

not :: Stream Bool -> Stream Bool
not = fmap B.complement

false :: Stream Bool
false = Stream $ repeat (False, defaultClkValue)

true :: Stream Bool
true = Stream $ repeat (True, defaultClkValue)

full_adder a b c = (s, co)
  where
    s = (a `xor` b) `xor` c
    co = (a & b) `or` (b & c) `or` (a & c)

edge c = false ->> c & not (pre c)

module Synchronous where

import Control.Applicative
import qualified Data.Bits as B

import Prelude hiding (or, not)

type Clock = Bool

type Stream a = [a]

class Nil t where
  nil :: t

instance Nil Int where
  nil = minBound
instance Nil Bool where
  nil = minBound

fby :: Nil a => Stream a -> Stream a -> Stream a
fby (x:_) ys = x : ys -- s ->> pre ys

pre :: Nil a => Stream a -> Stream a
pre xs = nil : xs

(->>) :: Stream a -> Stream a -> Stream a
(x : _) ->> (_ : ys) = x : ys

arr :: (a -> b) -> Stream a -> Stream b
arr = fmap

zipW :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipW = zipWith

-- Example
xor :: Stream Bool -> Stream Bool -> Stream Bool
xor = zipWith B.xor

(&) :: Stream Bool -> Stream Bool -> Stream Bool
(&) = zipWith (B..&.)

or :: Stream Bool -> Stream Bool -> Stream Bool
or = zipWith (B..|.)

not :: Stream Bool -> Stream Bool
not = map B.complement

false :: Stream Bool
false = repeat False

true :: Stream Bool
true = repeat True

full_adder a b c = (s, co)
  where
    s = (a `xor` b) `xor` c
    co = (a & b) `or` (b & c) `or` (a & c)

edge c = false ->> c & not (pre c)


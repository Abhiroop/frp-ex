module Synchronous where

import Control.Applicative
import qualified Data.Bits as B

import Prelude hiding (not, or, sum)

type Clock = [Bool]

defaultClkValue = False

newtype Stream a =
  Stream
    { st :: [a]
    }

instance Show a => Show (Stream a) where
  show = show . st

instance Functor Stream where
  fmap f = Stream . fmap f . st

instance Num a => Num (Stream a) where
  (+) = zipWSameClk (+)
  (*) = zipWSameClk (*)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum


class Nil t where
  nil :: t

instance Nil Int where
  nil = minBound

instance Nil Bool where
  nil = minBound

fby :: Nil a => Stream a -> Stream a -> Stream a
fby s1 s2 = Stream $ go (st s1) (st s2)
  where
    go (x:_) ys = x : ys -- s ->> pre ys

pre :: Nil a => Stream a -> Stream a
pre xs = Stream $ nil : st xs

(->>) :: Stream a -> Stream a -> Stream a
s1 ->> s2 = Stream $ go (st s1) (st s2)
  where
    go (x:_) (_:ys) = x : ys

arr :: (a -> b) -> Stream a -> Stream b
arr = fmap

-- -- assuming clk1 and clk2 are the same
zipWSameClk :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWSameClk f s1 s2 =
  Stream $ zipWith f (st s1) (st s2)

when :: Stream a -> Clock -> Stream a
when (Stream s) = Stream . fmap fst . filter ((True ==) . snd) . zip s

whenot :: Stream a -> Clock -> Stream a
whenot (Stream s) = Stream . fmap fst . filter ((False ==) . snd) . zip s

-- complementary clocks
merge :: Clock -> Stream a -> Stream a -> Stream a
merge clk (Stream s1) (Stream s2) = Stream $ go clk s1 s2
  where
    go [] _ _ = []
    go cs [] yss = go' cs yss
    go cs xss [] = go' cs xss
    go (c : cs) xss@(x : xs) yss@(y : ys) =
      if c == True
      then x : go cs xs yss
      else y : go cs xss ys

    go' [] _ = []
    go' (c:cs) (s:ss) =
      if c == True
      then s : go' cs ss
      else go' cs ss

-- ---------------- Example -----------------------
xor :: Stream Bool -> Stream Bool -> Stream Bool
xor = zipWSameClk B.xor

(&) :: Stream Bool -> Stream Bool -> Stream Bool
(&) = zipWSameClk (B..&.)

or :: Stream Bool -> Stream Bool -> Stream Bool
or = zipWSameClk (B..|.)

not :: Stream Bool -> Stream Bool
not = fmap B.complement

false :: Stream Bool
false = Stream $ repeat False

true :: Stream Bool
true = Stream $ repeat True

full_adder a b c = (s, co)
  where
    s = (a `xor` b) `xor` c
    co = (a & b) `or` (b & c) `or` (a & c)

edge c = false ->> c & not (pre c)

sum x = s
  where
    s = x ->> pre s + x

sampled_sum x c = sum (x `when` c)

sampleStream :: Stream Int
sampleStream = Stream [5, 2, 3, 6, 7]

clk :: [Bool]
clk = [False, True, False, False, False, True]

xs :: Stream Int
xs = Stream [0, 1]

ys :: Stream Int
ys = Stream [100, 102, 101,103]

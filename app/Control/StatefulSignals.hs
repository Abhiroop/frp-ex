{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module StatefulSignals where

import Control.Arrow
import Control.Category

type Signal s a = (s, a)

newtype SF s a b = SF {sf :: Signal s a -> Signal s b}

instance Category (SF s) where
  id = SF {sf = Prelude.id}
  (SF f) . (SF g) = SF {sf = f Prelude.. g}


instance Arrow (SF s) where
  arr f = SF {sf = sf'}
    where
      sf' = \(s,b) -> (s,f b)
  (SF f) *** (SF g) = SF {sf = sf'}
    where
      sf' = \(s, (b, b')) -> let (_,c) = f (s, b)
                                 (_,c') = g (s, b')
                              in (s, (c, c'))

mapS :: (a -> b) -> SF s a b
mapS = arr









--scan r

--        value read       transform state
--    old    |       initial    |
--    state  |        state     |
--      |    |          |    ________
--      v    v          v    vvvvvvvv
foo :: (s -> a -> b) -> s -> (s -> s)-> [a] -> [(s,b)]
foo f init sfunc ls = go ls [] init
  where
    go [] fs _ = fs
    go (x:xs) fs state =
      let newState = sfunc state
       in go xs ((newState, f newState x):fs) newState 

bar :: (s -> a -> b) -> s -> (s -> s)-> a -> (s, b)
bar f init sfunc a =
  let newState = sfunc init
   in bar f newState sfunc a


test :: Int -> IO ()
test s = do
  x <- getLine
  let cast = read x :: Int
  let (newS, b) = state (\s a -> s * a) (+ 1) s cast
  print b
  test newS

state :: (s -> a -> b) -> (s -> s) -> s -> a -> (s, b)
state f sf init var = (sf init , f init var)

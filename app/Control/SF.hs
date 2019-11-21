{-# LANGUAGE FlexibleInstances #-}
module Control.SF
( SF
, (>>>)
, (&&&)
, (~&&~)
, mapS
, scanrS
, filterS
, liftSignal
, InputChannel (..)
, OutputChannel (..)
, Run
, run
  )
where

import Control.Arrow
import Control.Category
import Control.Monad.State.Lazy as S

import Data.Dynamic
import Data.Vector

import Debug.Trace

type Signal a = a

newtype SF a b = SF {sf :: Signal a -> Signal b}

instance Category SF where
  id  = SF {sf = Prelude.id}
  (SF f) . (SF g) = SF {sf = f Prelude.. g}

instance Arrow SF where
  arr = SF
  (SF f) *** (SF g) = SF (\(b,b') -> (f b, g b'))

type InputBuffer a  = [Dynamic]
type OutputBuffer a = [Either Dynamic Dynamic]

data IOBuffer a b = IOBuffer { input  :: Vector (InputBuffer a)
                             , output :: Vector (OutputBuffer a)}

initBuffers :: IOBuffer a b
initBuffers = IOBuffer {input = empty, output = empty}

type Run a b c = StateT (IOBuffer a b) IO c

run = flip runStateT initBuffers

mapS :: (a -> b) -> SF a b
mapS = arr

scanrS :: (a -> b -> b) -> b -> SF a b
scanrS f init = SF {sf = sf'}
  where
    sf' = \val -> f val init

filterS :: Nil a => (a -> Bool) -> SF a a
filterS pred = SF {sf = sf'}
 where
   sf' = \val -> if pred val
                 then val
                 else nil

(~&&&~) :: Nil b => SF a b -> SF a c -> SF a (Either b c)
(SF f) ~&&&~ (SF g) = SF sf'
  where
    sf' = \a -> if f a == nil
                then Left $ f a
                else Right $ g a

(~&&~) :: (Nil b, Nil c)
       => (a -> Bool) -> SF a b -> SF a c -> SF a (b, c)
(~&&~) pred (SF f) (SF g) = SF sf'
  where
    sf' = \a -> if pred a
                then trace "in first" $ (f a, nil)
                else trace "in second" $ (nil, g a)


class Eq t => Nil t where
  nil :: t

instance Nil Int where
  nil = minBound
instance Nil Bool where
  nil = False
instance Nil (Int, Int) where
  nil = (nil , nil)

-- This is the synchronous logical clock
logicalTimeStep = 2


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
      x <- lift $ getLine
      let cast = read x :: Int
      let (Just cast_int) = fromDynamic $ toDyn cast
      let ret = sf cast_int
      let (Just (pr1, pr2)) = fromDynamic $ toDyn ret :: Maybe (Int, Int)
      if (pr1 == nil) && (pr2 == nil)
      then liftSignal (SF sf) [inpCh] [outpCh]
      else do
        out <- gets output
        let oBuf0 = out ! 0
        if Prelude.length out == logicalTimeStep
        then do
          let (a:b:[]) = oBuf0
          case a of
            Left a_l -> let (Right b_r) = b
                            Just a_int = fromDynamic a_l :: Maybe Int
                            Just b_int = fromDynamic b_r :: Maybe Int
                         in lift $ print (a_int, b_int)
            Right a_r -> let (Left b_l) = b
                             Just a_int = fromDynamic a_r :: Maybe Int
                             Just b_int = fromDynamic b_l :: Maybe Int
                          in lift $ print (b_int, a_int)
          S.modify (\s -> s {output = fromList $ []})
          --- Repetition of the else branch-------------
          if (pr2 == nil)
          then do
            let foo = Left (toDyn pr1) : oBuf0
            S.modify (\s -> s {output = fromList $ [foo]})
            liftSignal (SF sf) [inpCh] [outpCh]
          else if (pr1 == nil)
               then do
                 let bar = Right (toDyn pr2) : oBuf0
                 S.modify (\s -> s {output = fromList $ [bar]})
                 liftSignal (SF sf) [inpCh] [outpCh]
               else do -- normal pair received -- don't buffer
                 lift $ print (pr1, pr2)
                 liftSignal (SF sf) [inpCh] [outpCh]
          --Repetition of the else branch ends ---------
        else if (pr2 == nil)
             then do
               let foo = Left (toDyn pr1) : oBuf0
               S.modify (\s -> s {output = fromList $ [foo]})
               trace ("hello" <> show pr1) $ liftSignal (SF sf) [inpCh] [outpCh]
             else if (pr1 == nil)
                  then do
                    let bar = Right (toDyn pr2) : oBuf0
                    S.modify (\s -> s {output = fromList $ [bar]})
                    trace ("goodbye" <> show pr2) $ liftSignal (SF sf) [inpCh] [outpCh]
                  else do -- normal pair received -- don't buffer
                    lift $ print (pr1, pr2)
                    liftSignal (SF sf) [inpCh] [outpCh]
      -- inp <- gets input
      -- if Data.Vector.null inp
      -- then do
      --   let foo = [toDyn cast]
      --   S.modify (\s -> s {input = fromList $ [foo]})
      --   liftSignal (SF sf) [inpCh] [outpCh]
      -- else do
      --   let iBuf0 = inp ! 0
      --   if Prelude.length iBuf0 < logicalTimeStep
      --   then do
      --     let foo = toDyn cast : iBuf0
      --     S.modify (\s -> s {input = fromList $ [foo]})
      --     liftSignal (SF sf) [inpCh] [outpCh]
      --   else do
      --     -- push happens here
      --     let (a:b:[]) = iBuf0
      --     let (Just a_int) = fromDynamic a
      --     let (Just b_int) = fromDynamic b
      --     let ret  = sf a_int
      --     let ret2 = sf b_int
      --     lift $ print ret
      --     S.modify (\s -> s {input = fromList $ []})
      --     liftSignal (SF sf) [inpCh] [outpCh]
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

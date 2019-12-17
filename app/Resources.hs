{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds,
             TypeFamilies, TypeOperators, UndecidableInstances #-}
module Resources where

import Data.Type.Bool
import GHC.TypeLits
import Type.Set



type family Singleton (t :: k) :: TypeSet k where
  Singleton s = Insert s Empty

type family AnyIntersection (s1 :: TypeSet k) (s2 :: TypeSet k) :: Bool where
  AnyIntersection Empty _ = False
  AnyIntersection _ Empty = False
  AnyIntersection (Branch a l r) s =
    a `Member` s || (AnyIntersection l s) || (AnyIntersection r s)

type family DisjointUnion (s1 :: TypeSet k) (s2 :: TypeSet k) :: TypeSet k where
  DisjointUnion Empty s = s
  DisjointUnion s Empty = s
  DisjointUnion s1 s2 =
    If (AnyIntersection s1 s2)
    (TypeError (Text "Trying to compose same resources"))
    (Merge s1 s2)

type family EqSet (s1 :: TypeSet k) (s2 :: TypeSet k) :: Bool where
  EqSet Empty Empty = True
  EqSet Empty _     = False
  EqSet _ Empty     = False
  EqSet (Branch a l1 r1) (Branch a l2 r2) = (EqSet l1 l2) && (EqSet r1 r2)
  EqSet (Branch a _ _) (Branch b _ _) = False


type family IsEmpty (s :: TypeSet k) :: Bool where
  IsEmpty Empty = True
  IsEmpty _     = False


class Arrow a where
  arr :: (b -> c) -> a Empty b c
  first :: a r b c -> a r (b,d) (c,d)
  (>>>) :: a r1 b c -> a r2 c d -> a (DisjointUnion r1 r2) b d

class Arrow a => ArrowChoice a where
  unsafeCompose :: a r1 b c -> a r2 c d -> a (Merge r1 r2) b d

data SFM m r a b = SFM
  {sfmFun :: a -> m (b, SFM m r a b)}

instance Monad m => Arrow (SFM m) where
  arr f = SFM h
    where
      h x = return (f x, SFM h)

  first (SFM f) = SFM (h f)
    where
      h f (x,z) = do
        (y, SFM f') <- f x
        return ((y,z), SFM (h f'))

  SFM f >>> SFM g = SFM (h f g)
    where
      h f g x = do
        (y, SFM f') <- f x
        (z, SFM g') <- g y
        return (z, SFM (h f' g'))

instance Monad m => ArrowChoice (SFM m) where
  SFM f `unsafeCompose` SFM g = SFM (h f g)
    where
      h f g x = do
        (y, SFM f') <- f x
        (z, SFM g') <- g y
        return (z, SFM (h f' g'))



type SF = SFM IO

source :: IO c -> SF (Singleton r) () c
source action = SFM g
  where
    g = \_ -> do
      c <- action
      return (c, SFM g)

sink :: (b -> IO ()) -> SF (Singleton r) b ()
sink cont = SFM g
  where
    g = \b -> do
      cont b
      return ((), SFM g)


pseudoMain :: Monad m => SFM m r () b -> m ()
pseudoMain x = sfmFun x () >> return ()


data Cmd

foo :: SF (Singleton Cmd) () String
foo = source getLine

bar :: SF (Singleton Cmd) String ()
bar = sink (\b -> putStrLn $ "Hello " ++ b)

{-

• Trying to compose same resources

baz :: SF (Merge (Singleton Cmd) (Singleton Cmd)) () ()
baz = foo >>> bar
-}

baz :: SF (Merge (Singleton Cmd) (Singleton Cmd)) () ()
baz = foo `unsafeCompose` bar


run = sfmFun baz () >> return ()

-- or

run1 = pseudoMain baz

--------------------------------------------
-- Test 2 --
step1 :: SF Empty Int Int
step1 = arr (+ 1)


step2 :: SF Empty Int Int
step2 = arr (* 5)

getInt :: IO Int -- () -> Int
getInt = fmap (\x -> read x :: Int) getLine

newStep :: SF (Singleton Cmd) () Int
newStep = source getInt

printer :: SF (Singleton Cmd) Int ()
printer = sink (\x -> putStrLn $ show x)

finalProg :: SF (Merge (Singleton Cmd) Empty) () ()
finalProg = newStep >>> step1 >>> step2 `unsafeCompose` printer

finalRun = sfmFun finalProg () >> return ()
-----------------------------------------------
{-
WAP which accepts a stream and emits a tuple with odd indexed element in the
first projection and even indexed element in the second projection
-}







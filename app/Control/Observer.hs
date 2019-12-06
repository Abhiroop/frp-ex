module Control.Observer where

import Control.Exception

type Observable a = IO a

newtype Cont a r = Cont {cont :: (a -> r) -> r}

runCont :: Cont a r -> (a -> r) -> r
runCont c = cont c

foo :: [Int] -> Int -> IO ()
foo xs c = do
  x <- getLine
  let xr = read x :: Int
  if (c == 0)
  then do
    foo [xr] (c + 1)
  else if (c `mod` 2 == 0)
       then do
         let m:n:_ = reverse xs
         print(m,n)
         foo [xr] (c + 1)
       else do
         foo (xr:xs) (c+1)

{-

foo =
  let x = getInput : String
   in (x, x, x) : (String, String, String)

bar :: (String, String, String) ->Int
bar (s1, s2, s3) = s1 * 2 + s2*4 + s3

-}

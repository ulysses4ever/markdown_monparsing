{-# LANGUAGE FlexibleContexts #-}
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Lazy
import Control.Monad (void)
import Data.Typeable

-- Write the elements of a list of numbers, in order.
writeAll :: (Typeable a, Member (Writer a) e)
          => [a]
          -> Eff e ()
writeAll = mapM_ putWriter -- tell

-- Add a list of numbers to the current state.
sumAll :: (Typeable a, Num a, Member (State a) e)
       => [a]
       -> Eff e ()
sumAll = mapM_ (onState . (+))

-- Write a list of numbers and add them to the current state.
writeAndAdd :: (Member (Writer Integer) e, Member (State Integer) e)
            => [Integer]
            -> Eff e ()
writeAndAdd l = do
   writeAll l
   sumAll l

-- Sum a list of numbers.
sumEff :: (Num a, Typeable a) => [a] -> a
sumEff l = let (s, ()) = run $ runState 0 $ sumAll l
          in s

-- Safely get the last element of a list.
-- Nothing for empty lists; Just the last element otherwise.
lastEff :: Typeable a => [a] -> Maybe a
lastEff l = let (a, ()) = run $ runWriter $ writeAll l
           in a

-- Get the last element and sum of a list
lastAndSum :: (Typeable a, Num a) => [a] -> (Maybe a, a)
lastAndSum l = let (lst, (total, ())) = run $ runWriter $ runState 0 $ writeAndAdd l
              in (lst, total)
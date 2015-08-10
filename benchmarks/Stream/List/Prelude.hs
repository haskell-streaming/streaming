{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Stream.List.Prelude 
  ( -- concats, 
   cons, 
   drop, 
   filter,
   -- filterM,
   foldl',
   yield,
   iterate,
   -- iterateM,
   map,
   -- mapM,
   repeat,
   -- repeatM,
   replicate,
   scanr,
   -- span, 
   -- splitAt, 
   sum,
   take,
   takeWhile,
   enumFromStepN
   ) where
import Stream.Types
import qualified Stream.Folding.Prelude as F
import Control.Monad hiding (filterM, mapM)
import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Free (FreeT)
import qualified System.IO as IO
import Pipes hiding (yield)
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo
                      , mapM, scanr, span)


-- ---------------
-- ---------------
-- Prelude
-- ---------------
-- ---------------

-- ------
-- concats
-- ------

-- 
-- concats :: Monad m =>  FreeT (Producer a m) m r -> [a]
-- concats  = buildList 
--                 . F.concats 
--                 . (\(Folding phi) -> 
--                        Folding (\c w d -> phi (c . foldList) w d))
--                 . foldFreeT
-- {-# INLINE concats #-}


-- ------
-- cons
-- ------
cons :: a -> [a] -> [a] 
cons a = buildList . F.cons a . foldList
{-# INLINE cons #-}


-- ------
-- yield
-- ------
yield :: a -> [a]
yield = buildList . F.yield
{-# INLINE yield #-}

-- -------
-- foldl'
-- -------
foldl' :: forall a b . (b -> a -> b) -> b -> [a] -> b
foldl' op b0 = runIdentity . F.foldl op b0 . foldList 
{-# INLINE foldl' #-}

-- -------
-- scanr
-- -------

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr op b = buildList 
           . F.scanr op b
           . foldList 
{-# INLINE scanr #-}



-- ---------------
-- sum 
-- ---------------

sum :: Num n =>  [n] -> n
sum  = runIdentity . F.sum . foldList 
{-# INLINE sum #-}


-- ---------------
-- replicate 
-- ---------------

replicate ::  Int -> a -> [a]
replicate n = take n . repeat
{-# INLINE replicate #-}

-- replicateM :: Monad m => Int -> m a -> [a]
-- replicateM n a = buildList (F.replicateM n a)
-- {-# INLINE replicateM #-}

-- ---------------
-- iterate
-- ---------------

iterate :: (a -> a) -> a -> [a]
iterate f  = buildList . F.iterate f 
{-# INLINE iterate #-}

-- iterateM :: Monad m => (a -> m a) -> m a -> [a]
-- iterateM = \f m -> buildList (F.iterateM f m)
-- {-# INLINE iterateM #-}

-- ---------------
-- repeat
-- ---------------

repeat ::  a -> [a]
repeat = buildList . F.repeat 
{-# INLINE repeat #-}

-- repeatM :: Monad m => m a -> [a]
-- repeatM = buildList . F.repeatM
-- {-# INLINE repeatM #-}

-- ---------------
-- filter 
-- ---------------

filter  :: (a -> Bool) -> [a] -> [a]               
filter pred = buildList . F.filter pred . foldList
{-# INLINE filter #-}

-- filterM  :: (Monad m) => (a -> m Bool) -> [a] -> [a]
-- filterM pred = buildList . F.filterM pred . foldList
-- {-# INLINE filterM #-}

-- ---------------
-- drop
-- ---------------

drop :: Int -> [a] -> [a]
drop n = buildList . F.drop n . foldList
{-# INLINE drop #-}


-- ---------------
-- take
-- ---------------


take :: Int -> [a] -> [a]
take n = buildList . F.take n . foldList
{-# INLINE take #-}

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile pred = buildList . F.takeWhile pred . foldList 
{-# INLINE takeWhile #-}

-- ---------------
-- map
-- ---------------


map :: (a -> b) -> [a] -> [b]
map f = buildList . F.map f . foldList
{-# INLINE map #-}

-- mapM :: Monad m => (a -> m b) -> [a] -> [b]
-- mapM f = buildList . F.mapM f . foldList
-- {-# INLINE mapM #-}
-- 
-- span :: Monad m => (a -> Bool) -> [a] 
--       -> Producer a m ([a])
-- span pred = 
--   buildList 
--   . ( \(Folding phi) -> 
--         Folding (\c w d -> F.jspan phi pred c w (d . buildList . Folding)))
--   . foldList
--   
--   
-- splitAt :: (Monad m) 
--          => Int 
--          -> [a] 
--          -> ([a],[a])
-- splitAt n = 
--    buildList 
--    . (\(Folding phi) -> Folding (\c w d -> F.jsplitAt_ phi n c w (d . buildList . Folding)))
--    . foldList 
-- {-# INLINE splitAt #-}


enumFrom n = buildList (Folding (F.lenumFrom n))
enumFromTo n m = buildList (Folding (F.lenumFromTo n m))
enumFromToStep n m k = buildList (Folding (F.lenumFromToStep n m k))

enumFromStepN :: Num a => a -> a -> Int -> [a]
enumFromStepN start step n = buildList (Folding (F.lenumFromStepN start step n))
{-# INLINE enumFromStepN #-}






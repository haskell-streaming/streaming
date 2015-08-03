
{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Stream.Producer.Prelude 
    (concats, 
     cons, 
     drop, 
     filter,
     filterM,
     foldl',
     yield,
     iterate,
     iterateM,
     map,
     mapM,
     repeat,
     repeatM,
     replicate,
     scanr,
     span, 
     splitAt, 
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
                      , mapM, scanr, span, break)
-- ---------------
-- ---------------
-- Prelude
-- ---------------
-- ---------------

-- ------
-- concats
-- ------


concats :: Monad m =>  FreeT (Producer a m) m r -> Producer a m r
concats  = buildProducer 
                . F.concats 
                . (\(Folding phi) -> 
                       Folding (\c w d -> phi (c . foldProducer) w d))
                . foldFreeT
{-# INLINE concats #-}


-- ------
-- cons
-- ------
cons :: Monad m => a -> Producer a m r -> Producer a m r 
cons a = buildProducer . F.cons a . foldProducer
{-# INLINE cons #-}


-- ------
-- yield
-- ------
yield :: Monad m => a -> Producer a m ()
yield = buildProducer . F.yield
{-# INLINE yield #-}

-- -------
-- foldl'
-- -------
foldl' :: Monad m => forall a b . (b -> a -> b) -> b -> Producer a m r -> m b
foldl' op b0 = F.foldl op b0 . foldProducer 
{-# INLINE foldl' #-}

-- -------
-- scanr
-- -------

scanr :: Monad m => (a -> b -> b) -> b -> Producer a m r -> Producer b m r
scanr op b = buildProducer 
           . F.scanr op b
           . foldProducer 
{-# INLINE scanr #-}



-- ---------------
-- sum 
-- ---------------

sum :: (Monad m, Num a) => Producer a m () -> m a
sum  = F.sum . foldProducer 
{-# INLINE sum #-}


-- ---------------
-- replicate 
-- ---------------

replicate :: Monad m => Int -> a -> Producer a m ()
replicate n = take n . repeat
{-# INLINE replicate #-}

replicateM :: Monad m => Int -> m a -> Producer a m ()
replicateM n a = buildProducer (F.replicateM n a)
{-# INLINE replicateM #-}

-- ---------------
-- iterate
-- ---------------

iterate :: Monad m => (a -> a) -> a -> Producer a m r
iterate f  = buildProducer . F.iterate f 
{-# INLINE iterate #-}

iterateM :: Monad m => (a -> m a) -> m a -> Producer a m r
iterateM = \f m -> buildProducer (F.iterateM f m)
{-# INLINE iterateM #-}

-- ---------------
-- repeat
-- ---------------

repeat :: Monad m => a -> Producer a m r
repeat = buildProducer . F.repeat 
{-# INLINE repeat #-}

repeatM :: Monad m => m a -> Producer a m r
repeatM = buildProducer . F.repeatM
{-# INLINE repeatM #-}

-- ---------------
-- filter 
-- ---------------

filter  :: (Monad m) => (a -> Bool) -> Producer a m r -> Producer a m r               
filter pred = buildProducer . F.filter pred . foldProducer
{-# INLINE filter #-}

filterM  :: (Monad m) => (a -> m Bool) -> Producer a m r -> Producer a m r
filterM pred = buildProducer . F.filterM pred . foldProducer
{-# INLINE filterM #-}

-- ---------------
-- drop
-- ---------------

drop :: (Monad m) => Int -> Producer a m r -> Producer a m r
drop n = buildProducer . F.drop n . foldProducer
{-# INLINE drop #-}


-- ---------------
-- take
-- ---------------


take :: (Monad m) => Int -> Producer a m r -> Producer a m ()
take n = buildProducer . F.take n . foldProducer
{-# INLINE take #-}

takeWhile :: Monad m => (a -> Bool) -> Producer a m r -> Producer a m ()
takeWhile pred = buildProducer . F.takeWhile pred . foldProducer 
{-# INLINE takeWhile #-}

-- ---------------
-- map
-- ---------------


map :: Monad m => (a -> b) -> Producer a m r -> Producer b m r
map f = buildProducer . F.map f . foldProducer
{-# INLINE map #-}

mapM :: Monad m => (a -> m b) -> Producer a m r -> Producer b m r
mapM f = buildProducer . F.mapM f . foldProducer
{-# INLINE mapM #-}

span :: Monad m => (a -> Bool) -> Producer a m r 
      -> Producer a m (Producer a m r)
span pred = 
  buildProducer 
  . fmap buildProducer
  . F.span pred
  . foldProducer
{-# INLINE span #-}


--
-- --------
-- break
-- --------

break
  :: Monad m =>
     (a -> Bool)
     -> Producer a m r -> Producer a m (Producer a m r)
break predicate = 
  buildProducer 
  . fmap buildProducer
  . F.span  (not . predicate) 
  . foldProducer
{-# INLINE break #-}


splitAt :: (Monad m) 
         => Int 
         -> Producer a m r 
         -> Producer a m (Producer a m r)
splitAt n = 
   buildProducer 
   . fmap buildProducer
   . F.splitAt_ n
   . foldProducer 
{-# INLINE splitAt #-}


enumFrom n = buildProducer (Folding (F.lenumFrom n))
enumFromTo n m = buildProducer (Folding (F.lenumFromTo n m))
enumFromToStep n m k = buildProducer (Folding (F.lenumFromToStep n m k))

enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> Producer a m ()
enumFromStepN start step n = buildProducer (Folding (F.lenumFromStepN start step n))
{-# INLINE enumFromStepN #-}





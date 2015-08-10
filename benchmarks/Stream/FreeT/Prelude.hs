{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Stream.FreeT.Prelude 
    ( break
    , concats
    , concats_
    , cons
    , drop
    , enumFrom
    , enumFromStepN
    , enumFromTo
    , enumFromToStep
    , filter
    , filterM
    , foldl'
    , iterate
    , iterateM
    , map
    , mapM
    , maps
    , repeat
    , repeatM
    , replicate
    , replicateM
    , scanr
    , span
    , splitAt
    , splitAt_
    , sum
    , take
    , takeWhile
    , yield) where

import Stream.Types
import qualified Stream.Folding.Prelude as F
import Control.Monad hiding (filterM, mapM, replicateM)
import Data.Functor.Identity
import Control.Monad.Trans
import qualified System.IO as IO
import Control.Monad.Trans.Free
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

concats_ :: Monad m =>  FreeT (Folding (Of a) m) m r -> FreeT (Of a) m r
concats_  = buildFreeT 
                . F.concats 
                . foldFreeT
{-# INLINE concats_ #-}

concats :: Monad m =>  FreeT (FreeT (Of a) m) m r -> FreeT (Of a) m r
concats  = buildFreeT 
                . F.concats 
                . (\(Folding phi) -> 
                       Folding (\c w d -> phi (c . foldFreeT) w d))
                . foldFreeT
{-# INLINE concats #-}


-- ------
-- cons
-- ------
cons :: Monad m => a -> FreeT (Of a) m r -> FreeT (Of a) m r 
cons a = buildFreeT . F.cons a . foldFreeT
{-# INLINE cons #-}


-- ------
-- yield
-- ------
yield :: Monad m => a -> FreeT (Of a) m ()
yield = buildFreeT . F.yield
{-# INLINE yield #-}

-- -------
-- foldl'
-- -------
foldl' :: Monad m => forall a b . (b -> a -> b) -> b -> FreeT (Of a) m r -> m b
foldl' op b0 = F.foldl op b0 . foldFreeT 
{-# INLINE foldl' #-}

-- -------
-- scanr
-- -------

scanr :: Monad m => (a -> b -> b) -> b -> FreeT (Of a) m r -> FreeT (Of b) m r
scanr op b = buildFreeT 
           . F.scanr op b
           . foldFreeT 
{-# INLINE scanr #-}



-- ---------------
-- sum 
-- ---------------

sum :: (Monad m, Num a) => FreeT (Of a) m () -> m a
sum  = F.sum . foldFreeT 
{-# INLINE sum #-}


-- ---------------
-- replicate 
-- ---------------

replicate :: Monad m => Int -> a -> FreeT (Of a) m ()
replicate n = take n . repeat
{-# INLINE replicate #-}

replicateM :: Monad m => Int -> m a -> FreeT (Of a) m ()
replicateM n a = buildFreeT (F.replicateM n a)
{-# INLINE replicateM #-}

-- ---------------
-- iterate
-- ---------------

iterate :: Monad m => (a -> a) -> a -> FreeT (Of a) m r
iterate f  = buildFreeT . F.iterate f 
{-# INLINE iterate #-}

iterateM :: Monad m => (a -> m a) -> m a -> FreeT (Of a) m r
iterateM = \f m -> buildFreeT (F.iterateM f m)
{-# INLINE iterateM #-}

-- ---------------
-- repeat
-- ---------------

repeat :: Monad m => a -> FreeT (Of a) m r
repeat = buildFreeT . F.repeat 
{-# INLINE repeat #-}

repeatM :: Monad m => m a -> FreeT (Of a) m r
repeatM = buildFreeT . F.repeatM
{-# INLINE repeatM #-}

-- ---------------
-- filter 
-- ---------------

filter  :: (Monad m) => (a -> Bool) -> FreeT (Of a) m r -> FreeT (Of a) m r               
filter pred = buildFreeT . F.filter pred . foldFreeT
{-# INLINE filter #-}

filterM  :: (Monad m) => (a -> m Bool) -> FreeT (Of a) m r -> FreeT (Of a) m r
filterM pred = buildFreeT . F.filterM pred . foldFreeT
{-# INLINE filterM #-}

-- ---------------
-- drop
-- ---------------

drop :: (Monad m) => Int -> FreeT (Of a) m r -> FreeT (Of a) m r
drop n = buildFreeT . F.drop n . foldFreeT
{-# INLINE drop #-}


-- ---------------
-- take
-- ---------------


take :: (Monad m, Functor f) => Int -> FreeT f m r -> FreeT f m ()
take n = buildFreeT . F.take n . foldFreeT
{-# INLINE take #-}

takeWhile :: Monad m => (a -> Bool) -> FreeT (Of a) m r -> FreeT (Of a) m ()
takeWhile pred = buildFreeT . F.takeWhile pred . foldFreeT 
{-# INLINE takeWhile #-}

-- ---------------
-- map
-- ---------------


map :: Monad m => (a -> b) -> FreeT (Of a) m r -> FreeT (Of b) m r
map f = buildFreeT . F.map f . foldFreeT
{-# INLINE map #-}

mapM :: Monad m => (a -> m b) -> FreeT (Of a) m r -> FreeT (Of b) m r
mapM f = buildFreeT . F.mapM f . foldFreeT
{-# INLINE mapM #-}

maps :: (Monad m, Functor f, Functor g) => (forall x . f x -> g x) -> FreeT f m r -> FreeT g m r
maps morph = buildFreeT . F.maps morph . foldFreeT
{-# INLINE maps #-}

span :: Monad m => (a -> Bool) -> FreeT (Of a) m r 
      -> FreeT (Of a) m (FreeT (Of a) m r)
span pred = 
  buildFreeT
  . fmap buildFreeT 
  . F.span pred
  . foldFreeT
{-# INLINE span #-}
  
-- --------
-- break
-- --------

break
  :: Monad m =>
     (a -> Bool)
     -> FreeT (Of a) m r -> FreeT (Of a) m (FreeT (Of a) m r)
break predicate = 
     buildFreeT
     . fmap buildFreeT 
     . F.span  (not . predicate) 
     . foldFreeT 
{-# INLINE break #-}
  
splitAt :: (Monad m, Functor f) 
         => Int 
         -> FreeT f m r 
         -> FreeT f m (FreeT f m r)
splitAt n = 
   buildFreeT
   . fmap buildFreeT 
   . F.splitAt n
   . foldFreeT 
{-# INLINE splitAt #-}
--
splitAt_ :: (Monad m) 
         => Int 
         -> FreeT (Of a) m r 
         -> FreeT (Of a) m (FreeT (Of a) m r)
splitAt_ n = 
   buildFreeT
   . fmap buildFreeT 
   . F.splitAt_ n
   . foldFreeT 
{-# INLINE splitAt_ #-}

enumFrom n = buildFreeT (Folding (F.lenumFrom n))
enumFromTo n m = buildFreeT (Folding (F.lenumFromTo n m))
enumFromToStep n m k = buildFreeT (Folding (F.lenumFromToStep n m k))

enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> FreeT (Of a) m ()
enumFromStepN start step n = buildFreeT (Folding (F.lenumFromStepN start step n))
{-# INLINE enumFromStepN #-}





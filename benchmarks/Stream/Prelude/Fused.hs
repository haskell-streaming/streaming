{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Streaming.Prelude.Fused 
  (cons, 
   drop, 
   filter,
   filterM,
   foldl',
   fold,
   fold',
   foldM,
   foldM',
   yield,
   iterate,
   iterateM,
   map,
   mapM,
   mapsM,
   print,
   repeat,
   repeatM,
   replicate,
   scanr,
   span, 
   splitAt, 
   sum,
   take,
   takeWhile,
   enumFromStepN,
   toList,
   fromList
   ) where
import qualified Streaming.Internal.Folding as F
import qualified Streaming.Prelude as D

import Streaming.Internal hiding (splitAt)
import Control.Monad hiding (filterM, mapM, foldM)
import Data.Functor.Identity
import Control.Monad.Trans
import qualified System.IO as IO
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo
                      , mapM, scanr, span, break, print, show)
import qualified Prelude as Prelude

-- ---------------
-- Prelude
-- ---------------
-- ---------------

-- for :: Monad m => Stream (Of a) m r -> (a -> Stream (Of b) m ()) -> Stream (Of b) m r
-- ------
-- cons
-- ------
cons :: Monad m => a -> Stream (Of a) m r -> Stream (Of a) m r 
cons a = buildStream . F.cons a . foldStream
{-# INLINE cons #-}


-- ------
-- yield
-- ------
yield :: Monad m => a -> Stream (Of a) m ()
yield = buildStream . F.yield
{-# INLINE yield #-}

-- -------
-- foldl'
-- -------
foldl' :: Monad m => forall a b . (b -> a -> b) -> b -> Stream (Of a) m r -> m b
foldl' op b0 = F.foldl op b0 . foldStream 
{-# INLINE foldl' #-}

-- -------
-- scanr
-- -------

scanr :: Monad m => (a -> b -> b) -> b -> Stream (Of a) m r -> Stream (Of b) m r
scanr op b = buildStream 
           . F.scanr op b
           . foldStream 
{-# INLINE scanr #-}


-- ---------------
-- sum 
-- ---------------

sum :: (Monad m, Num a) => Stream (Of a) m () -> m a
sum  = foldl' (+) 0
{-# INLINE sum #-}


-- ---------------
-- replicate 
-- ---------------

replicate :: Monad m => Int -> a -> Stream (Of a) m ()
replicate n = take n . repeat
{-# INLINE replicate #-}

replicateM :: Monad m => Int -> m a -> Stream (Of a) m ()
replicateM n a = buildStream (F.replicateM n a)
{-# INLINE replicateM #-}

-- ---------------
-- iterate
-- ---------------

iterate :: Monad m => (a -> a) -> a -> Stream (Of a) m r
iterate f  = buildStream . F.iterate f 
{-# INLINE iterate #-}

iterateM :: Monad m => (a -> m a) -> m a -> Stream (Of a) m r
iterateM = \f m -> buildStream (F.iterateM f m)
{-# INLINE iterateM #-}

-- ---------------
-- repeat
-- ---------------

repeat :: a -> Stream (Of a) m r
repeat = buildStream . F.repeat 
{-# INLINE repeat #-}

repeatM :: Monad m => m a -> Stream (Of a) m r
repeatM = buildStream . F.repeatM
{-# INLINE repeatM #-}

-- ---------------
-- filter 
-- ---------------

filter  :: (Monad m) => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r               
filter pred = buildStream . F.filter pred . foldStream
{-# INLINE filter #-}

filterM  :: (Monad m) => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filterM pred = buildStream . F.filterM pred . foldStream
{-# INLINE filterM #-}

-- ---------------
-- drop
-- ---------------

drop :: (Monad m) => Int -> Stream (Of a) m r -> Stream (Of a) m r
drop n = buildStream . F.drop n . foldStream
{-# INLINE drop #-}


-- ---------------
-- take
-- ---------------


take :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
take n = buildStream . F.take n . foldStream
{-# INLINE take #-}

takeWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhile pred = buildStream . F.takeWhile pred . foldStream 
{-# INLINE takeWhile #-}

-- ---------------
-- map
-- ---------------

show :: (Monad m, Show a) =>  Stream (Of a) m r -> Stream (Of String) m r
show = map Prelude.show
{-# INLINE show #-}

map :: Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
map f = buildStream . F.map f . foldStream
{-# INLINE map #-}

mapM :: Monad m => (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapM f = buildStream . F.mapM f . foldStream
{-# INLINE mapM #-}

--
-- mapFoldable :: (Monad m, Foldable t) => (a -> t b) -> Stream (Of a) m r -> Stream (Of b) m r
-- mapFoldable

span :: Monad m => (a -> Bool) -> Stream (Of a) m r 
      -> Stream (Of a) m (Stream (Of a) m r)
span pred = 
  buildStream 
  . fmap buildStream
  . F.span pred
  . foldStream
{-# INLINE span #-}


break :: Monad m => (a -> Bool) -> Stream (Of a) m r 
      -> Stream (Of a) m (Stream (Of a) m r)
break pred = 
  buildStream 
  . fmap buildStream
  . F.span (not . pred)
  . foldStream
{-# INLINE break #-}

splitAt :: (Monad m, Functor f) 
         => Int 
         -> Stream f m r 
         -> Stream f m (Stream f m r)
splitAt n = 
   buildStream 
   . fmap buildStream
   . F.splitAt n
   . foldStream 
{-# INLINE splitAt #-}

splitAt_ :: (Monad m) 
         => Int 
         -> Stream (Of a) m r 
         -> Stream (Of a) m (Stream (Of a) m r)
splitAt_ n = 
   buildStream 
   . fmap buildStream
   . F.splitAt_ n
   . foldStream 
{-# INLINE splitAt_ #-}

enumFrom n = buildStream (Folding (F.enumFrom n))
enumFromTo n m = buildStream (Folding (F.enumFromTo n m))
enumFromToStep n m k = buildStream (Folding (F.enumFromToStep n m k))

enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> Stream (Of a) m ()
enumFromStepN start step n = buildStream (Folding (F.enumFromStepN start step n))
{-# INLINE enumFromStepN #-}

-- ---------------------------------------
-- IO fripperies copped from Pipes.Prelude
-- ---------------------------------------


-- stdinLn = Delay loop where
--   loop = getLine >>= \str -> return (Step (str :> Delay loop))
-- 
-- jstdinLn = \construct wrap done -> 
--      let loop = wrap $ getLine >>= \str -> return (construct (str :> loop))
--      in loop 
--
stdinLn :: MonadIO m => Stream (Of String) m ()
stdinLn = fromHandle IO.stdin
{-# INLINE stdinLn #-}

-- | 'read' values from 'IO.stdin', ignoring failed parses
readLn :: (MonadIO m, Read a) => Stream (Of a) m ()
readLn = map read stdinLn
{-# INLINE readLn #-}

print :: (MonadIO m, Show a) => Stream (Of a) m r -> m r
print str = getFolding (foldStream str)
    (\(a :> b) -> liftIO (Prelude.print a) >> b) join return
{-# INLINE print #-}

{-| Read 'String's from a 'IO.Handle' using 'IO.hGetLine'

    Terminates on end of input
-}
fromHandle :: MonadIO m => IO.Handle -> Stream (Of String) m ()
fromHandle h = go
  where
    go = do
        eof <- liftIO $ IO.hIsEOF h
        unless eof $ do
            str <- liftIO $ IO.hGetLine h
            yield str
            go
{-# INLINABLE fromHandle #-}     


toList :: Monad m => Stream (Of a) m r -> m [a]
toList str = getFolding (foldStream str) 
                      (\(a :> mas) -> liftM (a :) mas)
                      (>>= id)
                      (\_ -> return [])
{-# INLINE toList #-}

fromList :: [a] -> Stream (Of a) m ()
fromList xs = foldr (\x xs -> Step (x :> xs)) (Return ()) xs
{-# INLINE fromList #-}

fold
  :: Monad m =>
     (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r -> m b
fold step begin done  = F.fold step begin done . foldStream 

fold'
  :: Monad m =>
     (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r -> m (b,r)
fold' step begin done  = F.fold' step begin done . foldStream 

foldM :: Monad m =>
          (x -> a -> m x)
          -> m x -> (x -> m b) -> Stream (Of a) m r -> m b
foldM step begin done  = F.foldM step begin done . foldStream 

foldM' :: Monad m =>
          (x -> a -> m x)
          -> m x -> (x -> m b) -> Stream (Of a) m r -> m (b,r)
foldM' step begin done  = F.foldM' step begin done . foldStream 



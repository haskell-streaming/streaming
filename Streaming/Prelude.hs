{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
module Streaming.Prelude 
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
   maps,
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
import qualified Streaming.Internal.Folding as F
import Streaming.Internal
import Control.Monad hiding (filterM, mapM)
import Data.Functor.Identity
import Control.Monad.Trans
import qualified System.IO as IO
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo
                      , mapM, scanr, span, break)

-- ---------------
-- Prelude
-- ---------------
-- ---------------

-- ------
-- concats
-- ------

concats_ :: Monad m =>  Stream (Folding (Of a) m) m r -> Stream (Of a) m r
concats_  = buildStream 
                . F.concats 
                . foldStream
{-# INLINE concats_ #-}

concats :: Monad m =>  Stream (Stream (Of a) m) m r -> Stream (Of a) m r
concats  = buildStream 
                . F.concats 
                . (\(Folding phi) -> 
                       Folding (\c w d -> phi (c . foldStream) w d))
                . foldStream
{-# INLINE concats #-}


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
sum  = F.sum . foldStream 
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

iterate :: (a -> a) -> a -> Stream (Of a) m r
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


map :: Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
map f = buildStream . F.map f . foldStream
{-# INLINE map #-}

mapM :: Monad m => (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapM f = buildStream . F.mapM f . foldStream
{-# INLINE mapM #-}


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

enumFrom n = buildStream (Folding (F.lenumFrom n))
enumFromTo n m = buildStream (Folding (F.lenumFromTo n m))
enumFromToStep n m k = buildStream (Folding (F.lenumFromToStep n m k))

enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> Stream (Of a) m ()
enumFromStepN start step n = buildStream (Folding (F.lenumFromStepN start step n))
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
{-# INLINABLE stdinLn #-}

-- | 'read' values from 'IO.stdin', ignoring failed parses
readLn :: (MonadIO m, Read a) => Stream (Of a) m ()
readLn = map read stdinLn
{-# INLINABLE readLn #-}

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



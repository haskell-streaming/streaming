{-# LANGUAGE LambdaCase, RankNTypes, BangPatterns #-}
module Stream.Prelude.Direct where
import Stream.Types
import Control.Monad hiding (filterM, mapM)
import Data.Functor.Identity
import Control.Monad.Trans
import qualified System.IO as IO
import Prelude hiding (map, filter, drop, take, sum
                      , iterate, repeat, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo)


-- ---------------
-- ---------------
-- Prelude
-- ---------------
-- ---------------

yield :: Monad m => a -> Stream (Of a) m ()
yield r = Step (r :> Return ())
{-# INLINEABLE yield #-}
-- ---------------
-- sum 
-- ---------------
sum :: (Monad m, Num a) => Stream (Of a) m () -> m a
sum = loop where
  loop = \case Step (a :> as) -> liftM (a+) (loop as)
               Delay m -> m >>= loop
               Return r -> return 0
{-# INLINEABLE sum #-}
-- ---------------
-- replicate 
-- ---------------

replicate :: Monad m => Int -> a -> Stream (Of a) m ()
replicate n a = loop n where
  loop 0 = Return ()
  loop m = Step (a :> loop (m-1))
{-# INLINEABLE replicate #-}

replicateM :: Monad m => Int -> m a -> Stream (Of a) m ()
replicateM n ma = loop n where 
  loop 0 = Return ()
  loop n = Delay $ ma >>= \a -> return (Step $ a :> loop (n-1))
{-# INLINEABLE replicateM #-}


-- ---------------
-- iterate
-- ---------------

iterate :: (a -> a) -> a -> Stream (Of a) m r
iterate f = loop where
  loop a' = Step (a' :> loop (f a'))
{-# INLINEABLE iterate #-}

iterateM :: Monad m => (a -> m a) -> m a -> Stream (Of a) m r
iterateM f = loop where
  loop ma  = Delay $ do a <- ma
                        return (Step (a :> loop (f a)))
{-# INLINEABLE iterateM #-}

-- ---------------
-- repeat
-- ---------------

repeat :: a -> Stream (Of a) m r
repeat a = loop where
  loop = Step (a :> loop)
{-# INLINE repeat #-}

repeatM :: Monad m => m a -> Stream (Of a) m r
repeatM ma = loop where
  loop = Delay $ ma >>= \a -> return (Step (a :> loop))
{-# INLINEABLE repeatM #-}

-- ---------------
-- filter 
-- ---------------

filter  :: (Monad m) => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filter pred = loop where
  loop = \case Step (a :> as) -> if pred a then Step (a :> loop as)
                                                else loop as
               Delay m -> Delay $ liftM loop m
               Return r -> Return r
{-# INLINEABLE filter #-}

filterM  :: (Monad m) => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filterM pred = loop where
  loop = \case 
     Step (a:>as) -> Delay $ do b <- pred a
                                if b then return $ Step (a :> loop as)
                                     else return $ loop as
     Delay m            -> Delay $ liftM loop m
     Return r           -> Return r
{-# INLINEABLE filterM #-}

-- ---------------
-- drop
-- ---------------

drop :: (Monad m) => Int -> Stream (Of a) m r -> Stream (Of a) m r
drop = loop where
  loop 0 p = p
  loop n p = case p of
     Step (a :> as) -> loop (n-1) as
     Delay ma      -> Delay (liftM (drop n) ma)
     Return r       -> Return r
{-# INLINEABLE drop #-}

-- ---------------
-- map
-- ---------------

map f = loop where
  loop = \case Step (a :> as) -> Step (f a :> loop as)
               Delay m -> Delay (liftM (map f) m)
               Return r -> Return r
{-# INLINEABLE map #-}

mapM :: Monad m => (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapM f = loop where
  loop = \case Step (a :> as) -> Delay $ liftM (Step.(:> loop as)) (f a)
{-# INLINEABLE mapM #-}

-- ---------------
-- take
-- ---------------



take :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
take = loop where
  loop 0 p = return ()
  loop n p = case p of Step fas -> Step (fmap (loop (n-1)) fas)
                       Delay m -> Delay (liftM (loop n) m)
                       Return r -> Return ()
{-# INLINEABLE take #-}

takeWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhile pred = loop where
  loop = \case Step (a :> as) -> if pred a then Step (a :> loop as)
                                                else return () 
               Delay m              -> Delay (liftM loop m)
               Return r              -> Return ()
{-# INLINEABLE takeWhile #-}



-- ------- 
-- lenumFrom n = \construct wrap done -> 
--       let loop m = construct (m :> loop (succ m)) in loop n
--         
-- lenumFromTo n m = \construct wrap done -> 
--       let loop k = if k <= m then construct (k :> loop (succ k)) 
--                              else done ()
--       in loop n
-- 
-- lenumFromToStep n m k = \construct wrap done -> 
--             let loop p = if p <= k then construct (p :> loop (p + m)) 
--                                    else done ()
--             in loop n
-- --
-- lenumFromStepN a b k = \construct wrap done -> 
--             let loop 0 p = done ()
--                 loop j p = construct (p :> loop (j-1) (p + b)) 
--             in loop a k
-- {-# INLINE lenumFromStepN #-}
-- enumFrom n = buildStream (Folding (lenumFrom n))
-- enumFromTo n m = buildStream (Folding (lenumFromTo n m))
-- enumFromToStep n m k = buildStream (Folding (lenumFromToStep n m k))
enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> Stream (Of a) m ()
enumFromStepN start step = loop start
  where
    loop !s 0 = Return ()
    loop s m = Step (s :> loop (s+step) (m-1))
{-# INLINEABLE enumFromStepN #-}


splitAt :: Monad m => Int -> Stream (Of a) m r -> Stream (Of a) m (Stream (Of a) m r)
splitAt n = loop n where
  loop n (Return r)              = Return (Return r)
  loop n (Delay m)              = Delay (liftM (loop n) m)
  loop n (Step (a :> fs)) = case n of 
     0 -> Return (Step (a :> fs))
     _ -> Step (a :> loop (n-1) fs)
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
--


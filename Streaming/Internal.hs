{-# LANGUAGE RankNTypes, StandaloneDeriving,DeriveDataTypeable, BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-} -- for show, data instances
module Streaming.Internal (
    -- * The free monad transformer
    -- $stream
    Stream (..)
    
    -- * Introducing a stream
    , construct 
    , unfold 
    , replicates
    , repeats
    , repeatsM
    
    -- * Eliminating a stream
    , destroy 
    , concats 
    , intercalates 
    , iterT 
    , iterTM 

    -- * Inspecting a stream step by step
    , inspect 
    
    -- * Transforming streams
    , maps 
    , mapsM 
    , distribute
    
    -- *  Splitting streams
    , chunksOf 
    , splitsAt
   ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Foldable ( Foldable )
import Data.Traversable
import Control.Monad.Morph
import Data.Monoid
import Data.Functor.Identity
import GHC.Exts ( build )
import Data.Data ( Data, Typeable )
import Prelude hiding (splitAt)

{- $stream

    The 'Stream' data type is equivalent to @FreeT@ and can represent any effectful
    succession of steps, where the form of the steps or 'commands' is 
    specified by the first (functor) parameter. 

> data Stream f m r = Step !(f (Stream f m r)) | Delay (m (Stream f m r)) | Return r

    The /producer/ concept uses the simple functor @ (a,_) @ \- or the stricter 
    @ Of a _ @. Then the news at each step or layer is just: an individual item of type @a@. 
    Since @Stream (Of a) m r@ is equivalent to @Pipe.Producer a m r@, much of
    the @pipes@ @Prelude@ can easily be mirrored in a @streaming@ @Prelude@. Similarly, 
    a simple @Consumer a m r@ or @Parser a m r@ concept arises when the base functor is
    @ (a -> _) @ . @Stream ((->) input) m result@ consumes @input@ until it returns a 
    @result@.

    To avoid breaking reasoning principles, the constructors 
    should not be used directly. A pattern-match should go by way of 'inspect' \
    \- or, in the producer case, 'Streaming.Prelude.next'
    The constructors are exported by the 'Internal' module.
-}
data Stream f m r = Step !(f (Stream f m r))
                  | Delay (m (Stream f m r))
                  | Return r
                  deriving (Typeable)

deriving instance (Show r, Show (m (Stream f m r))
                  , Show (f (Stream f m r))) => Show (Stream f m r)
deriving instance (Eq r, Eq (m (Stream f m r))
                  , Eq (f (Stream f m r))) => Eq (Stream f m r)
deriving instance (Typeable f, Typeable m, Data r, Data (m (Stream f m r))
                  , Data (f (Stream f m r))) => Data (Stream f m r)

instance (Functor f, Monad m) => Functor (Stream f m) where
  fmap f = loop where
    loop stream = case stream of
      Return r -> Return (f r)
      Delay m  -> Delay (liftM loop m)
      Step f   -> Step (fmap loop f)
  {-# INLINABLE fmap #-}
  
instance (Functor f, Monad m) => Monad (Stream f m) where
  return = Return
  {-# INLINE return #-}
  stream1 >> stream2 = loop stream1 where
    loop stream = case stream of
      Return _ -> stream2
      Delay m  -> Delay (liftM loop m)
      Step f   -> Step (fmap loop f)    
  {-# INLINABLE (>>) #-}                              
  stream >>= f = loop stream where
    loop stream0 = case stream0 of
      Step f -> Step (fmap loop f)
      Delay m      -> Delay (liftM loop m)
      Return r      -> f r
  {-# INLINABLE (>>=) #-}                              

instance (Functor f, Monad m) => Applicative (Stream f m) where
  pure = Return
  {-# INLINE pure #-}
  streamf <*> streamx = do {f <- streamf; x <- streamx; return (f x)}   
  {-# INLINABLE (<*>) #-}    
  
instance Functor f => MonadTrans (Stream f) where
  lift = Delay . liftM Return
  {-# INLINE lift #-}

instance Functor f => MFunctor (Stream f) where
  hoist trans = loop where
    loop stream = case stream of 
      Return r  -> Return r
      Delay m   -> Delay (trans (liftM loop m))
      Step f    -> Step (fmap loop f)
  {-# INLINABLE hoist #-}    

instance Functor f => MMonad (Stream f) where
  embed phi = loop where
    loop stream = case stream of
      Return r -> Return r
      Delay m  -> phi m >>= loop
      Step f   -> Step (fmap loop f)
  {-# INLINABLE embed #-}   
   
instance (MonadIO m, Functor f) => MonadIO (Stream f m) where
  liftIO = Delay . liftM Return . liftIO
  {-# INLINE liftIO #-}

-- | Map a stream to its church encoding; compare @Data.List.foldr@
destroy 
  :: (Functor f, Monad m) =>
     Stream f m r -> (f b -> b) -> (m b -> b) -> (r -> b) -> b
destroy stream0 construct wrap done = loop stream0 where
  loop stream = case stream of
    Return r -> done r
    Delay m  -> wrap (liftM loop m)
    Step fs  -> construct (fmap loop fs)
{-# INLINABLE destroy #-}

-- | Reflect a church-encoded stream; cp. @GHC.Exts.build@
construct
  :: (forall b . (f b -> b) -> (m b -> b) -> (r -> b) -> b) ->  Stream f m r
construct = \phi -> phi Step Delay Return
{-# INLINE construct #-}


{-| Inspect the first stage of a freely layered sequence. 
    Compare @Pipes.next@ and the replica @Streaming.Prelude.next@. 
    This is the 'uncons' for the general 'unfold'.

> unfold inspect = id
> Streaming.Prelude.unfoldr StreamingPrelude.next = id
-}
inspect :: (Functor f, Monad m) =>
     Stream f m r -> m (Either r (f (Stream f m r)))
inspect = loop where
  loop stream = case stream of
    Return r -> return (Left r)
    Delay m  -> m >>= loop
    Step fs  -> return (Right fs)
{-# INLINABLE inspect #-}
    
{-| Build a @Stream@ by unfolding steps starting from a seed. See also
    the specialized 'Streaming.Prelude.unfoldr' in the prelude.

> unfold inspect = id -- modulo the quotient we work with
> unfold Pipes.next :: Monad m => Producer a m r -> Stream ((,) a) m r
> unfold (curry (:>) . Pipes.next) :: Monad m => Producer a m r -> Stream (Of a) m r

-}

unfold :: (Monad m, Functor f) 
        => (s -> m (Either r (f s))) -> s -> Stream f m r
unfold step = loop where
  loop s0 = Delay $ do 
    e <- step s0
    case e of
      Left r -> return (Return r)
      Right fs -> return (Step (fmap loop fs))
{-# INLINABLE unfold #-}


-- | Map layers of one functor to another with a natural transformation
maps :: (Monad m, Functor f) 
     => (forall x . f x -> g x) -> Stream f m r -> Stream g m r
maps phi = loop where
  loop stream = case stream of 
    Return r  -> Return r
    Delay m   -> Delay (liftM loop m)
    Step f    -> Step (phi (fmap loop f))
{-# INLINABLE maps #-}

-- | Map layers of one functor to another with a transformation involving the base monad
mapsM :: (Monad m, Functor f) => (forall x . f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsM phi = loop where
  loop stream = case stream of 
    Return r  -> Return r
    Delay m   -> Delay (liftM loop m)
    Step f    -> Delay (liftM Step (phi (fmap loop f)))
{-# INLINABLE mapsM #-}


{-| Interpolate a layer at each segment. This specializes to e.g.

> intercalates :: (Monad m, Functor f) => Stream f m () -> Stream (Stream f m) m r -> Stream f m r
-}
intercalates :: (Monad m, Monad (t m), MonadTrans t) =>
     t m a -> Stream (t m) m b -> t m b
intercalates sep = go0
  where
    go0 f = case f of 
      Return r -> return r 
      Delay m -> lift m >>= go0 
      Step fstr -> do
                f' <- fstr
                go1 f'
    go1 f = case f of 
      Return r -> return r 
      Delay m     -> lift m >>= go1
      Step fstr ->  do
                sep
                f' <- fstr
                go1 f'
{-# INLINABLE intercalates #-}

{-| Specialized fold

> iterTM alg stream = destroy stream alg (join . lift) return
-}
iterTM ::
  (Functor f, Monad m, MonadTrans t,
   Monad (t m)) =>
  (f (t m a) -> t m a) -> Stream f m a -> t m a
iterTM out stream = destroy stream out (join . lift) return
{-# INLINE iterTM #-}

{-| Specialized fold

> iterT alg stream = destroy stream alg join return
-}
iterT ::
  (Functor f, Monad m) => (f (m a) -> m a) -> Stream f m a -> m a
iterT out stream = destroy stream out join return
{-# INLINE iterT #-}

{-| This specializes to the more transparent case:

> concats :: (Monad m, Functor f) => Stream (Stream f m) m r -> Stream f m r

    Thus dissolving the segmentation into @Stream f m@ layers.

> concats stream = destroy stream join (join . lift) return

>>> S.print $ concats $ maps (cons 1776) $ chunksOf 2 (each [1..5])
1776
1
2
1776
3
4
1776
5

-}
concats ::
    (MonadTrans t, Monad (t m), Monad m) =>
    Stream (t m) m a -> t m a
concats stream = destroy stream join (join . lift) return
{-# INLINE concats #-}

{-| Split a succession of layers after some number, returning a streaming or
    effectful pair.

>>> rest <- S.print $ S.splitAt 1 $ each [1..3]
1
>>> S.print rest
2
3
-}
splitsAt :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
splitsAt = loop where
  loop !n stream 
    | n <= 0 = Return stream
    | otherwise = case stream of
        Return r       -> Return (Return r)
        Delay m        -> Delay (liftM (loop n) m)
        Step fs        -> case n of 
          0 -> Return (Step fs)
          _ -> Step (fmap (loop (n-1)) fs)
{-# INLINABLE splitsAt #-}                        

{-| Break a stream into substreams each with n functorial layers. 

>>>  S.print $ maps' sum' $ chunksOf 2 $ each [1,1,1,1,1,1,1]
2
2
2
1
-}
chunksOf :: (Monad m, Functor f) => Int -> Stream f m r -> Stream (Stream f m) m r
chunksOf n0 = loop where
  loop stream = case stream of
    Return r       -> Return r
    Delay m        -> Delay (liftM loop m)
    Step fs        -> Step $ Step $ fmap (fmap loop . splitsAt (n0-1)) fs
{-# INLINABLE chunksOf #-}          

{- | Make it possible to \'run\' the underlying transformed monad. A simple
     minded example might be: 

> debugFibs = flip runStateT 1 $ distribute $ loop 1 where
>   loop n = do
>     S.yield n
>     s <- lift get 
>     liftIO $ putStr "Current state is:  " >> print s
>     lift $ put (s + n :: Int)
>     loop s

>>> S.print $  S.take 4 $ S.drop 4 $ debugFibs
Current state is:  1
Current state is:  2
Current state is:  3
Current state is:  5
5
Current state is:  8
8
Current state is:  13
13
Current state is:  21
21

-}
distribute :: (Monad m, Functor f, MonadTrans t, MFunctor t, Monad (t (Stream f m)))
           => Stream f (t m) r -> t (Stream f m) r
distribute = loop where
  loop stream = case stream of 
    Return r    -> lift $ Return r
    Delay tmstr -> hoist lift tmstr >>= distribute
    Step fstr   -> join $ lift (Step (fmap (Return . distribute) fstr))
    
-- | Repeat a functorial layer, command or instruction forever.
repeats :: (Monad m, Functor f) => f () -> Stream f m r 
repeats f = loop where
  loop = Step $ fmap (\_ -> loop) f

-- Repeat a functorial layer, command or instruction forever.
repeatsM :: (Monad m, Functor f) => m (f ()) -> Stream f m r 
repeatsM mf = loop where
  loop = Delay $ do
     f <- mf
     return $ Step $ fmap (\_ -> loop) f

-- | Repeat a functorial layer, command or instruct several times.
replicates :: (Monad m, Functor f) => Int -> f () -> Stream f m ()
replicates n f = splitsAt n (repeats f) >> return ()

{-| Construct an infinite stream by cycling a finite one

> cycles = forever

>>> S.print $ S.take 3 $ forever $ S.each "hi"
'h'
'i'
'h'
> S.sum $ S.take 13 $ forever $ S.each [1..3]
25
-}
 
cycles :: (Monad m, Functor f) =>  Stream f m () -> Stream f m r
cycles = forever


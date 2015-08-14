{-# LANGUAGE RankNTypes, StandaloneDeriving,DeriveDataTypeable, BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-} -- for show, data instances
module Streaming.Internal (
    -- * The free monad transformer
    -- $stream
    Stream (..)
    
    -- * Introducing a stream
    , construct 
    , unfold 
    
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
    
    -- *  Splitting streams
    , chunksOf 
    , split 
    
    -- * Utility type
    , SPEC (..)
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
import GHC.Exts ( SpecConstrAnnotation(..) )

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}
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
  stream1 >> stream2 = loop SPEC stream1 where
    loop !_ stream = case stream of
      Return _ -> stream2
      Delay m  -> Delay (liftM (loop SPEC) m)
      Step f   -> Step (fmap (loop SPEC) f)    
  {-# INLINABLE (>>) #-}                              
  stream >>= f = loop SPEC stream where
    loop !_ stream0 = case stream0 of
      Step f -> Step (fmap (loop SPEC) f)
      Delay m      -> Delay (liftM (loop SPEC) m)
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

-- | Map a stream to its church encoding; compare list 'foldr'
destroy 
  :: (Functor f, Monad m) =>
     Stream f m r -> (f b -> b) -> (m b -> b) -> (r -> b) -> b
destroy stream0 construct wrap done = loop stream0 where
  loop stream = case stream of
    Return r -> done r
    Delay m  -> wrap (liftM loop m)
    Step fs  -> construct (fmap loop fs)
{-# INLINABLE destroy #-}

-- | Reflect a church-encoded stream; cp. GHC.Exts.build
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
    
{-| Build a @Stream@ by unfolding steps starting from a seed. 

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

iterTM ::
  (Functor f, Monad m, MonadTrans t,
   Monad (t m)) =>
  (f (t m a) -> t m a) -> Stream f m a -> t m a
iterTM out stream = destroy stream out (join . lift) return
{-# INLINE iterTM #-}

iterT ::
  (Functor f, Monad m) => (f (m a) -> m a) -> Stream f m a -> m a
iterT out stream = destroy stream out join return
{-# INLINE iterT #-}

concats ::
    (MonadTrans t, Monad (t m), Monad m) =>
    Stream (t m) m a -> t m a
concats stream = destroy stream join (join . lift) return
{-# INLINE concats #-}


split :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
split = loop where
  loop !n stream 
    | n <= 1 = Return stream
    | otherwise = case stream of
        Return r       -> Return (Return r)
        Delay m        -> Delay (liftM (loop n) m)
        Step fs        -> case n of 
          0 -> Return (Step fs)
          _ -> Step (fmap (loop (n-1)) fs)
{-# INLINABLE split #-}                        

chunksOf :: (Monad m, Functor f) => Int -> Stream f m r -> Stream (Stream f m) m r
chunksOf n0 = loop where
  loop stream = case stream of
    Return r       -> Return r
    Delay m        -> Delay (liftM loop m)
    Step fs        -> Step $ Step $ fmap (fmap loop . split n0) fs
{-# INLINABLE chunksOf #-}          

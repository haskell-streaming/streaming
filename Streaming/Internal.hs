{-# LANGUAGE LambdaCase, RankNTypes, EmptyCase,
             StandaloneDeriving, FlexibleContexts,
             DeriveDataTypeable, DeriveFoldable,
             DeriveFunctor, DeriveTraversable,
             ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-} -- for Streaming show instance
module Streaming.Internal where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Data ( Data, Typeable )
import Data.Foldable ( Foldable )
import Data.Traversable
import Control.Monad.Morph
import Data.Monoid
import Data.Functor.Identity
import GHC.Exts ( build )
import Prelude hiding (splitAt)

-- | A left-strict pair; the base functor for streams of individual elements.
data Of a b = !a :> b
    deriving (Data, Eq, Foldable, Functor, Ord,
              Read, Show, Traversable, Typeable)
infixr 4 :>

-- | Curry a function of left-strict pairs
kurry :: (Of a b -> c) -> a -> b -> c
kurry f = \a b -> f (a :> b)
{-# INLINE kurry #-}

-- | Uncurry a function into a function on left-strict pairs 

unkurry :: (a -> b -> c) -> Of a b -> c
unkurry f = \(a :> b) -> f a b
{-# INLINE unkurry #-}

-- | @Stream@ (\'FreeT\') data type. The constructors are exported by @Streaming.Internal@
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


{-| Inspect the first \'layer\' of a free sequence of \'f\'. 
    Compare @Pipes.next@ and the replica @Streaming.next@. 
    This is the 'uncons' for 'unfoldr'

unfoldr inspect = id

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

unfold inspect = id -- modulo the quotient we work with
unfold Pipes.next :: Monad m => Producer a m r -> Stream ((,) a) m r
unfold (curry (:>) . Pipes.next) :: Monad m => Producer a m r -> Stream (Of a) m r

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

mapsM :: (Monad m, Functor f) => (forall x . f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsM phi = loop where
  loop stream = case stream of 
    Return r  -> Return r
    Delay m   -> Delay (liftM loop m)
    Step f    -> Delay (liftM Step (phi (fmap loop f)))
{-# INLINABLE mapsM #-}

maps' :: (Monad m, Functor f) 
          => (forall x . f x -> m (a, x)) 
          -> Stream f m r 
          -> Stream (Of a) m r
maps' phi = loop where
  loop stream = case stream of 
    Return r -> Return r
    Delay m -> Delay $ liftM loop m
    Step fs -> Delay $ liftM (Step . uncurry (:>)) (phi (fmap loop fs))
{-# INLINABLE maps' #-}


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

intercalates' :: (Monad m, Monad (t m), MonadTrans t) =>
     t m a -> Stream (t m) m b -> t m b
intercalates' sep stream = destroy stream 
   (\tmstr -> do 
     str <- tmstr
     sep
     str
     )
   (join . lift)
   return
{-# INLINE intercalates' #-}

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

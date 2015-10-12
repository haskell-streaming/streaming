{-# LANGUAGE RankNTypes, StandaloneDeriving,DeriveDataTypeable, BangPatterns #-}
{-# LANGUAGE UndecidableInstances, CPP #-} -- for show, data instances
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
    , effect
    , wrap
    , elevate
    
    -- * Eliminating a stream
    , intercalates 
    , concats 
    , iterT 
    , iterTM 
    , destroy 
    , destroyWith
    
    -- * Inspecting a stream wrap by wrap
    , inspect 
    
    -- * Transforming streams
    , maps 
    , mapsM 
    , decompose
    , mapsM_
    , eithers
    , run
    , distribute
    
    -- *  Splitting streams
    , chunksOf 
    , splitsAt
    , takes
    
    -- * Zipping streams
    , zipsWith
    , zips
    , interleaves
    
    -- *  For use in implementation
    , unexposed
    , hoistExposed
    , mapsExposed
    , mapsMExposed
    , destroyExposed
    
   ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Foldable ( Foldable(..) )
import Data.Traversable
import Control.Monad.Morph
import Data.Monoid (Monoid (..), (<>))
import Data.Functor.Identity
import GHC.Exts ( build )
import Data.Data ( Data, Typeable )
import Prelude hiding (splitAt)
import Data.Functor.Compose
import Data.Functor.Sum
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
#if __GLASGOW_HASKELL__ >= 710
                  deriving (Typeable)
#endif
deriving instance (Show r, Show (m (Stream f m r))
                  , Show (f (Stream f m r))) => Show (Stream f m r)
deriving instance (Eq r, Eq (m (Stream f m r))
                  , Eq (f (Stream f m r))) => Eq (Stream f m r)
#if __GLASGOW_HASKELL__ >= 710
deriving instance (Typeable f, Typeable m, Data r, Data (m (Stream f m r))
                  , Data (f (Stream f m r))) => Data (Stream f m r)
#endif
instance (Functor f, Monad m) => Functor (Stream f m) where
  fmap f = loop where
    loop stream = case stream of
      Return r -> Return (f r)
      Delay m  -> Delay (do {stream' <- m; return (loop stream')})
      Step f   -> Step (fmap loop f)
  {-# INLINABLE fmap #-}
  a <$ stream0 = loop stream0 where
    loop stream = case stream of
      Return r -> Return a
      Delay m  -> Delay (do {stream' <- m; return (loop stream')})
      Step f    -> Step (fmap loop f)
  {-# INLINABLE (<$) #-}    
  
instance (Functor f, Monad m) => Monad (Stream f m) where
  return = Return
  {-# INLINE return #-}
  stream1 >> stream2 = loop stream1 where
    loop stream = case stream of
      Return _ -> stream2
      Delay m  -> Delay (liftM loop m)
      Step f   -> Step (fmap loop f)    
  {-# INLINABLE (>>) #-}
  (>>=) = _bind
  -- stream >>= f = 
  --   loop stream where
  --   loop stream0 = case stream0 of
  --     Step fstr -> Step (fmap loop fstr)
  --     Delay m   -> Delay (liftM loop m)
  --     Return r  -> f r
  -- {-# INLINABLE (>>=) #-}                         

  fail = lift . fail


_bind
    :: (Functor f, Monad m)
    => Stream f m r
    -> (r -> Stream f m s)
    -> Stream f m s
_bind p0 f = go p0 where
    go p = case p of
      Step fstr  -> Step (fmap go fstr)
      Delay m   -> Delay (m >>= \s -> return (go s))
      Return r  -> f r

{-# RULES
    "_bind (Step    fstr) f" forall  fstr f .
        _bind (Step fstr) f = Step (fmap (\p -> _bind p f) fstr);
    "_bind (Delay      m) f" forall m    f .
        _bind (Delay   m) f = Delay (m >>= \p -> return (_bind p f));
    "_bind (Return     r) f" forall r    f .
        _bind (Return  r) f = f r;
  #-}

  
  
instance (Functor f, Monad m) => Applicative (Stream f m) where
  pure = Return
  {-# INLINE pure #-}
  streamf <*> streamx = do {f <- streamf; x <- streamx; return (f x)}   
  {-# INLINABLE (<*>) #-}    
  stra0 *> strb = loop stra0 where
    loop stra = case stra of
      Return _ -> strb
      Delay m  -> Delay (do {stra' <- m ; return (stra' *> strb)})
      Step fstr -> Step (fmap (*> strb) fstr)
  {-# INLINABLE (*>) #-}    
  stra <* strb0 = loop strb0 where
    loop strb = case strb of
      Return _ -> stra
      Delay m  -> Delay (do {strb' <- m ; return (stra <* strb')})
      Step fstr -> Step (fmap (stra <*) fstr)
  {-# INLINABLE (<*) #-}    
    
instance Functor f => MonadTrans (Stream f) where
  lift = Delay . liftM Return
  {-# INLINE lift #-}

instance Functor f => MFunctor (Stream f) where
  hoist trans = loop . unexposed where
    loop stream = case stream of 
      Return r  -> Return r
      Delay m   -> Delay (trans (liftM loop m))
      Step f    -> Step (fmap loop f)
  {-# INLINABLE hoist #-}    

instance Functor f => MMonad (Stream f) where
  embed phi = loop where
    loop stream = case stream of
      Return r -> Return r
      Delay  m -> phi m >>= loop
      Step   f -> Step (fmap loop f)
  {-# INLINABLE embed #-}   

instance (MonadIO m, Functor f) => MonadIO (Stream f m) where
  liftIO = Delay . liftM Return . liftIO
  {-# INLINE liftIO #-}


{-| Map a stream directly to its church encoding; compare @Data.List.foldr@
    It permits distinctions that should be hidden, as can be seen from
    e.g. 

isPure stream = destroy_ (const True) (const False) (const True)

    and similar nonsense.  The crucial 
    constraint is that the @m x -> x@ argument is an /Eilenberg-Moore algebra/.
    See Atkey "Reasoning about Stream Processing with Effects"

    The destroy exported by the safe modules is 

destroy str = destroy (observe str)
-}
destroy
  :: (Functor f, Monad m) =>
     Stream f m r -> (f b -> b) -> (m b -> b) -> (r -> b) -> b
destroy stream0 construct effect done = loop (unexposed stream0) where
  loop stream = case stream of
    Return r -> done r
    Delay m  -> effect (liftM loop m)
    Step fs  -> construct (fmap loop fs)
{-# INLINABLE destroy #-}


{-| 'destroyWith' reorders the arguments of 'destroy' to be more akin
    to @foldr@  It is more convenient to query in ghci to figure out
    what kind of \'algebra\' you need to write.

>>> :t destroyWith join return
(Monad m, Functor f) => 
     (f (m a) -> m a) -> Stream f m a -> m a        -- iterT
>>> :t destroyWith (join . lift) return
(Monad m, Monad (t m), Functor f, MonadTrans t) =>
     (f (t m a) -> t m a) -> Stream f m a -> t m a  -- iterTM
>>> :t destroyWith effect return
(Monad m, Functor f, Functor f1) =>
     (f (Stream f1 m r) -> Stream f1 m r) -> Stream f m r -> Stream f1 m r
>>> :t destroyWith effect return (wrap . lazily)
Monad m => 
     Stream (Of a) m r -> Stream ((,) a) m r
>>> :t destroyWith effect return (wrap . strictly)
Monad m => 
     Stream ((,) a) m r -> Stream (Of a) m r
>>> :t destroyWith Data.ByteString.Streaming.effect return  
(Monad m, Functor f) =>
     (f (ByteString m r) -> ByteString m r) -> Stream f m r -> ByteString m r
>>> :t destroyWith Data.ByteString.Streaming.effect return (\(a:>b) -> consChunk a b) 
Monad m => 
     Stream (Of B.ByteString) m r -> ByteString m r -- fromChunks
-}
destroyWith
  :: (Functor f, Monad m) =>
     (m b -> b) -> (r -> b) -> (f b -> b) -> Stream f m r -> b
destroyWith effect done construct stream  = destroy stream construct effect done

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


-- | Map layers of one functor to another with a transformation
maps :: (Monad m, Functor f) 
     => (forall x . f x -> g x) -> Stream f m r -> Stream g m r
maps phi = loop where
  loop stream = case stream of 
    Return r  -> Return r
    Delay m   -> Delay (liftM loop m)
    Step f    -> Step (phi (fmap loop f))
{-# INLINABLE maps #-}

-- newtype NT g f = NT {runNT :: forall x . f x -> g x}
-- newtype NTM g m f = NTM {runNTM :: forall x . f x -> m (g x)}
-- compNTNT :: NT f g -> NT g h -> NT f h
-- compNTNT (NT f) (NT g) = NT (f . g)
-- compNTNTM :: Monad m => NT f g -> NTM g m h -> NTM f m h
-- compNTNTM (NT f) (NTM g) = NTM (liftM f . g)
-- compNTMNT :: Monad m =>  NTM f m g -> NT g h -> NTM f m h
-- compNTMNT (NTM f) (NT g) = NTM (f . g)
-- compNTMNTM ::  Monad m =>  NTM f m g -> NTM g m h -> NTM f m h
-- compNTMNTM (NTM f) (NTM g) = NTM (f <=< g)
--
-- {-# NOINLINE [0] mapsNT #-}
-- mapsNT :: (Functor f, Functor g, Monad m) => NT g f -> Stream f m r -> Stream g m r
-- mapsNT (NT phi) = loop where
--   loop stream = case stream of
--     Return r  -> Return r
--     Delay m   -> Delay (liftM loop m)
--     Step f    -> Step (phi (fmap loop f))

{- | Map layers of one functor to another with a transformation involving the base monad
     @maps@ is more fundamental than @mapsM@, which is best understood as a convenience
     for effecting this frequent composition:

> mapsM phi = decompose . maps (Compose . phi)
-}
mapsM :: (Monad m, Functor f) => (forall x . f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsM phi = loop where
  loop stream = case stream of 
    Return r  -> Return r
    Delay m   -> Delay (liftM loop m)
    Step f    -> Delay (liftM Step (phi (fmap loop f)))
{-# INLINABLE mapsM #-}

{-| Resort a succession of layers of the form @m (f x)@. Though @mapsM@ 
    is best understood as:

> mapsM phi = decompose . maps (Compose . phi)

   we could as well define @decompose@ by @mapsM@:

> decompose = mapsM getCompose

-}
decompose :: (Monad m, Functor f) => Stream (Compose m f) m r -> Stream f m r
decompose = loop where
  loop stream = case stream of 
    Return r -> Return r 
    Delay m ->  Delay (liftM loop m)
    Step (Compose mstr) -> Delay $ do
      str <- mstr
      return (Step (fmap loop str))


{-| Run the effects in a stream that merely layers effects.
-}
run :: Monad m => Stream m m r  -> m r
run = loop where
  loop stream = case stream of
    Return r -> return r
    Delay  m -> m >>= loop
    Step mrest -> mrest >>= loop
{-# INLINABLE run #-}


{-| Map each layer to an effect in the base monad, and run them all.
-}
mapsM_ :: (Functor f, Monad m) => (forall x . f x -> m x) -> Stream f m r -> m r
mapsM_ f str = run (maps f str)
{-# INLINABLE mapsM_ #-}


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

{-| Dissolves the segmentation into layers of @Stream f m@ layers.

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
concats :: (Monad m, Functor f) => Stream (Stream f m) m r -> Stream f m r
concats  = loop where
  loop stream = case stream of
    Return r -> return r
    Delay m  -> join $ lift (liftM loop m)
    Step fs  -> join (fmap loop fs)
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
                      
takes :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
takes n = void . splitsAt n
{-# INLINE takes #-}                        

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

{- | Make it possible to \'run\' the underlying transformed monad. 
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



hoistExposed trans = loop where
  loop stream = case stream of 
    Return r  -> Return r
    Delay m   -> Delay (trans (liftM loop m))
    Step f    -> Step (fmap loop f)

mapsExposed :: (Monad m, Functor f) 
     => (forall x . f x -> g x) -> Stream f m r -> Stream g m r
mapsExposed phi = loop where
  loop stream = case stream of 
    Return r  -> Return r
    Delay m   -> Delay (liftM loop m)
    Step f    -> Step (phi (fmap loop f))
{-# INLINABLE mapsExposed #-}

mapsMExposed phi = loop where
  loop stream = case stream of 
    Return r  -> Return r
    Delay m   -> Delay (liftM loop m)
    Step f    -> Delay (liftM Step (phi (fmap loop f)))
{-# INLINABLE mapsMExposed #-}

--     Map a stream directly to its church encoding; compare @Data.List.foldr@
--     It permits distinctions that should be hidden, as can be seen from
--     e.g.
--
-- isPure stream = destroy (const True) (const False) (const True)
--
--     and similar nonsense.  The crucial
--     constraint is that the @m x -> x@ argument is an /Eilenberg-Moore algebra/.
--     See Atkey "Reasoning about Stream Processing with Effects"


destroyExposed stream0 construct effect done = loop stream0 where
  loop stream = case stream of
    Return r -> done r
    Delay m  -> effect (liftM loop m)
    Step fs  -> construct (fmap loop fs)
{-# INLINABLE destroyExposed #-}


{-| This is akin to the @observe@ of @Pipes.Internal@ . It reeffects the layering
    in instances of @Stream f m r@ so that it replicates that of 
    @FreeT@. 

-}
unexposed :: (Functor f, Monad m) => Stream f m r -> Stream f m r
unexposed = Delay . loop where
  loop stream = case stream of 
    Return r -> return (Return r)
    Delay  m -> m >>= loop
    Step   f -> return (Step (fmap (Delay . loop) f))
{-# INLINABLE unexposed #-}   



effect :: (Monad m, Functor f ) => m (Stream f m r) -> Stream f m r
effect = Delay

wrap :: (Monad m, Functor f ) => f (Stream f m r) -> Stream f m r
wrap = Step


{-| Lift for items in the base functor. Makes a singleton or
    one-layer succession. It is named by similarity to lift: 

> lift :: (Monad m, Functor f)     => m r -> Stream f m r
> elevate ::  (Monad m, Functor f) => f r -> Stream f m r
-}

elevate ::  (Monad m, Functor f) => f r -> Stream f m r
elevate fr = Step (fmap Return fr)


zipsWith :: (Monad m, Functor h) 
  => (forall x y . f x -> g y -> h (x,y)) 
  -> Stream f m r -> Stream g m r -> Stream h m r
zipsWith phi = curry loop where
  loop (s1, s2) = Delay $ go s1 s2
  go (Return r)  p        = return $ Return r
  go q         (Return s) = return $ Return s
  go (Delay m) p          = m >>= \s -> go s p
  go q         (Delay m)  = m >>= go q
  go (Step f) (Step g)    = return $ Step $ fmap loop (phi f g)
{-# INLINABLE zipsWith #-}   
  
zips :: (Monad m, Functor f, Functor g) 
     => Stream f m r -> Stream g m r -> Stream (Compose f g) m r  
zips = zipsWith go where
  go fx gy = Compose (fmap (\x -> fmap (\y -> (x,y)) gy) fx)
{-# INLINE zips #-}   


{-| Interleave functor layers, with the effects of the first preceding
    the effects of the second.

> interleaves = zipsWith (liftA2 (,))

>>> let paste = \a b -> interleaves (Q.lines a) (maps (Q.cons' '\t') (Q.lines b))
>>> Q.stdout $ Q.unlines $ paste "hello\nworld\n" "goodbye\nworld\n"
hello	goodbye
world	world

-}
  
interleaves
  :: (Monad m, Applicative h) =>
     Stream h m r -> Stream h m r -> Stream h m r
interleaves = zipsWith (liftA2 (,))
{-# INLINE interleaves #-}   


eithers :: (Monad m, Applicative h) => 
    (forall x . f x -> h x) -> (forall x . g x -> h x) -> Stream (Sum f g) m r -> Stream h m r
eithers f g = loop where
  loop str = case str of 
    Return r -> Return r
    Delay m -> Delay (liftM loop m)
    Step str' -> case str' of 
      InL s -> Step (fmap loop (f s))
      InR t -> Step (fmap loop (g t))
      
  
  
  
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Streaming.Internal (
    -- * The free monad transformer
    -- $stream
    Stream (..)

    -- * Introducing a stream
    , unfold
    , replicates
    , repeats
    , repeatsM
    , effect
    , wrap
    , yields
    , streamBuild
    , cycles
    , delays
    , never
    , untilJust

    -- * Eliminating a stream
    , intercalates
    , concats
    , iterT
    , iterTM
    , destroy
    , streamFold

    -- * Inspecting a stream wrap by wrap
    , inspect

    -- * Transforming streams
    , maps
    , mapsM
    , mapsPost
    , mapsMPost
    , hoistUnexposed
    , decompose
    , mapsM_
    , run
    , distribute
    , groups
--    , groupInL

    -- *  Splitting streams
    , chunksOf
    , splitsAt
    , takes
    , cutoff
    -- , period
    -- , periods

    -- * Zipping and unzipping streams
    , zipsWith
    , zipsWith'
    , zips
    , unzips
    , interleaves
    , separate
    , unseparate
    , expand
    , expandPost


    -- * Assorted Data.Functor.x help
    , switch

    -- *  For use in implementation
    , unexposed
    , hoistExposed
    , hoistExposedPost
    , mapsExposed
    , mapsMExposed
    , destroyExposed

   ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Morph
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Data.Data (Typeable)
import Data.Function ( on )
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))

{- $stream

    The 'Stream' data type is equivalent to @FreeT@ and can represent any effectful
    succession of steps, where the form of the steps or 'commands' is
    specified by the first (functor) parameter.

> data Stream f m r = Step !(f (Stream f m r)) | Effect (m (Stream f m r)) | Return r

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
                  | Effect (m (Stream f m r))
                  | Return r
#if __GLASGOW_HASKELL__ >= 710
                  deriving (Typeable)
#endif

-- The most obvious approach would probably be
--
-- s1 == s2 = eqUnexposed (unexposed s1) (unexposed s2)
--
-- but that seems to actually be rather hard (especially if performance
-- matters even a little bit). Using `inspect` instead
-- is nice and simple. The main downside is the rather weird-looking
-- constraint it imposes. We *could* write
--
-- instance (Monad m, Eq r, Eq1 m, Eq1 f) => Eq (Stream f m r)
--
-- but there are an awful lot more Eq instances in the wild than
-- Eq1 instances. Maybe some day soon we'll have implication constraints
-- and everything will be beautiful.
instance (Monad m, Eq (m (Either r (f (Stream f m r)))))
         => Eq (Stream f m r) where
  s1 == s2 = inspect s1 == inspect s2

-- See the notes on Eq.
instance (Monad m, Ord (m (Either r (f (Stream f m r)))))
         => Ord (Stream f m r) where
  compare = compare `on` inspect
  (<) = (<) `on` inspect
  (>) = (>) `on` inspect
  (<=) = (<=) `on` inspect
  (>=) = (>=) `on` inspect

#if MIN_VERSION_base(4,9,0)

-- We could avoid a Show1 constraint for our Show1 instance by sneakily
-- mapping everything to a single known type, but there's really no way
-- to do that for Eq1 or Ord1.
instance (Monad m, Functor f, Eq1 m, Eq1 f) => Eq1 (Stream f m) where
  liftEq eq xs ys = liftEqExposed (unexposed xs) (unexposed ys)
    where
      liftEqExposed (Return x) (Return y) = eq x y
      liftEqExposed (Effect m) (Effect n) = liftEq liftEqExposed m n
      liftEqExposed (Step f) (Step g) = liftEq liftEqExposed f g
      liftEqExposed _ _ = False

instance (Monad m, Functor f, Ord1 m, Ord1 f) => Ord1 (Stream f m) where
  liftCompare cmp xs ys = liftCmpExposed (unexposed xs) (unexposed ys)
    where
      liftCmpExposed (Return x) (Return y) = cmp x y
      liftCmpExposed (Effect m) (Effect n) = liftCompare liftCmpExposed m n
      liftCmpExposed (Step f) (Step g) = liftCompare liftCmpExposed f g
      liftCmpExposed (Return _) _ = LT
      liftCmpExposed _ (Return _) = GT
      liftCmpExposed _ _ = error "liftCmpExposed: stream was exposed!"

#endif

-- We could get a much less scary implementation using Show1, but
-- Show1 instances aren't nearly as common as Show instances.
--
-- How does this
-- funny-looking instance work?
--
-- We 'inspect' the stream to produce @m (Either r (Stream f m r))@.
-- Then we work under @m@ to produce @m ShowSWrapper@. That's almost
-- like producing @m String@, except that a @ShowSWrapper@ can be
-- shown at any precedence. So the 'Show' instance for @m@ can show
-- the contents at the correct precedence.
instance (Monad m, Show r, Show (m ShowSWrapper), Show (f (Stream f m r)))
         => Show (Stream f m r) where
  showsPrec p xs = showParen (p > 10) $
                     showString "Effect " . (showsPrec 11 $
    flip fmap (inspect xs) $ \front ->
      SS $ \d -> showParen (d > 10) $
        case front of
          Left r ->  showString "Return " . showsPrec 11 r
          Right f -> showString "Step "   . showsPrec 11 f)

#if MIN_VERSION_base(4,9,0)

instance (Monad m, Functor f, Show (m ShowSWrapper), Show (f ShowSWrapper))
         => Show1 (Stream f m) where
  liftShowsPrec sp sl p xs = showParen (p > 10) $
                     showString "Effect " . (showsPrec 11 $
    flip fmap (inspect xs) $ \front ->
      SS $ \d -> showParen (d > 10) $
        case front of
          Left r ->  showString "Return " . sp 11 r
          Right f -> showString "Step "   .
                     showsPrec 11 (fmap (SS . (\str i -> liftShowsPrec sp sl i str)) f))

#endif

newtype ShowSWrapper = SS (Int -> ShowS)
instance Show ShowSWrapper where
  showsPrec p (SS s) = s p

instance (Functor f, Monad m) => Functor (Stream f m) where
  fmap f = loop where
    loop stream = case stream of
      Return r -> Return (f r)
      Effect m  -> Effect (do {stream' <- m; return (loop stream')})
      Step g -> Step (fmap loop g)
  {-# INLINABLE fmap #-}
  a <$ stream0 = loop stream0 where
    loop stream = case stream of
      Return _ -> Return a
      Effect m -> Effect (do {stream' <- m; return (loop stream')})
      Step f -> Step (fmap loop f)
  {-# INLINABLE (<$) #-}

instance (Functor f, Monad m) => Monad (Stream f m) where
  return = Return
  {-# INLINE return #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}
  -- (>>=) = _bind
  -- {-# INLINE (>>=) #-}
  --
  stream >>= f =
    loop stream where
    loop stream0 = case stream0 of
      Step fstr -> Step (fmap loop fstr)
      Effect m   -> Effect (fmap loop m)
      Return r  -> f r
  {-# INLINABLE (>>=) #-}

  fail = lift . fail
  {-# INLINE fail #-}


-- _bind
--     :: (Functor f, Monad m)
--     => Stream f m r
--     -> (r -> Stream f m s)
--     -> Stream f m s
-- _bind p0 f = go p0 where
--     go p = case p of
--       Step fstr  -> Step (fmap go fstr)
--       Effect m   -> Effect (m >>= \s -> return (go s))
--       Return r  -> f r
-- {-# INLINABLE _bind #-}
--
-- see https://github.com/Gabriel439/Haskell-Pipes-Library/pull/163
-- for a plan to delay inlining and manage interaction with other operations.

-- {-# RULES
    -- "_bind (Step    fstr) f" forall  fstr f .
    --     _bind (Step fstr) f = Step (fmap (\p -> _bind p f) fstr);
    -- "_bind (Effect      m) f" forall m    f .
    --     _bind (Effect   m) f = Effect (m >>= \p -> return (_bind p f));
    -- "_bind (Return     r) f" forall r    f .
    --     _bind (Return  r) f = f r;
--  #-}

instance (Functor f, Monad m) => Applicative (Stream f m) where
  pure = Return
  {-# INLINE pure #-}
  streamf <*> streamx = do {f <- streamf; x <- streamx; return (f x)}
  {-# INLINE (<*>) #-}
  stream1 *> stream2 = loop stream1 where
    loop stream = case stream of
      Return _ -> stream2
      Effect m  -> Effect (fmap loop m)
      Step f   -> Step (fmap loop f)
  {-# INLINABLE (*>) #-}


{- | The 'Alternative' instance glues streams together stepwise.

> empty = never
> (<|>) = zipsWith (liftA2 (,))

   See also 'never', 'untilJust' and 'delays'
-}
instance (Applicative f, Monad m) => Alternative (Stream f m) where
  empty = never
  {-# INLINE empty #-}

  str <|> str' = zipsWith' liftA2 str str'
  {-# INLINE (<|>) #-}

instance (Functor f, Monad m, Semigroup w) => Semigroup (Stream f m w) where
  a <> b = a >>= \w -> fmap (w <>) b
  {-# INLINE (<>) #-}

instance (Functor f, Monad m, Monoid w) => Monoid (Stream f m w) where
  mempty = return mempty
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend a b = a >>= \w -> fmap (w `mappend`) b
  {-# INLINE mappend #-}
#endif

instance (Applicative f, Monad m) => MonadPlus (Stream f m) where
  mzero = empty
  mplus = (<|>)

instance Functor f => MonadTrans (Stream f) where
  lift = Effect . fmap Return
  {-# INLINE lift #-}

instance Functor f => MFunctor (Stream f) where
  hoist trans = loop  where
    loop stream = case stream of
      Return r  -> Return r
      Effect m   -> Effect (trans (fmap loop m))
      Step f    -> Step (fmap loop f)
  {-# INLINABLE hoist #-}


instance Functor f => MMonad (Stream f) where
  embed phi = loop where
    loop stream = case stream of
      Return r -> Return r
      Effect  m -> phi m >>= loop
      Step   f -> Step (fmap loop f)
  {-# INLINABLE embed #-}

instance (MonadIO m, Functor f) => MonadIO (Stream f m) where
  liftIO = Effect . fmap Return . liftIO
  {-# INLINE liftIO #-}

instance (Functor f, MonadReader r m) => MonadReader r (Stream f m) where
  ask = lift ask
  {-# INLINE ask #-}
  local f = hoist (local f)
  {-# INLINE local #-}

instance (Functor f, MonadState s m) => MonadState s (Stream f m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}
#if MIN_VERSION_mtl(2,1,1)
  state f = lift (state f)
  {-# INLINE state #-}
#endif

instance (Functor f, MonadError e m) => MonadError e (Stream f m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}
  str `catchError` f = loop str where
    loop x = case x of
      Return r -> Return r
      Effect m -> Effect $ fmap loop m `catchError` (return . f)
      Step g -> Step (fmap loop g)
  {-# INLINABLE catchError #-}

{-| Map a stream to its church encoding; compare @Data.List.foldr@.
    'destroyExposed' may be more efficient in some cases when
    applicable, but it is less safe.

    @
    destroy s construct eff done
      = eff . iterT (return . construct . fmap eff) . fmap done $ s
    @
-}
destroy
  :: (Functor f, Monad m) =>
     Stream f m r -> (f b -> b) -> (m b -> b) -> (r -> b) -> b
destroy stream0 construct theEffect done = theEffect (loop stream0) where
  loop stream = case stream of
    Return r -> return (done r)
    Effect m -> m >>= loop
    Step fs -> return (construct (fmap (theEffect . loop) fs))
{-# INLINABLE destroy #-}


{-| 'streamFold' reorders the arguments of 'destroy' to be more akin
    to @foldr@  It is more convenient to query in ghci to figure out
    what kind of \'algebra\' you need to write.

>>> :t streamFold return join
(Monad m, Functor f) =>
     (f (m a) -> m a) -> Stream f m a -> m a        -- iterT

>>> :t streamFold return (join . lift)
(Monad m, Monad (t m), Functor f, MonadTrans t) =>
     (f (t m a) -> t m a) -> Stream f m a -> t m a  -- iterTM

>>> :t streamFold return effect
(Monad m, Functor f, Functor g) =>
     (f (Stream g m r) -> Stream g m r) -> Stream f m r -> Stream g m r

>>> :t \f -> streamFold return effect (wrap . f)
(Monad m, Functor f, Functor g) =>
     (f (Stream g m a) -> g (Stream g m a))
     -> Stream f m a -> Stream g m a                 -- maps

>>> :t \f -> streamFold return effect (effect . fmap wrap . f)
(Monad m, Functor f, Functor g) =>
     (f (Stream g m a) -> m (g (Stream g m a)))
     -> Stream f m a -> Stream g m a                 -- mapped

@
    streamFold done eff construct
       = eff . iterT (return . construct . fmap eff) . fmap done
@
-}
streamFold
  :: (Functor f, Monad m) =>
     (r -> b) -> (m b -> b) ->  (f b -> b) -> Stream f m r -> b
streamFold done theEffect construct stream  = destroy stream construct theEffect done
{-# INLINE streamFold #-}

{- | Reflect a church-encoded stream; cp. @GHC.Exts.build@

> streamFold return_ effect_ step_ (streamBuild psi)  = psi return_ effect_ step_
-}
streamBuild
  :: (forall b . (r -> b) -> (m b -> b) -> (f b -> b) ->  b) ->  Stream f m r
streamBuild = \phi -> phi Return Effect Step
{-# INLINE streamBuild #-}


{-| Inspect the first stage of a freely layered sequence.
    Compare @Pipes.next@ and the replica @Streaming.Prelude.next@.
    This is the 'uncons' for the general 'unfold'.

> unfold inspect = id
> Streaming.Prelude.unfoldr StreamingPrelude.next = id
-}
inspect :: Monad m =>
     Stream f m r -> m (Either r (f (Stream f m r)))
inspect = loop where
  loop stream = case stream of
    Return r -> return (Left r)
    Effect m  -> m >>= loop
    Step fs  -> return (Right fs)
{-# INLINABLE inspect #-}

{-| Build a @Stream@ by unfolding steps starting from a seed. See also
    the specialized 'Streaming.Prelude.unfoldr' in the prelude.

> unfold inspect = id -- modulo the quotient we work with
> unfold Pipes.next :: Monad m => Producer a m r -> Stream ((,) a) m r
> unfold (curry (:>) . Pipes.next) :: Monad m => Producer a m r -> Stream (Of a) m r

-}
unfold :: (Monad m, Functor f)
        => (s -> m (Either r (f s)))
        -> s -> Stream f m r
unfold step = loop where
  loop s0 = Effect $ do
    e <- step s0
    case e of
      Left r -> return (Return r)
      Right fs -> return (Step (fmap loop fs))
{-# INLINABLE unfold #-}


{- | Map layers of one functor to another with a transformation. Compare
     hoist, which has a similar effect on the 'monadic' parameter.

> maps id = id
> maps f . maps g = maps (f . g)

-}
maps :: (Monad m, Functor f)
     => (forall x . f x -> g x) -> Stream f m r -> Stream g m r
maps phi = loop where
  loop stream = case stream of
    Return r  -> Return r
    Effect m   -> Effect (fmap loop m)
    Step f    -> Step (phi (fmap loop f))
{-# INLINABLE maps #-}


{- | Map layers of one functor to another with a transformation involving the base monad.
     'maps' is more fundamental than @mapsM@, which is best understood as a convenience
     for effecting this frequent composition:

> mapsM phi = decompose . maps (Compose . phi)

     The streaming prelude exports the same function under the better name @mapped@,
     which overlaps with the lens libraries.

-}
mapsM :: (Monad m, Functor f) => (forall x . f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsM phi = loop where
  loop stream = case stream of
    Return r  -> Return r
    Effect m   -> Effect (fmap loop m)
    Step f    -> Effect (fmap Step (phi (fmap loop f)))
{-# INLINABLE mapsM #-}

{- | Map layers of one functor to another with a transformation. Compare
     hoist, which has a similar effect on the 'monadic' parameter.

> mapsPost id = id
> mapsPost f . mapsPost g = mapsPost (f . g)
> mapsPost f = mapsPost f


     @mapsPost@ is essentially the same as 'maps', but it imposes a 'Functor' constraint on
     its target functor rather than its source functor. It should be preferred if 'fmap'
     is cheaper for the target functor than for the source functor.
-}
mapsPost :: forall m f g r. (Monad m, Functor g)
         => (forall x. f x -> g x)
         -> Stream f m r -> Stream g m r
mapsPost phi = loop where
  loop :: Stream f m r -> Stream g m r
  loop stream = case stream of
    Return r -> Return r
    Effect m -> Effect (fmap loop m)
    Step f -> Step $ fmap loop $ phi f
{-# INLINABLE mapsPost #-}

{- | Map layers of one functor to another with a transformation involving the base monad.
     @mapsMPost@ is essentially the same as 'mapsM', but it imposes a 'Functor' constraint on
     its target functor rather than its source functor. It should be preferred if 'fmap'
     is cheaper for the target functor than for the source functor.

     @mapsPost@ is more fundamental than @mapsMPost@, which is best understood as a convenience
     for effecting this frequent composition:

> mapsMPost phi = decompose . mapsPost (Compose . phi)

     The streaming prelude exports the same function under the better name @mappedPost@,
     which overlaps with the lens libraries.

-}
mapsMPost :: forall m f g r. (Monad m, Functor g)
       => (forall x. f x -> m (g x))
       -> Stream f m r -> Stream g m r
mapsMPost phi = loop where
  loop :: Stream f m r -> Stream g m r
  loop stream = case stream of
    Return r -> Return r
    Effect m -> Effect (fmap loop m)
    Step f -> Effect $ fmap (Step . fmap loop) (phi f)
{-# INLINABLE mapsMPost #-}

{-| Rearrange a succession of layers of the form @Compose m (f x)@.

   we could as well define @decompose@ by @mapsM@:

> decompose = mapped getCompose

  but @mapped@ is best understood as:

> mapped phi = decompose . maps (Compose . phi)

  since @maps@ and @hoist@ are the really fundamental operations that preserve the
  shape of the stream:

> maps  :: (Monad m, Functor f) => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
> hoist :: (Monad m, Functor f) => (forall a. m a -> n a) -> Stream f m r -> Stream f n r

-}
decompose :: (Monad m, Functor f) => Stream (Compose m f) m r -> Stream f m r
decompose = loop where
  loop stream = case stream of
    Return r -> Return r
    Effect m ->  Effect (fmap loop m)
    Step (Compose mstr) -> Effect $ do
      str <- mstr
      return (Step (fmap loop str))


{-| Run the effects in a stream that merely layers effects.
-}
run :: Monad m => Stream m m r -> m r
run = loop where
  loop stream = case stream of
    Return r -> return r
    Effect  m -> m >>= loop
    Step mrest -> mrest >>= loop
{-# INLINABLE run #-}


{-| Map each layer to an effect, and run them all.
-}
mapsM_ :: (Functor f, Monad m) => (forall x . f x -> m x) -> Stream f m r -> m r
mapsM_ f = run . maps f
{-# INLINE mapsM_ #-}


{-| Interpolate a layer at each segment. This specializes to e.g.

> intercalates :: (Monad m, Functor f) => Stream f m () -> Stream (Stream f m) m r -> Stream f m r
-}
intercalates :: (Monad m, Monad (t m), MonadTrans t) =>
     t m x -> Stream (t m) m r -> t m r
intercalates sep = go0
  where
    go0 f = case f of
      Return r -> return r
      Effect m -> lift m >>= go0
      Step fstr -> do
        f' <- fstr
        go1 f'
    go1 f = case f of
      Return r -> return r
      Effect m     -> lift m >>= go1
      Step fstr ->  do
        _ <- sep
        f' <- fstr
        go1 f'
{-# INLINABLE intercalates #-}

{-| Specialized fold following the usage of @Control.Monad.Trans.Free@

> iterTM alg = streamFold return (join . lift)
> iterTM alg = iterT alg . hoist lift
-}
iterTM ::
  (Functor f, Monad m, MonadTrans t,
   Monad (t m)) =>
  (f (t m a) -> t m a) -> Stream f m a -> t m a
iterTM out stream = destroyExposed stream out (join . lift) return
{-# INLINE iterTM #-}

{-| Specialized fold following the usage of @Control.Monad.Trans.Free@

> iterT alg = streamFold return join alg
> iterT alg = runIdentityT . iterTM (IdentityT . alg . fmap runIdentityT)
-}
iterT ::
  (Functor f, Monad m) => (f (m a) -> m a) -> Stream f m a -> m a
iterT out stream = destroyExposed stream out join return
{-# INLINE iterT #-}

{-| Dissolves the segmentation into layers of @Stream f m@ layers.

-}
concats :: (Monad m, Functor f) => Stream (Stream f m) m r -> Stream f m r
concats  = loop where
  loop stream = case stream of
    Return r -> return r
    Effect m  -> lift m >>= loop
    Step fs  -> fs >>= loop
{-# INLINE concats #-}

{-| Split a succession of layers after some number, returning a streaming or
    effectful pair.

>>> rest <- S.print $ S.splitAt 1 $ each [1..3]
1
>>> S.print rest
2
3

> splitAt 0 = return
> splitAt n >=> splitAt m = splitAt (m+n)

    Thus, e.g.

>>> rest <- S.print $ splitsAt 2 >=> splitsAt 2 $ each [1..5]
1
2
3
4
>>> S.print rest
5

-}
splitsAt :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
splitsAt  = loop  where
  loop !n stream
    | n <= 0 = Return stream
    | otherwise = case stream of
        Return r       -> Return (Return r)
        Effect m        -> Effect (fmap (loop n) m)
        Step fs        -> case n of
          0 -> Return (Step fs)
          _ -> Step (fmap (loop (n-1)) fs)
{-# INLINABLE splitsAt #-}

{- Functor-general take.

   @takes 3@ can take three individual values

>>> S.print $ takes 3 $ each [1..]
1
2
3


    or three sub-streams

>>> S.print $ mapped S.toList $ takes 3 $ chunksOf 2 $ each [1..]
[1,2]
[3,4]
[5,6]

   Or, using 'Data.ByteString.Streaming.Char' (here called @Q@),
   three byte streams.

>>> Q.stdout $ Q.unlines $ takes 3 $ Q.lines $ Q.chunk "a\nb\nc\nd\ne\nf"
a
b
c

-}
takes :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
takes n = void . splitsAt n
{-# INLINE takes #-}

{-| Break a stream into substreams each with n functorial layers.

>>>  S.print $ mapped S.sum $ chunksOf 2 $ each [1,1,1,1,1]
2
2
1
-}
chunksOf :: (Monad m, Functor f) => Int -> Stream f m r -> Stream (Stream f m) m r
chunksOf n0 = loop where
  loop stream = case stream of
    Return r  -> Return r
    Effect m  -> Effect (fmap loop m)
    Step fs   -> Step (Step (fmap (fmap loop . splitsAt (n0-1)) fs))
{-# INLINABLE chunksOf #-}

{- | Make it possible to \'run\' the underlying transformed monad.
-}
distribute :: (Monad m, Functor f, MonadTrans t, MFunctor t, Monad (t (Stream f m)))
           => Stream f (t m) r -> t (Stream f m) r
distribute = loop where
  loop stream = case stream of
    Return r     -> lift (Return r)
    Effect tmstr -> hoist lift tmstr >>= loop
    Step fstr    -> join (lift (Step (fmap (Return . loop) fstr)))
{-# INLINABLE distribute #-}

-- | Repeat a functorial layer (a \"command\" or \"instruction\") forever.
repeats :: (Monad m, Functor f) => f () -> Stream f m r
repeats f = loop where
  loop = Effect (return (Step (fmap (\_ -> loop) f)))

-- | Repeat an effect containing a functorial layer, command or instruction forever.
repeatsM :: (Monad m, Functor f) => m (f ()) -> Stream f m r
repeatsM mf = loop where
  loop = Effect $ do
     f <- mf
     return $ Step $ fmap (\_ -> loop) f

{- | Repeat a functorial layer, command or instruction a fixed number of times.

> replicates n = takes n . repeats
-}
replicates :: (Monad m, Functor f) => Int -> f () -> Stream f m ()
replicates n f = splitsAt n (repeats f) >> return ()

{-| Construct an infinite stream by cycling a finite one

> cycles = forever

>>>
-}

cycles :: (Monad m, Functor f) =>  Stream f m () -> Stream f m r
cycles = forever

-- | A less-efficient version of 'hoist' that works properly even when its
-- argument is not a monad morphism.
--
-- > hoistUnexposed = hoist . unexposed
hoistUnexposed :: (Monad m, Functor f)
               => (forall a. m a -> n a)
               -> Stream f m r -> Stream f n r
hoistUnexposed trans = loop where
  loop = Effect . trans . inspectC (return . Return) (return . Step . fmap loop)
{-# INLINABLE hoistUnexposed #-}

-- A version of 'inspect' that takes explicit continuations.
inspectC :: Monad m => (r -> m a) -> (f (Stream f m r) -> m a) -> Stream f m r -> m a
inspectC f g = loop where
  loop (Return r) = f r
  loop (Step x) = g x
  loop (Effect m) = m >>= loop
{-# INLINE inspectC #-}

-- | The same as 'hoist', but explicitly named to indicate that it
-- is not entirely safe. In particular, its argument must be a monad
-- morphism.
hoistExposed :: (Functor m, Functor f) => (forall b. m b -> n b) -> Stream f m a -> Stream f n a
hoistExposed trans = loop where
  loop stream = case stream of
    Return r  -> Return r
    Effect m   -> Effect (trans (fmap loop m))
    Step f    -> Step (fmap loop f)
{-# INLINABLE hoistExposed #-}

-- | The same as 'hoistExposed', but with a 'Functor' constraint on
-- the target rather than the source. This must be used only with
-- a monad morphism.
hoistExposedPost :: (Functor n, Functor f) => (forall b. m b -> n b) -> Stream f m a -> Stream f n a
hoistExposedPost trans = loop where
  loop stream = case stream of
    Return r -> Return r
    Effect m -> Effect (fmap loop (trans m))
    Step f -> Step (fmap loop f)
{-# INLINABLE hoistExposedPost #-}

{-# DEPRECATED mapsExposed "Use maps instead." #-}
mapsExposed :: (Monad m, Functor f)
     => (forall x . f x -> g x) -> Stream f m r -> Stream g m r
mapsExposed = maps
{-# INLINABLE mapsExposed #-}

{-# DEPRECATED mapsMExposed "Use mapsM instead." #-}
mapsMExposed :: (Monad m, Functor f)
     => (forall x . f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsMExposed = mapsM
{-# INLINABLE mapsMExposed #-}

{-| Map a stream directly to its church encoding; compare @Data.List.foldr@
    It permits distinctions that should be hidden, as can be seen from
    e.g.

    @isPure stream = destroyExposed (const True) (const False) (const True)@

    and similar nonsense.  The crucial
    constraint is that the @m x -> x@ argument is an /Eilenberg-Moore algebra/.
    See Atkey, "Reasoning about Stream Processing with Effects"

    When in doubt, use 'destroy' instead.
-}
destroyExposed
  :: (Functor f, Monad m) =>
     Stream f m r -> (f b -> b) -> (m b -> b) -> (r -> b) -> b
destroyExposed stream0 construct theEffect done = loop stream0 where
  loop stream = case stream of
    Return r -> done r
    Effect m  -> theEffect (fmap loop m)
    Step fs  -> construct (fmap loop fs)
{-# INLINABLE destroyExposed #-}


{-| This is akin to the @observe@ of @Pipes.Internal@ . It reeffects the layering
    in instances of @Stream f m r@ so that it replicates that of
    @FreeT@.

-}
unexposed :: (Functor f, Monad m) => Stream f m r -> Stream f m r
unexposed = Effect . loop where
  loop stream = case stream of
    Return r -> return (Return r)
    Effect  m -> m >>= loop
    Step   f -> return (Step (fmap (Effect . loop) f))
{-# INLINABLE unexposed #-}


{-| Wrap a new layer of a stream. So, e.g.

> S.cons :: Monad m => a -> Stream (Of a) m r -> Stream (Of a) m r
> S.cons a str = wrap (a :> str)

   and, recursively:

> S.each :: (Monad m, Foldable t) => t a -> Stream (Of a) m ()
> S.each = foldr (\a b -> wrap (a :> b)) (return ())

   The two operations

> wrap :: (Monad m, Functor f )   => f (Stream f m r) -> Stream f m r
> effect :: (Monad m, Functor f ) => m (Stream f m r) -> Stream f m r

   are fundamental. We can define the parallel operations @yields@ and @lift@ in
   terms of them

> yields :: (Monad m, Functor f )  => f r -> Stream f m r
> yields = wrap . fmap return
> lift ::  (Monad m, Functor f )   => m r -> Stream f m r
> lift = effect . fmap return

-}
wrap :: (Monad m, Functor f ) => f (Stream f m r) -> Stream f m r
wrap = Step
{-# INLINE wrap #-}


{- | Wrap an effect that returns a stream

> effect = join . lift

-}
effect :: (Monad m, Functor f ) => m (Stream f m r) -> Stream f m r
effect = Effect
{-# INLINE effect #-}


{-| @yields@ is like @lift@ for items in the streamed functor.
    It makes a singleton or one-layer succession.

> lift :: (Monad m, Functor f)    => m r -> Stream f m r
> yields ::  (Monad m, Functor f) => f r -> Stream f m r

    Viewed in another light, it is like a functor-general version of @yield@:

> S.yield a = yields (a :> ())

-}

yields ::  (Monad m, Functor f) => f r -> Stream f m r
yields fr = Step (fmap Return fr)
{-# INLINE yields #-}

{-
Note that if the first stream produces Return, we don't inspect
(and potentially run effects from) the second stream. We used to
do that. Aside from being (arguably) a bit strange, this also runs
into a bit of trouble with MonadPlus laws. Most MonadPlus instances
try to satisfy either left distribution or left catch. Let's first
consider left distribution:

(x <|> y) >>= k = (x >>= k) <|> (y >>= k)

[xy_1, xy_2, xy_3, ..., xy_o | r_xy] >>= k
=
[x_1,  x_2,  x_3, ..., x_m | r_x] >>= k
<|>
[y_1,  y_2,  y_3, ..., y_n | r_y] >>= k

x and y may have different lengths, and k may produce an utterly
arbitrary stream from each result, so left distribution seems
quite hopeless.

Now let's consider left catch:

zipsWith' liftA2 (return a) b = return a

To satisfy this, we can't run any effects from the second stream
if the first is finished.
-}

-- | Zip two streams together. The 'zipsWith'' function should generally
-- be preferred for efficiency.
zipsWith :: forall f g h m r. (Monad m, Functor h)
  => (forall x y . f x -> g y -> h (x,y))
  -> Stream f m r -> Stream g m r -> Stream h m r
zipsWith phi = zipsWith' $ \xyp fx gy -> (\(x,y) -> xyp x y) <$> phi fx gy
{-# INLINABLE zipsWith #-}
-- Somewhat surprisingly, GHC is *much* more willing to specialize
-- zipsWith if it's defined in terms of zipsWith'. Fortunately, zipsWith'
-- seems like a better function anyway, so I guess that's not a big problem.

-- | Zip two streams together.
zipsWith' :: forall f g h m r. Monad m
  => (forall x y p . (x -> y -> p) -> f x -> g y -> h p)
  -> Stream f m r -> Stream g m r -> Stream h m r
zipsWith' phi = loop
  where
    loop :: Stream f m r -> Stream g m r -> Stream h m r
    loop s t = case s of
       Return r -> Return r
       Step fs -> case t of
         Return r -> Return r
         Step gs -> Step $ phi loop fs gs
         Effect n -> Effect $ fmap (loop s) n
       Effect m -> Effect $ fmap (flip loop t) m
{-# INLINABLE zipsWith' #-}

zips :: (Monad m, Functor f, Functor g)
     => Stream f m r -> Stream g m r -> Stream (Compose f g) m r
zips = zipsWith' go where
  go p fx gy = Compose (fmap (\x -> fmap (\y -> p x y) gy) fx)
{-# INLINE zips #-}



{-| Interleave functor layers, with the effects of the first preceding
    the effects of the second. When the first stream runs out, any remaining
    effects in the second are ignored.

> interleaves = zipsWith (liftA2 (,))

>>> let paste = \a b -> interleaves (Q.lines a) (maps (Q.cons' '\t') (Q.lines b))
>>> Q.stdout $ Q.unlines $ paste "hello\nworld\n" "goodbye\nworld\n"
hello	goodbye
world	world

-}

interleaves
  :: (Monad m, Applicative h) =>
     Stream h m r -> Stream h m r -> Stream h m r
interleaves = zipsWith' liftA2
{-# INLINE interleaves #-}


{-| Swap the order of functors in a sum of functors.

>>> S.toList $ S.print $ separate $ maps S.switch $ maps (S.distinguish (=='a')) $ S.each "banana"
'a'
'a'
'a'
"bnn" :> ()
>>> S.toList $ S.print $ separate $ maps (S.distinguish (=='a')) $ S.each "banana"
'b'
'n'
'n'
"aaa" :> ()
-}
switch :: Sum f g r -> Sum g f r
switch s = case s of InL a -> InR a; InR a -> InL a
{-# INLINE switch #-}



{-| Given a stream on a sum of functors, make it a stream on the left functor,
    with the streaming on the other functor as the governing monad. This is
    useful for acting on one or the other functor with a fold, leaving the
    other material for another treatment. It generalizes
    'Data.Either.partitionEithers', but actually streams properly.

>>> let odd_even = S.maps (S.distinguish even) $ S.each [1..10::Int]
>>> :t separate odd_even
separate odd_even
  :: Monad m => Stream (Of Int) (Stream (Of Int) m) ()

    Now, for example, it is convenient to fold on the left and right values separately:

>>>  S.toList $ S.toList $ separate odd_even
[2,4,6,8,10] :> ([1,3,5,7,9] :> ())


   Or we can write them to separate files or whatever:

>>> S.writeFile "even.txt" . S.show $ S.writeFile "odd.txt" . S.show $ S.separate odd_even
>>> :! cat even.txt
2
4
6
8
10
>>> :! cat odd.txt
1
3
5
7
9

   Of course, in the special case of @Stream (Of a) m r@, we can achieve the above
   effects more simply by using 'Streaming.Prelude.copy'

>>> S.toList . S.filter even $ S.toList . S.filter odd $ S.copy $ each [1..10::Int]
[2,4,6,8,10] :> ([1,3,5,7,9] :> ())


    But 'separate' and 'unseparate' are functor-general.

-}

separate :: (Monad m, Functor f, Functor g) => Stream (Sum f g) m r -> Stream f (Stream g m) r
separate str = destroyExposed
  str
  (\x -> case x of InL fss -> wrap fss; InR gss -> effect (yields gss))
  (effect . lift)
  return
{-# INLINABLE separate #-}



unseparate :: (Monad m, Functor f, Functor g) =>  Stream f (Stream g m) r -> Stream (Sum f g) m r
unseparate str = destroyExposed
  str
  (wrap . InL)
  (join . maps InR)
  return
{-# INLINABLE unseparate #-}

-- | If 'Of' had a @Comonad@ instance, then we'd have
--
-- @copy = expand extend@
--
-- See 'expandPost' for a version that requires a @Functor g@
-- instance instead.
expand :: (Monad m, Functor f)
       => (forall a b. (g a -> b) -> f a -> h b)
       -> Stream f m r -> Stream g (Stream h m) r
expand ext = loop where
  loop (Return r) = Return r
  loop (Step f) = Effect $ Step $ ext (Return . Step) (fmap loop f)
  loop (Effect m) = Effect $ Effect $ fmap (Return . loop) m
{-# INLINABLE expand #-}

-- | If 'Of' had a @Comonad@ instance, then we'd have
--
-- @copy = expandPost extend@
--
-- See 'expand' for a version that requires a @Functor f@ instance
-- instead.
expandPost :: (Monad m, Functor g)
       => (forall a b. (g a -> b) -> f a -> h b)
       -> Stream f m r -> Stream g (Stream h m) r
expandPost ext = loop where
  loop (Return r) = Return r
  loop (Step f) = Effect $ Step $ ext (Return . Step . fmap loop) f
  loop (Effect m) = Effect $ Effect $ fmap (Return . loop) m
{-# INLINABLE expandPost #-}

unzips :: (Monad m, Functor f, Functor g) =>
   Stream (Compose f g) m r ->  Stream f (Stream g m) r
unzips str = destroyExposed
  str
  (\(Compose fgstr) -> Step (fmap (Effect . yields) fgstr))
  (Effect . lift)
  return
{-# INLINABLE unzips #-}

{-| Group layers in an alternating stream into adjoining sub-streams
    of one type or another.
-}
groups :: (Monad m, Functor f, Functor g)
           => Stream (Sum f g) m r
           -> Stream (Sum (Stream f m) (Stream g m)) m r
groups = loop
  where
  loop str = do
    e <- lift $ inspect str
    case e of
      Left r -> return r
      Right ostr -> case ostr of
        InR gstr -> wrap $ InR (fmap loop (cleanR (wrap (InR gstr))))
        InL fstr -> wrap $ InL (fmap loop (cleanL (wrap (InL fstr))))

  cleanL  :: (Monad m, Functor f, Functor g) =>
       Stream (Sum f g) m r -> Stream f m (Stream (Sum f g) m r)
  cleanL = go where
    go s = do
     e <- lift $ inspect s
     case e of
      Left r           -> return (return r)
      Right (InL fstr) -> wrap (fmap go fstr)
      Right (InR gstr) -> return (wrap (InR gstr))

  cleanR  :: (Monad m, Functor f, Functor g) =>
       Stream (Sum f g) m r -> Stream g m (Stream (Sum f g) m r)
  cleanR = go where
    go s = do
     e <- lift $ inspect s
     case e of
      Left r           -> return (return r)
      Right (InL fstr) -> return (wrap (InL fstr))
      Right (InR gstr) -> wrap (fmap go gstr)
{-# INLINABLE groups #-}

-- groupInL :: (Monad m, Functor f, Functor g)
--                      => Stream (Sum f g) m r
--                      -> Stream (Sum (Stream f m) g) m r
-- groupInL = loop
--   where
--   loop str = do
--     e <- lift $ inspect str
--     case e of
--       Left r -> return r
--       Right ostr -> case ostr of
--         InR gstr -> wrap $ InR (fmap loop gstr)
--         InL fstr -> wrap $ InL (fmap loop (cleanL (wrap (InL fstr))))
--   cleanL  :: (Monad m, Functor f, Functor g) =>
--        Stream (Sum f g) m r -> Stream f m (Stream (Sum f g) m r)
--   cleanL = loop where
--     loop s = dos
--      e <- lift $ inspect s
--      case e of
--       Left r           -> return (return r)
--       Right (InL fstr) -> wrap (fmap loop fstr)
--       Right (InR gstr) -> return (wrap (InR gstr))

{- | 'never' interleaves the pure applicative action with the return of the monad forever.
     It is the 'empty' of the 'Alternative' instance, thus

> never <|> a = a
> a <|> never = a

     and so on. If w is a monoid then @never :: Stream (Of w) m r@ is
     the infinite sequence of 'mempty', and
     @str1 \<|\> str2@ appends the elements monoidally until one of streams ends.
     Thus we have, e.g.

>>> S.stdoutLn $ S.take 2 $ S.stdinLn <|> S.repeat " " <|> S.stdinLn  <|> S.repeat " " <|> S.stdinLn
1<Enter>
2<Enter>
3<Enter>
1 2 3
4<Enter>
5<Enter>
6<Enter>
4 5 6

    This is equivalent to

>>> S.stdoutLn $ S.take 2 $ foldr (<|>) never [S.stdinLn, S.repeat " ", S.stdinLn, S.repeat " ", S.stdinLn ]

     Where 'f' is a monad, @(\<|\>)@ sequences the conjoined streams stepwise. See the
     definition of @paste@ <https://gist.github.com/michaelt/6c6843e6dd8030e95d58 here>,
     where the separate steps are bytestreams corresponding to the lines of a file.

     Given, say,

> data Branch r = Branch r r deriving Functor  -- add obvious applicative instance

    then @never :: Stream Branch Identity r@  is the pure infinite binary tree with
    (inaccessible) @r@s in its leaves. Given two binary trees, @tree1 \<|\> tree2@
    intersects them, preserving the leaves that came first,
    so @tree1 \<|\> never = tree1@

    @Stream Identity m r@ is an action in @m@ that is indefinitely delayed. Such an
    action can be constructed with e.g. 'untilJust'.

> untilJust :: (Monad m, Applicative f) => m (Maybe r) -> Stream f m r

    Given two such items, @\<|\>@ instance races them.
    It is thus the iterative monad transformer specially defined in
    <https://hackage.haskell.org/package/free-4.12.1/docs/Control-Monad-Trans-Iter.html Control.Monad.Trans.Iter>

    So, for example, we might write

>>> let justFour str = if length str == 4 then Just str else Nothing
>>> let four = untilJust (fmap justFour getLine)
>>> run four
one<Enter>
two<Enter>
three<Enter>
four<Enter>
"four"


    The 'Alternative' instance in
    <https://hackage.haskell.org/package/free-4.12.1/docs/Control-Monad-Trans-Free.html Control.Monad.Trans.Free>
    is avowedly wrong, though no explanation is given for this.


-}
never :: (Monad m, Applicative f) => Stream f m r
-- The Monad m constraint should really be an Applicative one,
-- but we still support old versions of base.
never =  let loop = Step $ pure (Effect (return loop)) in loop
{-# INLINABLE never #-}


delays :: (MonadIO m, Applicative f) => Double -> Stream f m r
delays seconds = loop where
  loop = Effect $ liftIO (threadDelay delay) >> return (Step (pure loop))
  delay = fromInteger (truncate (1000000 * seconds))
{-# INLINABLE delays #-}

-- {-| Permit streamed actions to proceed unless the clock has run out.
--
-- -}
-- period :: (MonadIO m, Functor f) => Double -> Stream f m r -> Stream f m (Stream f m r)
-- period seconds str = do
--     utc <- liftIO getCurrentTime
--     let loop s = do
--           utc' <- liftIO getCurrentTime
--           if diffUTCTime utc' utc > (cutoff / 1000000000)
--             then return s
--             else case s of
--               Return r -> Return (Return r)
--               Effect m -> Effect (fmap loop m)
--               Step f   -> Step (fmap loop f)
--     loop str
--   where
--   cutoff = fromInteger (truncate (1000000000 * seconds))
-- {-# INLINABLE period #-}
--
--
-- {-| Divide a succession of phases according to a specified time interval. If time runs out
--     while an action is proceeding, it is allowed to run to completion. The clock is only then
--     restarted.
-- -}
-- periods :: (MonadIO m, Functor f) => Double -> Stream f m r -> Stream (Stream f m) m r
-- periods seconds s = do
--   utc <- liftIO getCurrentTime
--   loop (addUTCTime cutoff utc) s
--
--   where
--   cutoff = fromInteger (truncate (1000000000 * seconds)) / 1000000000
--   loop final stream = do
--     utc <- liftIO getCurrentTime
--     if utc > final
--       then loop (addUTCTime cutoff utc) stream
--       else case stream of
--         Return r  -> Return r
--         Effect m  -> Effect $ fmap (loop final) m
--         Step fstr -> Step $ fmap (periods seconds) (cutoff_ final (Step fstr))
--
--         -- do
--         --   let sloop s = do
--         --         utc' <- liftIO getCurrentTime
--         --         if final < utc'
--         --           then return s
--         --           else case s of
--         --             Return r -> Return (Return r)
--         --             Effect m -> Effect (fmap sloop m)
--         --             Step f   -> Step (fmap sloop f)
--         --   Step (Step (fmap (fmap (periods seconds) . sloop) fstr))
--           -- str <- m
--           -- utc' <- liftIO getCurrentTime
--           -- if diffUTCTime utc' utc > (cutoff / 1000000000)
--           --   then return (loop utc' str)
--           --   else return (loop utc str)
--         -- Step fs   -> do
--         --   let check str = do
--         --         utc' <- liftIO getCurrentTime
--         --         loop utc' str
--         --
-- {-# INLINABLE periods #-}
--
-- cutoff_ final str = do
--     let loop s = do
--           utc' <- liftIO getCurrentTime
--           if utc' > final
--             then Return s
--             else case s of
--               Return r -> Return (Return r)
--               Effect m -> Effect (fmap loop m)
--               Step f   -> Step (fmap loop f)
--     loop str

{- | Repeat a

-}

untilJust :: (Monad m, Applicative f) => m (Maybe r) -> Stream f m r
untilJust act = loop where
  loop = Effect $ do
    m <- act
    case m of
      Nothing -> return $ Step $ pure loop
      Just a -> return $ Return a


cutoff :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Maybe r)
cutoff = loop where
  loop 0 _ = return Nothing
  loop n str = do
      e <- lift $ inspect str
      case e of
        Left r -> return (Just r)
        Right frest -> Step $ fmap (loop (n-1)) frest

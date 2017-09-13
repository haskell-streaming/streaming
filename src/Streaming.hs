{-#LANGUAGE RankNTypes, CPP, Trustworthy #-}
module Streaming
   (
   -- * An iterable streaming monad transformer
   -- $stream
   Stream,
   -- * Constructing a 'Stream' on a given functor
   yields,
   effect,
   wrap,
   replicates,
   repeats,
   repeatsM,
   unfold,
   never,
   untilJust,
   streamBuild,
   delays,

   -- * Transforming streams
   maps,
   mapsM,
   mapped,
   distribute,
   groups,

   -- * Inspecting a stream
   inspect,

   -- * Splitting and joining 'Stream's
   splitsAt,
   takes,
   chunksOf,
   concats,
   intercalates,
   cutoff,
   -- period,
   -- periods,


   -- * Zipping, unzipping, separating and unseparating streams
   zipsWith,
   zipsWith',
   zips,
   unzips,
   interleaves,
   separate,
   unseparate,
   decompose,


   -- * Eliminating a 'Stream'
   mapsM_,
   run,
   streamFold,
   iterTM,
   iterT,
   destroy,

   -- * Base functor for streams of individual items
   Of (..),
   lazily,
   strictly,

   -- * ResourceT help

   bracketStream,

   -- * re-exports
   MFunctor(..),
   MMonad(..),
   MonadTrans(..),
   MonadIO(..),
   Compose(..),
   Sum(..),
   Identity(..),
   Alternative((<|>)),
   MonadThrow(..),
   MonadResource(..),
   MonadBase(..),
   ResourceT(..),
   runResourceT,
#if MIN_VERSION_base(4,8,0)
   Bifunctor(..),
#endif

   join,
   liftM,
   liftM2,
   liftA2,
   liftA3,
   void,
   (<>)
   )
   where
import Streaming.Internal
import Streaming.Prelude
import Control.Monad.Morph
import Control.Monad
import Data.Monoid ((<>))
import Control.Applicative
import Control.Monad.Trans
import Data.Functor.Compose
import Data.Functor.Sum
import Data.Functor.Identity
import Data.Functor.Of
import Control.Monad.Base
import Control.Monad.Trans.Resource
#if MIN_VERSION_base(4,8,0)
import Data.Bifunctor
#endif

{- $stream

    The 'Stream' data type can be used to represent any effectful
    succession of steps arising in some monad.
    The form of the steps is specified by the first (\"functor\")
    parameter in @Stream f m r@. The monad of the underlying effects
    is expressed by the second parameter.

    This module exports combinators that pertain to that general case.
    Some of these are quite abstract and pervade any use of the library,
    e.g.

>   maps ::    (forall x . f x -> g x)     -> Stream f m r -> Stream g m r
>   mapped ::  (forall x . f x -> m (g x)) -> Stream f m r -> Stream g m r
>   hoist ::   (forall x . m x -> n x)     -> Stream f m r -> Stream f n r -- from the MFunctor instance
>   concats :: Stream (Stream f m) m r -> Stream f m r   

    (assuming here and thoughout that @m@ or @n@ satisfies a @Monad@ constraint, and
    @f@ or @g@ a @Functor@ constraint.)

    Others are surprisingly determinate in content:

>   chunksOf     :: Int -> Stream f m r -> Stream (Stream f m) m r
>   splitsAt     :: Int -> Stream f m r -> Stream f m (Stream f m r)
>   zipsWith     :: (forall x y. f x -> g y -> h (x, y))
                 -> Stream f m r -> Stream g m r -> Stream h m r
>   zipsWith'    :: (forall x y p. (x -> y -> p) -> f x -> g y -> h p)
                 -> Stream f m r -> Stream g m r -> Stream h m r
>   intercalates :: Stream f m () -> Stream (Stream f m) m r -> Stream f m r
>   unzips       :: Stream (Compose f g) m r ->  Stream f (Stream g m) r
>   separate     :: Stream (Sum f g) m r -> Stream f (Stream g) m r  -- cp. partitionEithers
>   unseparate   :: Stream f (Stream g) m r -> Stream (Sum f g) m r
>   groups       :: Stream (Sum f g) m r -> Stream (Sum (Stream f m) (Stream g m)) m r

    One way to see that /any/ streaming library needs some such general type is
    that it is required to represent the segmentation of a stream, and to
    express the equivalents of @Prelude/Data.List@ combinators that involve
    'lists of lists' and the like. See for example this
    <http://www.haskellforall.com/2013/09/perfect-streaming-using-pipes-bytestring.html post>
    on the correct expression of a streaming \'lines\' function.

    The module @Streaming.Prelude@ exports combinators relating to

> Stream (Of a) m r

    where @Of a r = !a :> r@ is a left-strict pair.


   This expresses the concept of a 'Producer' or 'Source' or 'Generator' and
   easily inter-operates with types with such names in e.g. 'conduit',
   'iostreams' and 'pipes'.
-}

{-| Map a stream to its church encoding; compare @Data.List.foldr@
    This is the @safe_destroy@ exported by the @Internal@ module.

    Typical @FreeT@ operators can be defined in terms of @destroy@
    e.g.

> iterT :: (Functor f, Monad m) => (f (m a) -> m a) -> Stream f m a -> m a
> iterT out stream = destroy stream out join return
> iterTM ::  (Functor f, Monad m, MonadTrans t, Monad (t m)) => (f (t m a) -> t m a) -> Stream f m a -> t m a
> iterTM out stream = destroy stream out (join . lift) return
> concats :: (Monad m, MonadTrans t, Monad (t m)) => Stream (t m) m a -> t m a
> concats stream = destroy stream join (join . lift) return
-}



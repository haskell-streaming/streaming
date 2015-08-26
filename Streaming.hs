{-#LANGUAGE RankNTypes #-}
module Streaming 
   (
   -- * Free monad transformer
   -- $stream
   Stream, 
   -- * Constructing a 'Stream' on a base functor
   unfold,
   construct,
   for,
   layer,
   layers,
   replicates,
   repeats,
   repeatsM,
   wrap,
   step,
   
   -- * Transforming streams
   maps,
   mapsM,
   distribute,
   
   -- * Inspecting a stream
   inspect,
   
   -- * Zipping streams
   zips,
   zipsWith,
   interleaves,
   
   -- * Eliminating a 'Stream'
   intercalates,
   concats,
   iterTM,
   iterT,
   destroy,
   mapsM_,
   runEffect,

   -- * Splitting and joining 'Stream's 
   splitsAt,
   chunksOf,
   concats,

   -- * Base functor for streams of individual items
   Of (..),
   lazily,
   strictly,
   
   -- * re-exports
   MFunctor(..),
   MMonad(..),
   MonadTrans(..),
   MonadIO(..),
   Compose(..),
   join,
   liftA2,
   liftA3,
   void,
   (&),
   (-->)
   )
   where
import Streaming.Internal 
import Streaming.Prelude 
import Control.Monad.Morph
import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Data.Functor.Compose 
import Data.Function ((&))
infixl 6 -->
(-->) = flip (.) 

{- $stream

    The 'Stream' data type is equivalent to @FreeT@ and can represent any effectful
    succession of steps, where the form of the steps or 'commands' is 
    specified by the first (functor) parameter. The (hidden) implementation is

> data Stream f m r = Step !(f (Stream f m r)) | Delay (m (Stream f m r)) | Return r

    In the simplest case, the base functor is @ (,) a @. Here the news 
    or /command/ at each step is an /individual element of type/ @ a @, 
    i.e. the command is a @yield@ statement.  The associated 
    @Streaming@ 'Streaming.Prelude' 
    uses the left-strict pair @Of a b@ in place of the Haskell pair @(a,b)@ 
    In it, various operations are defined for fundamental streaming types like

> Stream (Of a) m r                   -- a generator or producer (in the pipes sense) 
>                                        -- compare [a], or rather ([a],r) 
> Stream (Of a) m (Stream (Of a) m r) -- the effectful splitting of a producer
>                                        -- compare ([a],[a]) or rather ([a],([a],r))
> Stream (Stream (Of a) m) m r        -- segmentation of a producer
>                                        -- cp. [[a]], or rather ([a],([a],([a],(...,r))))

    and so on. But of course any functor can be used, and this is part of 
    the point of this prelude - as we already see from 
    the type of the segmented stream, @Stream (Stream (Of a) m) m r@

and operations like e.g. 

> chunksOf :: Monad m => Int -> Stream f m r -> Stream (Stream f m) m r
> mapsM Streaming.Prelude.length' :: Stream (Stream (Of a) m) r -> Stream (Of Int) m r

    To avoid breaking reasoning principles, the constructors 
    should not be used directly. A pattern-match should go by way of 'inspect' 
    \- or, in the producer case, 'Streaming.Prelude.next'. These mirror
    the type of @runFreeT@. The constructors are exported by the 'Internal' module.
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



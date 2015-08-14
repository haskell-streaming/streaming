{-#LANGUAGE RankNTypes #-}
module Streaming 
   (
   -- * Free monad transformer
   -- $stream
   Stream, 
   -- * Constructing a 'Stream' on a base functor
   unfold,
   for,
   construct,
   
   -- * Transforming streams
   maps,
   maps',
   mapsM,
   
   -- * Inspecting a stream
   inspect,
   
   -- * Eliminating a 'Stream'
   destroy,
   intercalates,
   concats,
   iterTM,
   iterT,

   -- * Splitting and joining 'Stream's 
   split,
   chunksOf,
   concats,

   -- * Useful functors
   Of (..),
   lazily,
   strictly,
   
   -- * re-exports
   MFunctor(..),
   MonadTrans(..)
   )
   where
import Streaming.Internal
import Streaming.Prelude 
import Control.Monad.Morph (MFunctor(..))
import Control.Monad
import Control.Monad.Trans


{- $stream

    The 'Stream' data type is equivalent to @FreeT@ and can represent any effectful
    succession of steps, where the form of the steps or 'commands' is 
    specified by the first (functor) parameter. 

> data Stream f m r = Step !(f (Stream f m r)) | Delay (m (Stream f m r)) | Return r

    In the simplest case, the base functor is @ (,) a @. Here the news 
    or /command/ at each step is an individual element of type @ a @, 
    i.e. a @yield@ statement.  In 'Streaming.Prelude', @(a,b)@ is
    replaced by the left-strict pair @Of a b@. Various operations are
    defined for types like

> Stream (Of a) m r                   -- a producer in the pipes sense 
>                                        -- i.e. an effectful, streaming [a], or rather ([a],r) 
> Stream (Of a) m (Stream (Of a) m r) -- the effectful splitting of a producer
>                                        -- i.e. an effectful ([a],[a]) or rather ([a],([a],r))
> Stream (Stream (Of a) m) m r        -- successive, segmentation of a producer
>                                        -- i.e. [[a]], or ([a],([a],([a]... r)))

    and so on. But of course any functor can be used. So, for example, 

> Stream ((->) input) m result

    is a simple @Consumer input m result@ or @Parser input m result@ type. And so on.
    See e.g. http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html ,
    http://www.haskellforall.com/2012/07/free-monad-transformers.html and similar
    literature.


    To avoid breaking reasoning principles, the constructors 
    should not be used directly. A pattern-match should go by way of 'inspect' 
    \- or, in the producer case, 'Streaming.Prelude.next'
    The constructors are exported by the 'Internal' module.
-}






{-#LANGUAGE RankNTypes #-}
module Streaming 
   (
   -- * Constructing a 'Stream' on a base functor
   construct,
   unfold,
   for,
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

   -- * Types
   Stream,
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






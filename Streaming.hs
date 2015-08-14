{-#LANGUAGE RankNTypes #-}
module Streaming 
   (
   concats,
   maps,
   split,
   chunksOf,
   maps',
   mapsM,
   intercalates,
   for,
   destroy,
   construct,
   inspect,
   unfold,
   iterTM,
   iterT,
   -- * Types
   Stream,
   Of (..),
   kurry,
   unkurry,
   -- * re-exports
   MFunctor(..),
   MonadTrans(..)
   )
   where
import Streaming.Internal
import Streaming.Prelude (for)
import Control.Monad.Morph (MFunctor(..))
import Control.Monad
import Control.Monad.Trans






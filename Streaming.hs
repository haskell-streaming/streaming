{-#LANGUAGE RankNTypes #-}
module Streaming 
   (Stream,
   concats,
   maps,
   intercalates,
   destroy,
   construct,
   iterTM,
   Of (..),
   kurry,
   unkurry
   )
   where
import Streaming.Internal
import Control.Monad.Morph
import Control.Monad


hoistStream ::
  (Monad m, Functor f) =>
  (forall a. m a -> n a) -> Stream f m b -> Stream f n b
hoistStream = hoist




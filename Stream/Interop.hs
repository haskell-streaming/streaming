{-# LANGUAGE LambdaCase, RankNTypes #-}
module Stream.Interop where
import Stream.Types hiding (buildList)
import Control.Monad
import Data.Functor.Identity
import qualified Control.Monad.Trans.Free as Free  
import Control.Monad.Trans.Free ( FreeT(..), FreeF(Free) )
import Pipes 
import Pipes.Internal 
import GHC.Exts ( build )
-- ----------------
-- Producer interop
-- ----------------

producerToStream :: Monad m => Producer a m r -> Stream (Of a) m r
producerToStream =  loop where
  loop = \case M mp         -> Delay (liftM loop mp)
               Pure r       -> Return r
               Respond a go -> Step (a :> loop (go ()))
               Request x f  -> closed x


streamToProducer :: Monad m =>  Stream (Of a) m r -> Producer a m r 
streamToProducer = loop where
  loop = \case Delay m        -> M (liftM loop m)  
               Step (a :> as) -> Respond a (\() -> loop as)
               Return r       -> Pure r

-- -----


-- ----------------
-- FreeT interop
-- ----------------

foldFree_ :: (Functor f, Monad m) => FreeT f m t -> Folding_ f m t
foldFree_ f construct wrap done = outer f where
   outer = wrap
         . liftM (\case Free.Pure r -> done r
                        Free fr     -> construct (fmap outer fr)) 
         . runFreeT


buildFree_ :: Monad m => Folding_ f m r -> FreeT f m r 
buildFree_ phi = phi (FreeT . return . Free) 
                     (FreeT . (>>= runFreeT )) 
                     (FreeT . return . Free.Pure)
                     
-- standard foldr order
freeFolding
  :: (Functor f, Monad m) =>
      (f r' -> r') -> (m r' -> r') -> (t -> r') -> FreeT f m t -> r'
freeFolding construct wrap done = 
  wrap 
  . liftM (\case Free.Pure r -> done r
                 Free free_  -> construct (fmap (freeFolding construct wrap done) free_)) 
  . runFreeT

-- ---------------------
-- haskell list interop
-- ---------------------
seriesToList :: Stream (Of t) Identity () -> [t]
seriesToList  = \case Delay (Identity ls)  -> seriesToList ls
                      Step (a :> ls) -> a : seriesToList ls
                      Return ()             -> []

foldToList, foldToList' :: Folding (Of a) Identity () -> [a]
foldToList  phi = buildList (getFolding phi)
foldToList' phi = buildList' (getFolding phi)

buildList, buildList' :: Folding_ (Of a) Identity () -> [a]
buildList phi = build (\cons nil -> phi (\(x :> xs) -> cons x xs) 
                                        runIdentity 
                                        (\() -> nil))
buildList' phi = phi (\(x :> xs) -> x : xs) 
                     runIdentity 
                     (\() -> [])



seriesFromList :: [a] -> Stream (Of a) Identity ()
seriesFromList  []    = Return ()
seriesFromList  (x:xs) = Step (x :> seriesFromList xs)

toStreamM :: Monad m => Folding_ (Of a) m r -> m ([a],r)
toStreamM phi = phi construct join (\r -> return ([], r)) where
  construct (a :> mls) = do (as,r) <- mls
                            return (a : as, r)

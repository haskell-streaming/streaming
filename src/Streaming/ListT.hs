{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}

module Streaming.ListT
  ( ListT(..)
  --, runListT
  ) where

import Streaming.Internal
import Data.Functor.Of
import qualified Streaming.Prelude as S

import Control.Applicative (Applicative (..)) --, Alternative (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Morph
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Zip
import Data.Functor (Functor (..))
--import Data.Semigroup (Semigroup ((<>)))

newtype ListT m a = Select { enumerate :: Stream (Of a) m () }

instance Monad m => Functor (ListT m) where
  fmap f (Select p) = Select (S.map f p)
  {-# INLINE fmap #-}

instance Monad m => Applicative (ListT m) where
  pure a = Select (S.yield a)
  {-# INLINE pure #-}
  mf <*> mx = Select
    ( S.for (enumerate mf) (\f ->
      S.for (enumerate mx) (\x ->
      S.yield (f x)))
    )

instance Monad m => Monad (ListT m) where
  return = pure
  {-# INLINE return #-}
  m >>= f = Select (S.for (enumerate m) (\a -> enumerate (f a)))
  {-# INLINE (>>=) #-}

instance (Monad m, Foldable m) => Foldable (ListT m) where
  foldMap f (Select p) = foldMap id (S.foldMap_ f p)

instance (Monad m, Traversable m) => Traversable (ListT m) where
  traverse k (Select p) = fmap Select (t_ p)
    where
      t_ x = case x of
        Return () -> pure (Return ())
        Effect m  -> fmap Effect (traverse t_ m)
        Step (a :> rest) -> (\a_ rest_ -> Step (a_ :> rest_)) <$> k a <*> t_ rest

instance MonadTrans ListT where
  lift m = Select (do
    a <- lift m
    S.yield a)

instance MonadIO m => MonadIO (ListT m) where
  liftIO m = lift (liftIO m)
  {-# INLINE liftIO #-}

-- what should this be?
--instance Monad m => Alternative (ListT m) where

--instance Monad m => MonadPlus (ListT m) where
--  mzero = empty
--  {-# INLINE mzero #-}
--  mplus = (<|>)
--  {-# INLINE mplus #-}

instance MFunctor ListT where
  hoist morph = Select . hoist morph . enumerate
  {-# INLINE hoist #-}

instance MMonad ListT where
  embed f (Select p0) = Select (loop p0)
    where
      loop x = case x of
        Return () -> Return ()
        Effect m  -> S.for (enumerate (fmap loop (f m))) id
        Step (a :> rest) -> Step (a :> loop rest)
  {-# INLINE embed #-}

instance (MonadState s m) => MonadState s (ListT m) where
  get = lift get
  {-# INLINE get #-}

  put s = lift (put s)
  {-# INLINE put #-}

  state f = lift (state f)
  {-# INLINE state #-}

instance (MonadWriter w m) => MonadWriter w (ListT m) where
  writer = lift . writer
  {-# INLINE writer #-}

  tell w = lift (tell w)
  {-# INLINE tell #-}

  listen l = Select (go (enumerate l) mempty)
    where
      go p w = case p of
        Return () -> Return ()
        Effect m  -> Effect (do
          (p', w') <- listen m
          pure (go p' $! mappend w w') )
        Step (a :> rest) -> Step ((a,w) :> go rest w)

  pass l = Select (go (enumerate l) mempty)
    where
      go p w = case p of
        Return () -> Return ()
        Effect m  -> Effect (do
          (p', w') <- listen m
          pure (go p' $! mappend w w'))
        Step ((b,f) :> rest) -> Effect (pass (pure
          (Step (b :> (go rest (f w))), \_ -> f w) ))

instance (MonadReader i m) => MonadReader i (ListT m) where
  ask = lift ask
  {-# INLINE ask #-}

  local f l = Select (local f (enumerate l))
  {-# INLINE local #-}

  reader f = lift (reader f)
  {-# INLINE reader #-}

instance (MonadError e m) => MonadError e (ListT m) where
  throwError e = lift (throwError e)
  {-# INLINE throwError #-}

  catchError l k = Select (catchError (enumerate l) (\e -> enumerate (k e)))
  {-# INLINE catchError #-}

{- These instances require a dependency on `exceptions`.
instance MonadThrow m => MonadThrow (ListT m) where
  throwM = Select . throwM
  {-# INLINE throwM #-}

instance MonadCatch m => MonadCatch (ListT m) where
  catch l k = Select (Control.Monad.Catch.catch (enumerate l) (\e -> enumerate (k e)))
  {-# INLINE catch #-}
-}

instance Monad m => MonadZip (ListT m) where
  mzipWith f (Select p) (Select p') = Select (S.zipWith f p p')

-- no MonadPlus instance yet
--runListT :: Monad m => ListT m a -> m ()
--runListT l = S.effects (enumerate (l >> mzero))
--{-# INLINABLE runListT #-}
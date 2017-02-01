{-#LANGUAGE CPP, DeriveDataTypeable, DeriveTraversable, DeriveFoldable #-}
module Data.Functor.Of where
import Data.Monoid
import Control.Applicative
#if MIN_VERSION_base(4,8,0)
import Data.Bifunctor
#endif
import Data.Data
import Data.Typeable

-- | A left-strict pair; the base functor for streams of individual elements.
data Of a b = !a :> b
    deriving (Data, Eq, Foldable, Ord,
              Read, Show, Traversable, Typeable)
infixr 5 :>

instance (Monoid a, Monoid b) => Monoid (Of a b) where
  mempty = mempty :> mempty
  {-#INLINE mempty #-}
  mappend (m :> w) (m' :> w') = mappend m m' :> mappend w w'
  {-#INLINE mappend #-}

instance Functor (Of a) where
  fmap f (a :> x) = a :> f x
  {-#INLINE fmap #-}
  a <$ (b :> x)   = b :> a
  {-#INLINE (<$) #-}

#if MIN_VERSION_base(4,8,0)
instance Bifunctor Of where
  bimap f g (a :> b) = f a :> g b
  {-#INLINE bimap #-}
  first f   (a :> b) = f a :> b
  {-#INLINE first #-}
  second g  (a :> b) = a :> g b
  {-#INLINE second #-}
#endif

instance Monoid a => Applicative (Of a) where
  pure x = mempty :> x
  {-#INLINE pure #-}
  m :> f <*> m' :> x = mappend m m' :> f x
  {-#INLINE (<*>) #-}
  m :> x *> m' :> y  = mappend m m' :> y
  {-#INLINE (*>) #-}
  m :> x <* m' :> y  = mappend m m' :> x
  {-#INLINE (<*) #-}

instance Monoid a => Monad (Of a) where
  return x = mempty :> x
  {-#INLINE return #-}
  m :> x >> m' :> y = mappend m m' :> y
  {-#INLINE (>>) #-}
  m :> x >>= f = let m' :> y = f x in mappend m m' :> y
  {-#INLINE (>>=) #-}

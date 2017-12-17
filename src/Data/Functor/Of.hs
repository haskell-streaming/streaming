{-# LANGUAGE CPP, DeriveDataTypeable, DeriveTraversable, DeriveFoldable,
       DeriveGeneric #-}
module Data.Functor.Of (Of(..)) where
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Control.Applicative
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import Data.Bifunctor
import Data.Data
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif
import GHC.Generics (Generic, Generic1)

-- | A left-strict pair; the base functor for streams of individual elements.
data Of a b = !a :> b
    deriving (Data, Eq, Foldable, Ord,
              Read, Show, Traversable, Typeable, Generic, Generic1)
infixr 5 :>

instance (Semigroup a, Semigroup b) => Semigroup (Of a b) where
  (m :> w) <> (m' :> w') = (m <> m') :> (w <> w')
  {-#INLINE (<>) #-}

instance (Monoid a, Monoid b) => Monoid (Of a b) where
  mempty = mempty :> mempty
  {-#INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend (m :> w) (m' :> w') = mappend m m' :> mappend w w'
  {-#INLINE mappend #-}
#endif

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

#if MIN_VERSION_base(4,9,0)
instance Show a => Show1 (Of a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Eq a => Eq1 (Of a) where
  liftEq = liftEq2 (==)

instance Ord a => Ord1 (Of a) where
  liftCompare = liftCompare2 compare

instance Show2 Of where
  liftShowsPrec2 spa _sla spb _slb p (a :> b) =
    showParen (p > 5) $
    spa 6 a .
    showString " :> " .
    spb 6 b

instance Eq2 Of where
  liftEq2 f g (x :> y) (z :> w) = f x z && g y w

instance Ord2 Of where
  liftCompare2 comp1 comp2 (x :> y) (z :> w) =
    comp1 x z `mappend` comp2 y w
#endif


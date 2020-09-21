{-# language FunctionalDependencies, ScopedTypeVariables, FlexibleInstances,
  BangPatterns, UndecidableInstances #-}

-- | An implementation of Okasaki's implicit queues holding elements of some
-- semigroup. We track the sum of them all. This structure is designed to
-- support efficient *sliding window* algorithms for streams.
--
-- References:
--
-- Hinze, Ralf & Paterson, Ross. (2006). Finger trees: A simple general-purpose
-- data structure. J. Funct. Program.. 16. 197-217. 10.1017/S0956796805005769.
--
-- Okasaki, C. (1998). Purely Functional Data Structures. Cambridge: Cambridge
-- University Press. doi:10.1017/CBO9780511530104

module Data.AnnotatedQueue
  ( Queue
  , ViewL (..)
  , empty
  , viewl
  , drop1
  , singleton
  , snoc
  , measure
  ) where

import Data.Semigroup (Semigroup (..))

data FDigit a = FOne !a | FTwo !a !a
data RDigit a = RZero | ROne !a
data Node s a = Node !s !a !a

newtype Queue s = Queue (Tree s (Elem s))
instance Semigroup s => Semigroup (Queue s) where
  (!t) <> u = case viewl u of
    EmptyL -> t
    ViewL x xs -> (t `snoc` x) <> xs
instance Semigroup s => Monoid (Queue s) where
  mempty = empty
  mappend = (<>)

newtype Elem a = Elem a

-- Debit invariant (Okasaki): the middle tree of
-- a Deep node is allowed |pr| - |sf| debits, where
-- pr is the prefix and sf is the suffix.
data Tree s a
  = Zero
  | One !a
  | Two !a !a
  | Deep !s !(FDigit a) (Tree s (Node s a)) !(RDigit a)

empty :: Queue s
empty = Queue Zero

singleton :: s -> Queue s
singleton = Queue . One . Elem

snoc :: Semigroup s => Queue s -> s -> Queue s
snoc (Queue t) s = Queue (snocTree t (Elem s))
{-# INLINABLE snoc #-}

measure :: Semigroup s => Queue s -> Maybe s
measure (Queue q) = case q of
  Zero -> Nothing
  One a -> Just (measure_ a)
  Two a b -> Just (measure_ a <> measure_ b)
  Deep s _ _ _ -> Just s
{-# INLINABLE measure #-}

class Measurable s a | a -> s where
  measure_ :: a -> s
instance Measurable s (Elem s) where
  measure_ (Elem x) = x
instance Measurable s (Node s a) where
  measure_ (Node s _ _) = s
instance (Semigroup s, Measurable s a) => Measurable s (FDigit a) where
  measure_ (FOne a) = measure_ a
  measure_ (FTwo a b) = measure_ a <> measure_ b

class SemiMeasurable s a | a -> s where
  semimeasure :: s -> a -> s
instance (Semigroup s, Measurable s a) => SemiMeasurable s (RDigit a) where
  semimeasure s RZero = s
  semimeasure s (ROne a) = s <> measure_ a
instance (Semigroup s, Measurable s a)
  => SemiMeasurable s (Tree s a) where
  semimeasure s Zero = s
  semimeasure s (One a) = s <> measure_ a
  semimeasure s (Two a b) = s <> measure_ a <> measure_ b
  semimeasure s (Deep t _ _ _) = s <> t

node
  :: (Semigroup s, Measurable s a)
  => a -> a -> Node s a
node a b = Node (measure_ a <> measure_ b) a b
{-# INLINABLE node #-}

deep :: (Semigroup s, Measurable s a) => FDigit a -> Tree s (Node s a) -> RDigit a -> Tree s a
deep pr m sf = Deep (measure_ pr `semimeasure` m `semimeasure` sf) pr m sf
{-# INLINABLE deep #-}

snocTree :: (Measurable s a, Semigroup s) => Tree s a -> a -> Tree s a
-- Note: in the last case we depart slightly from Okasaki. Following Hinze
-- and Paterson, we force the *old* middle immediately to prevent a chain of
-- thunks from accumulating in case of multiple sequential snocs.
snocTree Zero a = One a
snocTree (One a) b = Two a b
snocTree (Two a b) c = Deep (measure_ a <> measure_ b <> measure_ c) (FTwo a b) Zero (ROne c)
snocTree (Deep s pr m RZero) q = Deep (s <> measure_ q) pr m (ROne q)
snocTree (Deep s pr !m (ROne p)) !q
  = Deep (s <> measure_ q) pr (snocTree m (node p q)) RZero
{-# INLINABLE snocTree #-}

{-
Theorem: snocTree runs in O(1) amortized time.

Proof:

We show that snocTree costs at most 2 units of work.

Reminder: The debit invariant allows the middle tree of a Deep
node |pr| - |sf| debits.

The first three cases are trivial as they don't have any
debits in their inputs or outputs.

In the fourth case (Deep s pr m RZero), the debit allowance on `m` drops by 1.
We do 1 unit of unshared work and pay off one debit on `m`, for a total of 2
units of work.

In the last case (Deep s pr m (ROne p)), we have two possibilities, depending
on the prefix:

1. The prefix has one element. Then the debit allowance on `m` is 0. We force
`m` (for free). We do 1 unit of unshared work. We create a suspension for the
recursive call and place 2 debits on it to pay for that. Since the debit
allowance for the result middle only allows 1 debit, we pay one of them off
now.  So the amortized cost is 2.

2. The prefix has two elements. Then the debit allowance on `m` is 1. We pay
off that debit and force `m`. We do 1 unit of unshared work. We create a
suspension for the recursive call and place 2 debits on it. This is within the
debit allowance for the result middle. So the amortized cost is 2.
-}

data ViewL s = EmptyL | ViewL !s (Queue s)

-- Note: we need the ViewLTree constructor to be lazy in the
-- tail to maintain the right amortized bounds. We include
-- the measure of a nonempty tree in its view because we
-- need that in the recursive case of viewlTree.
data ViewLTree s a = EmptyLTree | ViewLTree !s !a (Tree s a)

viewl :: Semigroup s => Queue s -> ViewL s
-- We could write a separate version for this top layer to avoid unnecessarily
-- calculating a sum in the Two case.
viewl (Queue q) = case viewlTree q of
  EmptyLTree -> EmptyL
  ViewLTree _ (Elem s) q' -> ViewL s (Queue q')
{-# INLINABLE viewl #-}

viewlTree :: (Semigroup s, Measurable s a) => Tree s a -> ViewLTree s a
-- Important note: we produce the head before forcing the tail. This
-- is key to maintaining O(1) amortized time here.
viewlTree Zero = EmptyLTree
viewlTree (One a) = ViewLTree (measure_ a) a Zero
viewlTree (Two a b) = ViewLTree (measure_ a <> measure_ b) a (One b)
viewlTree (Deep s (FTwo a b) m sf) = ViewLTree s a (deep (FOne b) m sf)
viewlTree (Deep s (FOne a) m sf) = ViewLTree s a $ case viewlTree m of
  EmptyLTree -> case sf of
    RZero -> Zero
    ROne b -> One b
  ViewLTree sm (Node p b c) m' -> Deep (sm `semimeasure` sf) (FTwo b c) m' sf
{-# INLINABLE viewlTree #-}

{-
Theorem: drop1 runs in O(1) amortized time.

Proof. We follow the general outline of Okasaki Theorem 11.1, adjusting for the
need to measure (and therefore force) certain suspended middle trees in the
fourth case.

The short version: everything is the same as in Okasaki, but if the recursive
viewing reaches an FOne digit, we need to discharge up to two debits on the
tree middle there, adding just a constant amount to the amortized cost of
the operation.

The long version, in lots of detail:

This particular proof doesn't make use of the "debit passing" concept, because
we seem to be able to get away without it. We will analyze `drop1` as taking 3
units of work.  When reading this proof, it may be helpful to mentally imagine
breaking down `viewlTree` into `headTree` and `drop1Tree`, much like Okasaki
does.

The first three cases are trivial, with no debits on inputs or outputs, so we
can assign them each a cost of 1.

In the fourth case (an FTwo digit), we may have up to 2 debits on `m` we must
discharge so we can measure it in `deep`, plus 1 unit of unshared work, for
a total of 3.

In the fifth case (an FOne digit), we have two possibilities:

The suffix is RZero: We may have up to 1 debit on `m`, which we discharge to
view it. We do 1 unit of unshared work. If `m` is nonempty, we create a
suspension to take its tail `m'`, and by the inductive hypothesis create 3
debits to cover that. We place two of them on `m'` and discharge the third. So
the amortized cost is 3.

The suffix is ROne: There are no debits on `m`, so we can view it immediately.
We do one unit of unshared work. If `m` is nonempty, we create a suspension to
take its tail `m'`, and create 3 debits to cover that. We place one debit on
`m'` and discharge the other two. The amortized cost is 3.
-}

drop1 :: Semigroup s => Queue s -> Queue s
drop1 q = case viewl q of
  EmptyL -> empty
  ViewL _ q' -> q'
{-
-- We could expand out the upper layer to avoid an unnecessary view allocation.
-- Is that worth the extra code size?
-}
{-# INLINABLE drop1 #-}

{-| This names exported by this module are closely modeled on those in @Prelude@ and @Data.List@,
    but also on
    <http://hackage.haskell.org/package/pipes-4.1.9/docs/Pipes-Prelude.html Pipes.Prelude>,
    <http://hackage.haskell.org/package/pipes-group-1.0.3/docs/Pipes-Group.html Pipes.Group>
    and <http://hackage.haskell.org/package/pipes-parse-3.0.6/docs/Pipes-Parse.html Pipes.Parse>.
    The module may be said to give independent expression to the conception of
    Producer \/ Source \/ Generator manipulation
    articulated in the latter two modules. Because we dispense with piping and
    conduiting, the distinction between all of these modules collapses. Some things are
    lost but much is gained: on the one hand, everything comes much closer to ordinary
    beginning Haskell programming and, on the other, acquires the plasticity of programming 
    directly with a general free monad type. The leading type, @Stream (Of a) m r@ is chosen to permit an api
    that is as close as possible to that of @Data.List@ and the @Prelude@.

    Import qualified thus:

> import Streaming
> import qualified Streaming.Prelude as S

    For the examples below, one sometimes needs

> import Streaming.Prelude (each, yield, next, mapped, stdoutLn, stdinLn)
> import Data.Function ((&))

   Other libraries that come up in passing are

> import qualified Control.Foldl as L -- cabal install foldl
> import qualified Pipes as P
> import qualified Pipes.Prelude as P
> import qualified System.IO as IO

     Here are some correspondences between the types employed here and elsewhere:

>               streaming             |            pipes               |       conduit       |  io-streams
> -------------------------------------------------------------------------------------------------------------------
> Stream (Of a) m ()                  | Producer a m ()                | Source m a          | InputStream a
>                                     | ListT m a                      | ConduitM () o m ()  | Generator r ()
> -------------------------------------------------------------------------------------------------------------------
> Stream (Of a) m r                   | Producer a m r                 | ConduitM () o m r   | Generator a r
> -------------------------------------------------------------------------------------------------------------------
> Stream (Of a) m (Stream (Of a) m r) | Producer a m (Producer a m r)  |
> --------------------------------------------------------------------------------------------------------------------
> Stream (Stream (Of a) m) r          | FreeT (Producer a m) m r       |
> --------------------------------------------------------------------------------------------------------------------
> --------------------------------------------------------------------------------------------------------------------
> ByteString m ()                     | Producer ByteString m ()       | Source m ByteString  | InputStream ByteString
> --------------------------------------------------------------------------------------------------------------------
>
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module Streaming.Prelude (
    -- * Types
    Of (..)

    -- * Introducing streams of elements
    -- $producers
    , yield
    , each
    , stdinLn
    , readLn
    , fromHandle
    , readFile 
    , iterate
    , iterateM
    , repeat
    , repeatM
    , replicate
    , untilRight
    , cycle
    , replicateM
    , enumFrom
    , enumFromThen
    , unfoldr
    


    -- * Consuming streams of elements
    -- $consumers
    , stdoutLn
    , stdoutLn'
    , mapM_
    , print
    , toHandle
    , writeFile 
    , effects
    , erase
    , drained


    -- * Stream transformers
    -- $pipes
    , map
    , mapM
    , maps
    , mapsPost
    , mapped
    , mappedPost
    , for
    , with
    , subst
    , copy
    , duplicate
    , store
    , chain
    , sequence
    , nubOrd
    , nubOrdOn
    , nubInt
    , nubIntOn
    , filter
    , filterM
    , mapMaybeM
    , delay
    , intersperse
    , take
    , takeWhile
--    , takeWhile'
    , takeWhileM
    , drop
    , dropWhile
    , concat
    -- , elemIndices
    -- , findIndices
    , scan
    , scanM
    , scanned
    , read
    , show
    , cons
    , slidingWindow    


    -- * Splitting and inspecting streams of elements
    , next
    , uncons
    , splitAt
    , split
    , breaks
    , break
    , breakWhen
    , span
    , group
    , groupBy
 --   , groupedBy


    -- * Sum and Compose manipulation

    , distinguish
    , switch
    , separate
    , unseparate
    , eitherToSum
    , sumToEither
    , sumToCompose
    , composeToSum

    -- * Folds
    -- $folds
    , fold
    , fold_
    , foldM
    , foldM_
    , all
    , all_
    , any
    , any_
    , sum
    , sum_
    , product
    , product_
    , head
    , head_
    , last
    , last_
    , elem
    , elem_
    , notElem
    , notElem_
    , length
    , length_
    , toList
    , toList_
    , mconcat
    , mconcat_
    , minimum
    , minimum_
    , maximum
    , maximum_
    , foldrM
    , foldrT


    -- , all
    -- , any
    -- , and
    -- , or
    -- , elem

    -- , find
    -- , findIndex
    -- , head
    -- , index
    -- , last
    -- , length
    -- , maximum
    -- , minimum
    -- , null

    -- * Zips and unzips
    , zip
    , zipWith
    , zip3
    , zipWith3
    , unzip
    , partitionEithers
    , partition

    -- * Merging streams
    -- $merging

    , merge
    , mergeOn
    , mergeBy
    
    -- * Maybes
    -- $maybes
    , catMaybes
    , mapMaybe

    -- * Pair manipulation
    , lazily
    , strictly
    , fst'
    , snd'
    , mapOf
    , _first
    , _second

    -- * Interoperation
    , reread

    -- * Basic Type
    , Stream
  ) where
import Streaming.Internal

import Control.Monad hiding (filterM, mapM, mapM_, foldM, foldM_, replicateM, sequence)
import Data.Functor.Identity
import Data.Functor.Sum
import Control.Monad.Trans
import Control.Applicative (Applicative (..))
import Data.Functor (Functor (..), (<$))

import qualified Prelude as Prelude
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import Text.Read (readMaybe)
import Prelude hiding (map, mapM, mapM_, filter, drop, dropWhile, take, mconcat
                      , sum, product, iterate, repeat, cycle, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo, enumFromThen, length
                      , print, zipWith, zip, zipWith3, zip3, unzip, seq, show, read
                      , readLn, sequence, concat, span, break, readFile, writeFile
                      , minimum, maximum, elem, notElem, all, any, head
                      , last)

import qualified GHC.IO.Exception as G
import qualified System.IO as IO
import Foreign.C.Error (Errno(Errno), ePIPE)
import Control.Exception (throwIO, try)
import Data.Monoid (Monoid (mappend, mempty))
import Control.Concurrent (threadDelay)
import Data.Functor.Compose
import Data.Functor.Of
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Ord (Ordering (..), comparing)

-- instance (Eq a) => Eq1 (Of a) where eq1 = (==)
-- instance (Ord a) => Ord1 (Of a) where compare1 = compare
-- instance (Read a) => Read1 (Of a) where readsPrec1 = readsPrec
-- instance (Show a) => Show1 (Of a) where showsPrec1 = showsPrec

{-| Note that 'lazily', 'strictly', 'fst'', and 'mapOf' are all so-called /natural transformations/ on the primitive @Of a@ functor
    If we write

>  type f ~~> g = forall x . f x -> g x

   then we can restate some types as follows:

>  mapOf            :: (a -> b) -> Of a ~~> Of b   -- Bifunctor first
>  lazily           ::             Of a ~~> (,) a
>  Identity . fst'  ::             Of a ~~> Identity a

   Manipulation of a @Stream f m r@ by mapping often turns on recognizing natural transformations of @f@.
   Thus @maps@ is far more general the the @map@ of the @Streaming.Prelude@, which can be
   defined thus:

>  S.map :: (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
>  S.map f = maps (mapOf f)

  i.e.

>  S.map f = maps (\(a :> x) -> (f a :> x))

  This rests on recognizing that @mapOf@ is a natural transformation; note though
  that it results in such a transformation as well:

>  S.map :: (a -> b) -> Stream (Of a) m ~> Stream (Of b) m

  Thus we can @maps@ it in turn.


 -}
lazily :: Of a b -> (a,b)
lazily = \(a:>b) -> (a,b)
{-# INLINE lazily #-}

{-| Convert a standard Haskell pair into a left-strict pair  -}
strictly :: (a,b) -> Of a b
strictly = \(a,b) -> a :> b
{-# INLINE strictly #-}

{-| @fst'@ and @snd'@ extract the first and second element of a pair

>>> S.fst' (1:>"hi")
1
>>> S.snd' (1:>"hi")
"hi"


     They are contained in the @_first@ and @_second@ lenses,
     if any lens library is in scope

>>> import Lens.Micro
>>> (1:>"hi") ^. S._first
1
>>> (1:>"hi") ^. S._second
"hi"

 -}

fst' :: Of a b -> a
fst' (a :> _) = a
{-#INLINE fst' #-}
snd' :: Of a b -> b
snd' (_ :> b) = b
{-#INLINE snd' #-}

{-| Map a function over the first element of an @Of@ pair

>>> S.mapOf even (1:>"hi")
False :> "hi"

     @mapOf@ is just @first@ from the @Bifunctor@ instance

>>> first even (1:>"hi")
False :> "hi"

     and is contained in the @_first@ lens

>>> import Lens.Micro
>>> over S._first even (1:>"hi")
False :> "hi"

 -}

mapOf :: (a -> b) -> Of a r -> Of b r
mapOf f (a:> b) = (f a :> b)
{-#INLINE mapOf #-}

{-| A lens into the first element of a left-strict pair -}
_first :: Functor f => (a -> f a') -> Of a b -> f (Of a' b)
_first afb (a:>b) = fmap (\c -> (c:>b)) (afb a)
{-# INLINE _first #-}

{-| A lens into the second element of a left-strict pair -}
_second :: Functor f => (b -> f b') -> Of a b -> f (Of a b')
_second afb (a:>b) = fmap (\c -> (a:>c)) (afb b)
{-#INLINABLE _second #-}

all :: Monad m => (a -> Bool) -> Stream (Of a) m r -> m (Of Bool r)
all thus = loop True where
  loop b str = case str of
    Return r -> return (b :> r)
    Effect m -> m >>= loop b
    Step (a :> rest) -> if thus a
      then loop True rest
      else do
        r <- effects rest
        return (False :> r)
{-#INLINABLE all #-}

all_ :: Monad m => (a -> Bool) -> Stream (Of a) m r -> m Bool
all_ thus = loop True where
  loop b str = case str of
    Return _ -> return b
    Effect m -> m >>= loop b
    Step (a :> rest) -> if thus a
      then loop True rest
      else return False
{-#INLINABLE all_ #-}


any :: Monad m => (a -> Bool) -> Stream (Of a) m r -> m (Of Bool r)
any thus = loop False where
  loop b str = case str of
    Return r -> return (b :> r)
    Effect m -> m >>= loop b
    Step (a :> rest) -> if thus a
      then do
        r <- effects rest
        return (True :> r)
      else loop False rest
{-#INLINABLE any #-}

any_ :: Monad m => (a -> Bool) -> Stream (Of a) m r -> m Bool
any_ thus = loop False where
  loop b str = case str of
    Return _ -> return b
    Effect m -> m >>= loop b
    Step (a :> rest) -> if thus a
      then return True
      else loop False rest
{-#INLINABLE any_ #-}

{-| Break a sequence upon meeting element falls under a predicate,
    keeping it and the rest of the stream as the return value.

>>> rest <- S.print $ S.break even $ each [1,1,2,3]
1
1
>>> S.print rest
2
3

-}

break :: Monad m => (a -> Bool) -> Stream (Of a) m r
      -> Stream (Of a) m (Stream (Of a) m r)
break thePred = loop where
  loop str = case str of
    Return r         -> Return (Return r)
    Effect m          -> Effect $ fmap loop m
    Step (a :> rest) -> if (thePred a)
      then Return (Step (a :> rest))
      else Step (a :> loop rest)
{-# INLINABLE break #-}

{-| Yield elements, using a fold to maintain state, until the accumulated
   value satifies the supplied predicate. The fold will then be short-circuited
   and the element that breaks it will be put after the break.
   This function is easiest to use with 'Control.Foldl.purely'

>>>  rest <- each [1..10] & L.purely S.breakWhen L.sum (>10) & S.print
1
2
3
4
>>> S.print rest
5
6
7
8
9
10

-}
breakWhen :: Monad m => (x -> a -> x) -> x -> (x -> b) -> (b -> Bool) -> Stream (Of a) m r -> Stream (Of a) m (Stream (Of a) m r)
breakWhen step begin done thePred = loop0 begin
  where
    loop0 x stream = case stream of
        Return r -> return (return r)
        Effect mn  -> Effect $ fmap (loop0 x) mn
        Step (a :> rest) -> loop a (step x a) rest
    loop a !x stream = do
      if thePred (done x)
        then return (yield a >> stream)
        else case stream of
          Return r -> yield a >> return (return r)
          Effect mn  -> Effect $ fmap (loop a x) mn
          Step (a' :> rest) -> do
            yield a
            loop a' (step x a') rest
{-# INLINABLE breakWhen #-}

-- -- Break during periods where the predicate is not satisfied, grouping the periods when it is.
--
-- >>> S.print $ mapped S.toList $ S.breaks not $ S.each [False,True,True,False,True,True,False]
-- [True,True]
-- [True,True]
-- >>> S.print $ mapped S.toList $ S.breaks id $ S.each [False,True,True,False,True,True,False]
-- [False]
-- [False]
-- [False]
--
-- -}
breaks
  :: Monad m =>
     (a -> Bool) -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r
breaks thus  = loop  where
  loop stream = Effect $ do
    e <- next stream
    return $ case e of
      Left   r      -> Return r
      Right (a, p') ->
       if not (thus a)
          then Step $ fmap loop (yield a >> break thus p')
          else loop p'
{-#INLINABLE breaks #-}

{-| Apply an action to all values, re-yielding each

>>> S.product $ S.chain Prelude.print $ S.each [1..5]
1
2
3
4
5
120 :> ()
-}

chain :: Monad m => (a -> m ()) -> Stream (Of a) m r -> Stream (Of a) m r
chain f = loop where
  loop str = case str of
    Return r -> return r
    Effect mn  -> Effect (fmap loop mn)
    Step (a :> rest) -> Effect $ do
      f a
      return (Step (a :> loop rest))
{-# INLINABLE chain #-}

{-| Make a stream of foldable containers into a stream of their separate elements.
    This is just

> concat str = for str each

>>> S.print $ S.concat (each ["xy","z"])
'x'
'y'
'z'

    Note that it also has the effect of 'Data.Maybe.catMaybes', 'Data.Either.rights'
    'map snd' and such-like operations.

>>> S.print $ S.concat $ S.each [Just 1, Nothing, Just 2]
1
2
>>> S.print $  S.concat $ S.each [Right 1, Left "Error!", Right 2]
1
2
>>> S.print $ S.concat $ S.each [('A',1), ('B',2)]
1
2

-}

concat :: (Monad m, Foldable.Foldable f) => Stream (Of (f a)) m r -> Stream (Of a) m r
concat = loop
  where
    loop str = case str of
        Return r -> Return r
        Effect m -> Effect (fmap loop m)
        Step (lst :> as) ->
          let inner [] = loop as
              inner (x:rest) = Step (x :> inner rest)
          in inner (Foldable.toList lst)
{-# INLINABLE concat #-}
-- The above hand-written loop is ~20% faster than the 'for' implementation
-- concat str = for str each

{-| The natural @cons@ for a @Stream (Of a)@.

> cons a stream = yield a >> stream

   Useful for interoperation:

> Data.Text.foldr S.cons (return ()) :: Text -> Stream (Of Char) m ()
> Lazy.foldrChunks S.cons (return ()) :: Lazy.ByteString -> Stream (Of Strict.ByteString) m ()

    and so on.
-}

cons :: (Monad m) => a -> Stream (Of a) m r -> Stream (Of a) m r
cons a str = Step (a :> str)
{-# INLINE cons #-}

{- | Cycle repeatedly through the layers of a stream, /ad inf./ This
     function is functor-general

> cycle = forever

>>> rest <- S.print $ S.splitAt 3 $ S.cycle (yield True >> yield False)
True
False
True
>>> S.print $ S.take 3 rest
False
True
False

-}

cycle :: (Monad m, Functor f) => Stream f m r -> Stream f m s
cycle str = loop where loop = str >> loop
{-#INLINABLE cycle #-}


{-| Interpolate a delay of n seconds between yields.
-}
delay :: MonadIO m => Double -> Stream (Of a) m r -> Stream (Of a) m r
delay seconds = loop where
  pico = truncate (seconds * 1000000)
  loop str = do
    e <- lift $ next str
    case e of
      Left r -> Return r
      Right (a,rest) -> do
        yield a
        liftIO $ threadDelay pico
        loop rest
{-#INLINABLE delay #-}



{-| Where a transformer returns a stream, run the effects of the stream, keeping
   the return value. This is usually used at the type

> drained :: Monad m => Stream (Of a) m (Stream (Of b) m r) -> Stream (Of a) m r
> drained = join . fmap (lift . effects)

   Here, for example, we split a stream in two places and throw out the middle segment:

>>> rest <- S.print $ S.drained $ S.splitAt 2 $ S.splitAt 5 $ each [1..7]
1
2
>>> S.print rest
6
7

   In particular, we can define versions  of @take@ and @takeWhile@ which
   retrieve the return value of the rest of the stream - and which can
   thus be used with 'maps':

> take' n = S.drained . S.splitAt n
> takeWhile' thus = S.drained . S.span thus

-}
drained :: (Monad m, Monad (t m), MonadTrans t) => t m (Stream (Of a) m r) -> t m r
drained tms = tms >>= lift . effects
{-#INLINE drained #-}

-- ---------------
-- drop
-- ---------------
{-|  Ignore the first n elements of a stream, but carry out the actions

>>> S.toList $ S.drop 2 $  S.replicateM 5 getLine
a<Enter>
b<Enter>
c<Enter>
d<Enter>
e<Enter>
["c","d","e"] :> ()

     Because it retains the final return value, @drop n@  is a suitable argument
     for @maps@:

>>> S.toList $ concats $ maps (S.drop 4) $ chunksOf 5 $ each [1..20]
[5,10,15,20] :> ()



  -}

drop :: (Monad m) => Int -> Stream (Of a) m r -> Stream (Of a) m r
drop n str | n <= 0 = str
drop n str = loop n str where
  loop 0 stream = stream
  loop m stream = case stream of
      Return r       -> Return r
      Effect ma      -> Effect (fmap (loop m) ma)
      Step (_ :> as) -> loop (m-1) as
{-# INLINABLE drop #-}

-- ---------------
-- dropWhile
-- ---------------

{- | Ignore elements of a stream until a test succeeds, retaining the rest.

>>> S.print $ S.dropWhile ((< 5) . length) S.stdinLn
one<Enter>
two<Enter>
three<Enter>
"three"
four<Enter>
"four"
^CInterrupted.


-}
dropWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
dropWhile thePred = loop where
  loop stream = case stream of
    Return r       -> Return r
    Effect ma       -> Effect (fmap loop ma)
    Step (a :> as) -> if thePred a
      then loop as
      else Step (a :> as)
{-# INLINABLE dropWhile #-}

-- ---------------
-- each
-- ---------------

{- | Stream the elements of a pure, foldable container.

>>> S.print $ each [1..3]
1
2
3


-}
each :: (Monad m, Foldable.Foldable f) => f a -> Stream (Of a) m ()
each = Foldable.foldr (\a p -> Step (a :> p)) (Return ())
{-# INLINABLE each #-}


-- ---------------
-- effects
-- ---------------

{- | Reduce a stream, performing its actions but ignoring its elements.

>>> rest <- S.effects $ S.splitAt 2 $ each [1..5]
>>> S.print rest
3
4
5

    'effects' should be understood together with 'copy' and is subject to the rules

> S.effects . S.copy       = id
> hoist S.effects . S.copy = id

    The similar @effects@ and @copy@ operations in @Data.ByteString.Streaming@ obey the same rules.

-}
effects :: Monad m => Stream (Of a) m r -> m r
effects = loop where
  loop stream = case stream of
    Return r         -> return r
    Effect m         -> m >>= loop
    Step (_ :> rest) -> loop rest
{-#INLINABLE effects #-}

{-| Exhaust a stream remembering only whether @a@ was an element.

-}

elem :: (Monad m, Eq a) => a -> Stream (Of a) m r -> m (Of Bool r)
elem a' = loop False where
  loop True str = fmap (True :>) (effects str)
  loop False str = case str of
    Return r -> return (False :> r)
    Effect m -> m >>= loop False
    Step (a:> rest) ->
      if a == a'
        then fmap (True :>) (effects rest)
        else loop False rest
{-#INLINABLE elem #-}

elem_ :: (Monad m, Eq a) => a -> Stream (Of a) m r -> m Bool
elem_ a' = loop False where
  loop True _ = return True
  loop False str = case str of
    Return _ -> return False
    Effect m -> m >>= loop False
    Step (a:> rest) ->
      if a == a'
        then return True
        else loop False rest
{-#INLINABLE elem_ #-}

-- -----
-- enumFrom
-- ------

{-| An infinite stream of enumerable values, starting from a given value.
    It is the same as @S.iterate succ@.
   Because their return type is polymorphic, @enumFrom@ and @enumFromThen@
   (and @iterate@ are useful for example with @zip@
   and @zipWith@, which require the same return type in the zipped streams.
   With @each [1..]@ the following bit of connect-and-resume would be impossible:

>>> rest <- S.print $ S.zip (S.enumFrom 'a') $ S.splitAt 3 $ S.enumFrom 1
('a',1)
('b',2)
('c',3)
>>>  S.print $ S.take 3 rest
4
5
6

-}
enumFrom :: (Monad m, Enum n) => n -> Stream (Of n) m r
enumFrom = loop where
  loop !n = Effect (return (Step (n :> loop (succ n))))
{-# INLINABLE enumFrom #-}


{-| An infinite sequence of enumerable values at a fixed distance, determined
   by the first and second values. See the discussion of 'Streaming.enumFrom'

>>> S.print $ S.take 3 $ S.enumFromThen 100 200
100
200
300

-}
enumFromThen:: (Monad m, Enum a) => a -> a -> Stream (Of a) m r
enumFromThen first second = Streaming.Prelude.map toEnum (loop _first)
  where
    _first = fromEnum first
    _second = fromEnum second
    diff = _second - _first
    loop !s =  Step (s :> loop (s+diff))
{-# INLINABLE enumFromThen #-}

-- ---------------
-- erase
-- ---------------
{- | Remove the elements from a stream of values, retaining the structure of layers.
-}
erase :: Monad m => Stream (Of a) m r -> Stream Identity m r
erase = loop where
  loop str = case str of
    Return r -> Return r
    Effect m -> Effect (fmap loop m)
    Step (_:>rest) -> Step (Identity (loop rest))
{-# INLINABLE erase #-}

-- ---------------
-- filter
-- ---------------

-- | Skip elements of a stream that fail a predicate
filter  :: (Monad m) => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filter thePred = loop where
  loop str = case str of
    Return r       -> Return r
    Effect m       -> Effect (fmap loop m)
    Step (a :> as) -> if thePred a
      then Step (a :> loop as)
      else loop as
{-# INLINE filter #-}  -- ~ 10% faster than INLINABLE in simple bench

                         
-- ---------------
-- filterM
-- ---------------

-- | Skip elements of a stream that fail a monadic test
filterM  :: (Monad m) => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filterM thePred = loop where
  loop str = case str of
    Return r       -> Return r
    Effect m       -> Effect $ fmap loop m
    Step (a :> as) -> Effect $ do
      bool <- thePred a
      if bool
        then return $ Step (a :> loop as)
        else return $ loop as
{-# INLINE filterM #-}  -- ~ 10% faster than INLINABLE in simple bench

-- -- ---------------
-- -- first
-- -- ---------------
-- {- | Take either the first item in a stream or the return value, if it is empty.
--      The typical mark of an infinite stream is a polymorphic return value; in
--      that case, 'first' is a sort of @safeHead@
--
--      To iterate an action returning a 'Maybe', until it succeeds.
--
-- -}
-- first :: Monad m => Stream (Of r) m r -> m r
-- first = loop where
--   loop str = case str of
--     Return r -> return r
--     Effect m -> m >>= loop
--     Step (r :> rest) -> return r
-- {-# INLINABLE first #-}

-- ---------------
-- fold
-- ---------------

{- $folds
    Use these to fold the elements of a 'Stream'.

>>> S.fold_ (+) 0 id $ S.each [1..0]
50

    The general folds 'fold', fold_', 'foldM' and 'foldM_' are arranged
    for use with @Control.Foldl@ 'Control.Foldl.purely' and 'Control.Foldl.impurely'

>>> L.purely fold_ L.sum $ each [1..10]
55
>>> L.purely fold_ (liftA3 (,,) L.sum L.product L.list) $ each [1..10]
(55,3628800,[1,2,3,4,5,6,7,8,9,10])

    All functions marked with an underscore omit
    (e.g. @fold_@, @sum_@) the stream's return value in a left-strict pair.
    They are good for exiting streaming completely,
    but when you are, e.g. @mapped@-ing over a @Stream (Stream (Of a) m) m r@,
    which is to be compared with @[[a]]@. Specializing, we have e.g.

>  mapped sum :: (Monad m, Num n) => Stream (Stream (Of Int)) IO () -> Stream (Of n) IO ()
>  mapped (fold mappend mempty id) :: Stream (Stream (Of Int)) IO () -> Stream (Of Int) IO ()

>>> S.print $ mapped S.sum $ chunksOf 3 $ S.each [1..10]
6
15
24
10

>>> let three_folds = L.purely S.fold (liftA3 (,,) L.sum L.product L.list)
>>> S.print $ mapped three_folds $ chunksOf 3 (each [1..10])
(6,6,[1,2,3])
(15,120,[4,5,6])
(24,504,[7,8,9])
(10,10,[10])
-}

{-| Strict fold of a 'Stream' of elements, preserving only the result of the fold, not
    the return value of the stream.  The third parameter will often be 'id' where a fold
    is written by hand:

>>> S.fold_ (+) 0 id $ each [1..10]
55

    It can be used to replace a standard Haskell type with one more suited to
    writing a strict accumulation function. It is also crucial to the
    Applicative instance for @Control.Foldl.Fold@

> Control.Foldl.purely fold :: Monad m => Fold a b -> Stream (Of a) m () -> m b
-}
fold_ :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r -> m b
fold_ step begin done = fmap (\(a :> _) -> a) . fold step begin done
{-#INLINE fold_ #-}

{-| Strict fold of a 'Stream' of elements that preserves the return value.
    The third parameter will often be 'id' where a fold is written by hand:

>>> S.fold (+) 0 id $ each [1..10]
55 :> ()

>>> S.fold (*) 1 id $ S.fold (+) 0 id $ S.copy $ each [1..10]
3628800 :> (55 :> ())

    It can be used to replace a standard Haskell type with one more suited to
    writing a strict accumulation function. It is also crucial to the
    Applicative instance for @Control.Foldl.Fold@  We can apply such a fold
    @purely@

> Control.Foldl.purely S.fold :: Monad m => Fold a b -> Stream (Of a) m r -> m (Of b r)

    Thus, specializing a bit:

> L.purely S.fold L.sum :: Stream (Of Int) Int r -> m (Of Int r)
> mapped (L.purely S.fold L.sum) :: Stream (Stream (Of Int)) IO r -> Stream (Of Int) IO r

    Here we use the Applicative instance for @Control.Foldl.Fold@ to
    stream three-item segments of a stream together with their sums and products.

>>> S.print $ mapped (L.purely S.fold (liftA3 (,,) L.list L.product L.sum)) $ chunksOf 3 $ each [1..10]
([1,2,3],6,6)
([4,5,6],120,15)
([7,8,9],504,24)
([10],10,10)

-}

fold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r -> m (Of b r)
fold step begin done str =  fold_loop str begin
  where
    fold_loop stream !x = case stream of
      Return r         -> return (done x :> r)
      Effect m         -> m >>= \str' -> fold_loop str' x
      Step (a :> rest) -> fold_loop rest $! step x a
{-# INLINE fold #-}


{-| Strict, monadic fold of the elements of a 'Stream (Of a)'

> Control.Foldl.impurely foldM :: Monad m => FoldM a b -> Stream (Of a) m () -> m b
-}
foldM_
    :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream (Of a) m r -> m b
foldM_ step begin done = fmap (\(a :> _) -> a) . foldM step begin done
{-#INLINE foldM_ #-}

{-| Strict, monadic fold of the elements of a 'Stream (Of a)'

> Control.Foldl.impurely foldM' :: Monad m => FoldM a b -> Stream (Of a) m r -> m (b, r)

   Thus to accumulate the elements of a stream as a vector, together with a random
   element we might write:

>>>  L.impurely S.foldM (liftA2 (,) L.vector L.random) $ each [1..10::Int] :: IO (Of (U.Vector Int,Maybe Int) ())
([1,2,3,4,5,6,7,8,9,10],Just 9) :> ()

-}
foldM
    :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream (Of a) m r ->m (Of b r)

foldM step begin done str = do
    x0 <- begin
    loop str x0
  where
    loop stream !x = case stream of
      Return r         -> done x >>= \b -> return (b :> r)
      Effect m          -> m >>= \s -> loop s x
      Step (a :> rest) -> do
        x' <- step x a
        loop rest x'
{-# INLINABLE foldM #-}

-- the following requires GHC.Magic.oneShot:
-- foldM step begin done str = do
--       x <- begin
--       (x' :> r) <- streamFold
--         (\r x -> return (x :> r))
--         (\mx2mx -> oneShot (\x -> x `seq` mx2mx >>= ($ x) ))
--         (\(a :> x2mx') -> oneShot (\x -> x `seq` (step x a >>= x2mx')) )
--         ( str)
--         x
--       b <- done x'
--       return (b :> r)
--   where seq = Prelude.seq
-- {-#INLINE foldM #-}

{-| A natural right fold for consuming a stream of elements.
    See also the more general 'iterTM' in the 'Streaming' module
    and the still more general 'destroy'

> foldrT (\a p -> Streaming.yield a >> p) = id
> foldrT (\a p -> Pipes.yield a     >> p) :: Monad m => Stream (Of a) m r -> Producer a m r
> foldrT (\a p -> Conduit.yield a   >> p) :: Monad m => Stream (Of a) m r -> Conduit a m r

-}

foldrT :: (Monad m, MonadTrans t, Monad (t m))
       => (a -> t m r -> t m r) -> Stream (Of a) m r -> t m r
foldrT step = loop where
  loop stream = case stream of
    Return r       -> return r
    Effect m       -> lift m >>= loop
    Step (a :> as) -> step a (loop as)
{-# INLINABLE foldrT #-}

{-| A natural right fold for consuming a stream of elements.
    See also the more general 'iterT' in the 'Streaming' module and the
    still more general 'destroy'
-}
foldrM :: Monad m
       => (a -> m r -> m r) -> Stream (Of a) m r -> m r
foldrM step = loop where
  loop stream = case stream of
    Return r       -> return r
    Effect m       -> m >>= loop
    Step (a :> as) -> step a (loop as)
{-# INLINABLE foldrM #-}

-- ---------------
-- for
-- ---------------

-- | @for@ replaces each element of a stream with an associated stream. Note that the
-- associated stream may layer any functor.
for :: (Monad m, Functor f) => Stream (Of a) m r -> (a -> Stream f m x) -> Stream f m r
for str0 act = loop str0 where
  loop str = case str of
    Return r         -> Return r
    Effect m          -> Effect $ fmap loop m
    Step (a :> rest) -> act a *> loop rest
{-# INLINABLE for #-}

-- -| Group layers of any functor by comparisons on a preliminary annotation

-- groupedBy
--   :: (Monad m, Functor f) =>
--      (a -> a -> Bool)
--      -> Stream (Compose (Of a) f) m r
--      -> Stream (Stream (Compose (Of a) f) m) m r
-- groupedBy equals = loop  where
--   loop stream = Effect $ do
--         e <- inspect stream
--         return $ case e of
--             Left   r      -> Return r
--             Right s@(Compose (a :> p')) -> Step $
--                 fmap loop (Step $ Compose (a :> fmap (span' (equals a)) p'))
--   span' :: (Monad m, Functor f) => (a -> Bool) -> Stream (Compose (Of a) f) m r
--         -> Stream (Compose (Of a) f) m (Stream (Compose (Of a) f) m r)
--   span' thePred = loop where
--     loop str = case str of
--       Return r         -> Return (Return r)
--       Effect m          -> Effect $ fmap loop m
--       Step s@(Compose (a :> rest)) -> case thePred a  of
--         True  -> Step (Compose (a :> fmap loop rest))
--         False -> Return (Step s)
-- {-# INLINABLE groupedBy #-}

{-| Group elements of a stream in accordance with the supplied comparison.


>>> S.print $ mapped S.toList $ S.groupBy (>=) $ each [1,2,3,1,2,3,4,3,2,4,5,6,7,6,5]
[1]
[2]
[3,1,2,3]
[4,3,2,4]
[5]
[6]
[7,6,5]

-}
groupBy :: Monad m
  => (a -> a -> Bool)
  -> Stream (Of a) m r
  -> Stream (Stream (Of a) m) m r
groupBy equals = loop  where
  loop stream = Effect $ do
        e <- next stream
        return $ case e of
            Left   r      -> Return r
            Right (a, p') -> Step $
                fmap loop (yield a >> span (equals a) p')
{-# INLINABLE groupBy #-}


{-| Group successive equal items together

>>> S.toList $ mapped S.toList $ S.group $ each "baaaaad"
["b","aaaaa","d"] :> ()

>>> S.toList $ concats $ maps (S.drained . S.splitAt 1) $ S.group $ each "baaaaaaad"
"bad" :> ()

-}
group :: (Monad m, Eq a) => Stream (Of a) m r -> Stream (Stream (Of a) m) m r
group = groupBy (==)
{-#INLINE group #-}


head :: Monad m => Stream (Of a) m r -> m (Of (Maybe a) r)
head str = case str of
  Return r            -> return (Nothing :> r)
  Effect m            -> m >>= head
  Step (a :> rest)    -> effects rest >>= \r -> return (Just a :> r)
{-#INLINABLE head #-}

head_ :: Monad m => Stream (Of a) m r -> m (Maybe a)
head_ str = case str of
  Return _ -> return Nothing
  Effect m -> m >>= head_
  Step (a :> _) -> return (Just a)
{-#INLINABLE head_ #-}

intersperse :: Monad m => a -> Stream (Of a) m r -> Stream (Of a) m r
intersperse x str = case str of
    Return r -> Return r
    Effect m -> Effect (fmap (intersperse x) m)
    Step (a :> rest) -> loop a rest
  where
  loop !a theStr = case theStr of
    Return r -> Step (a :> Return r)
    Effect m -> Effect (fmap (loop a) m)
    Step (b :> rest) -> Step (a :> Step (x :> loop b rest))
{-#INLINABLE intersperse #-}




-- ---------------
-- iterate
-- ---------------

{-| Iterate a pure function from a seed value, streaming the results forever



-}
iterate :: Monad m => (a -> a) -> a -> Stream (Of a) m r
iterate f = loop where
  loop a' = Effect (return (Step (a' :> loop (f a'))))
{-# INLINABLE iterate #-}

-- | Iterate a monadic function from a seed value, streaming the results forever
iterateM :: Monad m => (a -> m a) -> m a -> Stream (Of a) m r
iterateM f = loop where
  loop ma  = Effect $ do
    a <- ma
    return (Step (a :> loop (f a)))
{-# INLINABLE iterateM #-}



last :: Monad m => Stream (Of a) m r -> m (Of (Maybe a) r)
last = loop Nothing_ where
  loop mb str = case str of
    Return r            -> case mb of
      Nothing_ -> return (Nothing :> r)
      Just_ a  -> return (Just a :> r)
    Effect m            -> m >>= loop mb
    Step (a :> rest)  -> loop (Just_ a) rest
{-#INLINABLE last #-}



last_ :: Monad m => Stream (Of a) m r -> m (Maybe a)
last_ = loop Nothing_ where
  loop mb str = case str of
    Return _ -> case mb of
      Nothing_ -> return Nothing
      Just_ a  -> return (Just a)
    Effect m -> m >>= loop mb
    Step (a :> rest) -> loop (Just_ a) rest
{-#INLINABLE last_ #-}


-- ---------------
-- length
-- ---------------

{-| Run a stream, remembering only its length:

>>> S.length $ S.each [1..10]
10

-}
length_ :: Monad m => Stream (Of a) m r -> m Int
length_ = fold_ (\n _ -> n + 1) 0 id
{-#INLINE length_#-}

{-| Run a stream, keeping its length and its return value.

>>> S.print $ mapped S.length $ chunksOf 3 $ S.each [1..10]
3
3
3
1

-}

length :: Monad m => Stream (Of a) m r -> m (Of Int r)
length = fold (\n _ -> n + 1) 0 id
{-#INLINE length #-}
-- ---------------
-- map
-- ---------------

{-| Standard map on the elements of a stream.

>>> S.stdoutLn $ S.map reverse $ each (words "alpha beta")
ahpla
ateb
-}

map :: Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
map f =  maps (\(x :> rest) -> f x :> rest)
-- loop where  --
  -- loop stream = case stream of
  --   Return r -> Return r
  --   Effect m -> Effect (fmap loop m)
  --   Step (a :> as) -> Step (f a :> loop as)
{-# INLINABLE map #-}
-- {-# NOINLINE [1] map #-}
-- {-# RULES
-- "map/map"  [~1] forall f g bs . map f (map g bs) =
--   map (f . g) bs
-- #-}

{-| Replace each element of a stream with the result of a monadic action

>>> S.print $ S.mapM readIORef $ S.chain (\ior -> modifyIORef ior (*100)) $ S.mapM newIORef $ each [1..6]
100
200
300
400
500
600
-}
mapM :: Monad m => (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapM f = loop where
  loop str = case str of
    Return r       -> Return r
    Effect m        -> Effect (fmap loop m)
    Step (a :> as) -> Effect $ do
      a' <- f a
      return (Step (a' :> loop as) )
{-# INLINABLE mapM #-}



{-| Reduce a stream to its return value with a monadic action.

>>> S.mapM_ Prelude.print $ each [1..3]
1
2
3


>>> rest <- S.mapM_ Prelude.print $ S.splitAt 3 $ each [1..10]
1
2
3
>>> S.sum rest
49 :> ()

-}
mapM_ :: Monad m => (a -> m b) -> Stream (Of a) m r -> m r
mapM_ f = loop where
  loop str = case str of
    Return r -> return r
    Effect m -> m >>= loop
    Step (a :> as) -> f a *> loop as
{-# INLINABLE mapM_ #-}



{- | Map layers of one functor to another with a transformation involving the base monad.
     This could be trivial, e.g.

> let noteBeginning text x = putStrLn text >> return text

     this puts the
     is completely functor-general

     @maps@ and @mapped@ obey these rules:

> maps id              = id
> mapped return        = id
> maps f . maps g      = maps (f . g)
> mapped f . mapped g  = mapped (f <=< g)
> maps f . mapped g    = mapped (fmap f . g)
> mapped f . maps g    = mapped (f <=< fmap g)

     @maps@ is more fundamental than @mapped@, which is best understood as a convenience
     for effecting this frequent composition:

> mapped phi = decompose . maps (Compose . phi)


-}

mapped :: (Monad m, Functor f) => (forall x . f x -> m (g x)) -> Stream f m r -> Stream g m r
mapped = mapsM
{-#INLINE mapped #-}

{-| A version of 'mapped' that imposes a 'Functor' constraint on the target functor rather
    than the source functor. This version should be preferred if 'fmap' on the target
    functor is cheaper.

-}
mappedPost :: (Monad m, Functor g) => (forall x . f x -> m (g x)) -> Stream f m r -> Stream g m r
mappedPost = mapsMPost
{-# INLINE mappedPost #-}

{-| Fold streamed items into their monoidal sum

>>> S.mconcat $ S.take 2 $ S.map (Data.Monoid.Last . Just) (S.stdinLn)
first<Enter>
last<Enter>
Last {getLast = Just "last"} :> ()

 -}
mconcat :: (Monad m, Monoid w) => Stream (Of w) m r -> m (Of w r)
mconcat = fold mappend mempty id
{-#INLINE mconcat #-}

data Maybe_ a = Just_ !a | Nothing_
mconcat_ :: (Monad m, Monoid w) => Stream (Of w) m r -> m w
mconcat_ = fold_ mappend mempty id

minimum :: (Monad m, Ord a) => Stream (Of a) m r -> m (Of (Maybe a) r)
minimum = fold (\m a -> case m of Nothing_ -> Just_ a ; Just_ a' -> Just_ (min a a'))
               Nothing_
               (\m -> case m of Nothing_ -> Nothing; Just_ r -> Just r)
{-#INLINE minimum #-}

minimum_ :: (Monad m, Ord a) => Stream (Of a) m r -> m (Maybe a)
minimum_ = fold_ (\m a -> case m of Nothing_ -> Just_ a ; Just_ a' -> Just_ (min a a'))
                 Nothing_
                 (\m -> case m of Nothing_ -> Nothing; Just_ r -> Just r)
{-#INLINE minimum_ #-}

maximum :: (Monad m, Ord a) => Stream (Of a) m r -> m (Of (Maybe a) r)
maximum = fold (\m a -> case m of Nothing_ -> Just_ a ; Just_ a' -> Just_ (max a a'))
               Nothing_
               (\m -> case m of Nothing_ -> Nothing; Just_ r -> Just r)
{-#INLINE maximum #-}

maximum_ :: (Monad m, Ord a) => Stream (Of a) m r -> m (Maybe a)
maximum_ = fold_ (\m a -> case m of Nothing_ -> Just_ a ; Just_ a' -> Just_ (max a a'))
                 Nothing_
                 (\m -> case m of Nothing_ -> Nothing; Just_ r -> Just r)
{-#INLINE maximum_ #-}

{-| The standard way of inspecting the first item in a stream of elements, if the
     stream is still \'running\'. The @Right@ case contains a
     Haskell pair, where the more general @inspect@ would return a left-strict pair.
     There is no reason to prefer @inspect@ since, if the @Right@ case is exposed,
     the first element in the pair will have been evaluated to whnf.

> next :: Monad m => Stream (Of a) m r -> m (Either r (a, Stream (Of a) m r))
> inspect :: Monad m => Stream (Of a) m r -> m (Either r (Of a (Stream (Of a) m r)))

     Interoperate with @pipes@ producers thus:

> Pipes.unfoldr Stream.next :: Stream (Of a) m r -> Producer a m r
> Stream.unfoldr Pipes.next :: Producer a m r -> Stream (Of a) m r

     Similarly:

> IOStreams.unfoldM (fmap (either (const Nothing) Just) . next) :: Stream (Of a) IO b -> IO (InputStream a)
> Conduit.unfoldM (fmap (either (const Nothing) Just) . next)   :: Stream (Of a) m r -> Source a m r

     But see 'uncons', which is better fitted to these @unfoldM@s
-}
next :: Monad m => Stream (Of a) m r -> m (Either r (a, Stream (Of a) m r))
next = loop where
  loop stream = case stream of
    Return r         -> return (Left r)
    Effect m          -> m >>= loop
    Step (a :> rest) -> return (Right (a,rest))
{-# INLINABLE next #-}


{-| Exhaust a stream deciding whether @a@ was an element.

-}

notElem :: (Monad m, Eq a) => a -> Stream (Of a) m r -> m (Of Bool r)
notElem a' = loop True where
  loop False str = fmap (False :>) (effects str)
  loop True str = case str of
    Return r -> return (True:> r)
    Effect m -> m >>= loop True
    Step (a:> rest) ->
      if a == a'
        then fmap (False :>) (effects rest)
        else loop True rest
{-#INLINABLE notElem #-}

notElem_ :: (Monad m, Eq a) => a -> Stream (Of a) m r -> m Bool
notElem_ a' = loop True where
  loop False _ = return False
  loop True str = case str of
    Return _ -> return True
    Effect m -> m >>= loop True
    Step (a:> rest) ->
      if a == a'
        then return False
        else loop True rest
{-#INLINABLE notElem_ #-}


{-| Remove repeated elements from a Stream. 'nubOrd' of course accumulates a 'Data.Set.Set' of
    elements that have already been seen and should thus be used with care.

>>> S.toList_ $ S.nubOrd $ S.take 5 S.readLn :: IO ([Int])
1<Enter>
2<Enter>
3<Enter>
1<Enter>
2<Enter>
[1,2,3]

-}

nubOrd :: (Monad m, Ord a) => Stream (Of a) m r -> Stream (Of a) m r
nubOrd = nubOrdOn id
{-# INLINE nubOrd #-}

{-|  Use 'nubOrdOn' to have a custom ordering function for your elements. -}
nubOrdOn :: (Monad m, Ord b) => (a -> b) -> Stream (Of a) m r -> Stream (Of a) m r
nubOrdOn f xs = loop mempty xs where
  loop !set stream = case stream of
    Return r         -> Return r
    Effect m         -> Effect (liftM (loop set) m)
    Step (a :> rest) -> let !fa = f a in
      if Set.member fa set
         then loop set rest
         else Step (a :> loop (Set.insert fa set) rest)

{-| More efficient versions of above when working with 'Int's that use 'Data.IntSet.IntSet'. -}

nubInt :: Monad m => Stream (Of Int) m r -> Stream (Of Int) m r
nubInt = nubIntOn id
{-# INLINE nubInt #-}

nubIntOn :: Monad m => (a -> Int) -> Stream (Of a) m r -> Stream (Of a) m r
nubIntOn f xs = loop mempty xs where
  loop !set stream = case stream of
    Return r         -> Return r
    Effect m         -> Effect (liftM (loop set) m)
    Step (a :> rest) -> let !fa = f a in
      if IntSet.member fa set
         then loop set rest
         else Step (a :> loop (IntSet.insert fa set) rest)


{-|
> filter p = hoist effects (partition p)

 -}
partition :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) (Stream (Of a) m) r
partition thus = loop where
   loop str = case str of
     Return r -> Return r
     Effect m -> Effect (fmap loop (lift m))
     Step (a :> rest) -> if thus a
       then Step (a :> loop rest)
       else Effect $ do
               yield a
               return (loop rest)


{-| Separate left and right values in distinct streams. ('separate' is
    a more powerful, functor-general, equivalent using 'Sum' in place of 'Either').
    So, for example, to permit unlimited user
    input of @Int@s on condition of only two errors, we might write:

>>> S.toList $ S.print $ S.take 2 $ partitionEithers $ S.map readEither $ S.stdinLn  :: IO (Of [Int] ())
1<Enter>
2<Enter>
qqqqqqqqqq<Enter>
"Prelude.read: no parse"
3<Enter>
rrrrrrrrrr<Enter>
"Prelude.read: no parse"
[1,2,3] :> ()

> partitionEithers = separate . maps S.eitherToSum
> lefts  = hoist S.effects . partitionEithers
> rights = S.effects . partitionEithers
> rights = S.concat
-}
partitionEithers :: Monad m => Stream (Of (Either a b)) m r -> Stream (Of a) (Stream (Of b) m) r
partitionEithers =  loop where
   loop str = case str of
     Return r -> Return r
     Effect m -> Effect (fmap loop (lift m))
     Step (Left a :> rest) -> Step (a :> loop rest)
     Step (Right b :> rest) -> Effect $ do
       yield b
       return (loop rest)


-- | Fold a 'Stream' of numbers into their product
product_ :: (Monad m, Num a) => Stream (Of a) m () -> m a
product_ = fold_ (*) 1 id
{-# INLINE product_ #-}

{-| Fold a 'Stream' of numbers into their product with the return value

>  maps' product' :: Stream (Stream (Of Int)) m r -> Stream (Of Int) m r
-}
product :: (Monad m, Num a) => Stream (Of a) m r -> m (Of a r)
product = fold (*) 1 id
{-# INLINE product #-}


-- ---------------
-- read
-- ---------------

{- | Make a stream of strings into a stream of parsed values, skipping bad cases

>>> S.sum_ $ S.read $ S.takeWhile (/= "total") S.stdinLn :: IO Int
1000<Enter>
2000<Enter>
total<Enter>
3000


-}
read :: (Monad m, Read a) => Stream (Of String) m r -> Stream (Of a) m r
read stream = for stream $ \str -> case readMaybe str of
  Nothing -> return ()
  Just r  -> yield r
{-# INLINE read #-}

-- ---------------
-- repeat
-- ---------------
{-| Repeat an element /ad inf./ .

>>> S.print $ S.take 3 $ S.repeat 1
1
1
1
-}

repeat :: Monad m => a -> Stream (Of a) m r
repeat a = loop where loop = Effect (return (Step (a :> loop)))
{-# INLINE repeat #-}


{-| Repeat a monadic action /ad inf./, streaming its results.

>>>  S.toList $ S.take 2 $ repeatM getLine
one<Enter>
two<Enter>
["one","two"]

-}

repeatM :: Monad m => m a -> Stream (Of a) m r
repeatM ma = loop where
  loop = do
    a <- lift ma
    yield a
    loop
{-# INLINABLE repeatM #-}

-- ---------------
-- replicate
-- ---------------

-- | Repeat an element several times.
replicate :: Monad m => Int -> a -> Stream (Of a) m ()
replicate n _ | n <= 0 = return ()
replicate n a = loop n where
  loop 0 = Return ()
  loop m = Effect (return (Step (a :> loop (m-1))))
{-# INLINABLE replicate #-}

{-| Repeat an action several times, streaming its results.

>>> S.print $ S.replicateM 2 getCurrentTime
2015-08-18 00:57:36.124508 UTC
2015-08-18 00:57:36.124785 UTC

-}
replicateM :: Monad m => Int -> m a -> Stream (Of a) m ()
replicateM n _ | n <= 0 = return ()
replicateM n ma = loop n where
  loop 0 = Return ()
  loop m = Effect $ do
    a <- ma
    return (Step (a :> loop (m-1)))
{-# INLINABLE replicateM #-}

{-| Read an @IORef (Maybe a)@ or a similar device until it reads @Nothing@.
    @reread@ provides convenient exit from the @io-streams@ library

> reread readIORef    :: IORef (Maybe a) -> Stream (Of a) IO ()
> reread Streams.read :: System.IO.Streams.InputStream a -> Stream (Of a) IO ()
-}
reread :: Monad m => (s -> m (Maybe a)) -> s -> Stream (Of a) m ()
reread step s = loop where
  loop = Effect $ do
    m <- step s
    case m of
      Nothing -> return (Return ())
      Just a  -> return (Step (a :> loop))
{-# INLINABLE reread #-}

{-| Strict left scan, streaming, e.g. successive partial results. The seed 
    is yielded first, before any action of finding the next element is performed.


>>> S.print $ S.scan (++) "" id $ each (words "a b c d")
""
"a"
"ab"
"abc"
"abcd"

    'scan' is fitted for use with @Control.Foldl@, thus:

>>> S.print $ L.purely S.scan L.list $ each [3..5]
[]
[3]
[3,4]
[3,4,5]

-}
scan :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r -> Stream (Of b) m r
scan step begin done str = Step (done begin :> loop begin str)
  where                   
  loop !acc stream = do
    case stream of
      Return r -> Return r
      Effect m -> Effect (fmap (loop acc) m)
      Step (a :> rest) -> 
        let !acc' = step acc a 
        in Step (done acc' :> loop acc' rest)
{-#INLINABLE scan #-}

{-| Strict left scan, accepting a monadic function. It can be used with
    'FoldM's from @Control.Foldl@ using 'impurely'. Here we yield
    a succession of vectors each recording

>>> let v =  L.impurely scanM L.vector $ each [1..4::Int] :: Stream (Of (U.Vector Int)) IO ()
>>> S.print v
fromList []
fromList [1]
fromList [1,2]
fromList [1,2,3]
fromList [1,2,3,4]

-}
scanM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
scanM step begin done str = Effect $ do
    x <- begin
    b <- done x
    return (Step (b :> loop x str))  
  where
    loop !x stream = case stream of -- note we have already yielded from x
      Return r -> Return r
      Effect m  -> Effect (do
        stream' <- m
        return (loop x stream')
        )
      Step (a :> rest) -> Effect (do
        x' <- step x a
        b   <- done x'
        return (Step (b :> loop x' rest))
        )
{-# INLINABLE scanM #-}

{- Label each element in a stream with a value accumulated according to a fold.


>>> S.print $ S.scanned (*) 1 id $ S.each [100,200,300]
(100,100)
(200,20000)
(300,6000000)

>>> S.print $ L.purely S.scanned L.product $ S.each [100,200,300]
(100,100)
(200,20000)
(300,6000000)

-}

data Maybe' a = Just' a | Nothing'

scanned :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r -> Stream (Of (a,b)) m r
scanned step begin done = loop Nothing' begin
  where
    loop !m !x stream = do
      case stream of
        Return r -> return r
        Effect mn  -> Effect $ fmap (loop m x) mn
        Step (a :> rest) -> do
          case m of
            Nothing' -> do
              let !acc = step x a
              yield (a, done acc)
              loop (Just' a) acc rest
            Just' _ -> do
              let !acc = done (step x a)
              yield (a, acc)
              loop (Just' a) (step x a) rest
{-# INLINABLE scanned #-}


{-| Streams the number of seconds from the beginning of action

    Thus, to mark times of user input we might write something like:

>>> S.toList $ S.take 3 $ S.zip S.seconds S.stdinLn
a<Enter>
b<Enter>
c<Enter>
[(0.0,"a"),(1.088711,"b"),(3.7289649999999996,"c")] :> ()

   To restrict user input to some number of seconds, we might write:

>>> S.toList $ S.map fst $ S.zip S.stdinLn $ S.takeWhile (< 3) S.seconds
one<Enter>
two<Enter>
three<Enter>
four<Enter>
five<Enter>
["one","two","three","four","five"] :> ()

   This of course does not interrupt an action that has already begun.

  -}

-- ---------------
-- sequence
-- ---------------

{-| Like the 'Data.List.sequence' but streaming. The result type is a
    stream of a\'s, /but is not accumulated/; the effects of the elements
    of the original stream are interleaved in the resulting stream. Compare:

> sequence :: Monad m =>       [m a]           -> m [a]
> sequence :: Monad m => Stream (Of (m a)) m r -> Stream (Of a) m r

   This obeys the rule

-}
sequence :: Monad m => Stream (Of (m a)) m r -> Stream (Of a) m r
sequence = loop where
  loop stream = case stream of
    Return r          -> Return r
    Effect m           -> Effect $ fmap loop m
    Step (ma :> rest) -> Effect $ do
      a <- ma
      return (Step (a :> loop rest))
{-# INLINABLE sequence #-}

-- ---------------
-- show
-- ---------------

show :: (Monad m, Show a) => Stream (Of a) m r -> Stream (Of String) m r
show = map Prelude.show
{-# INLINE show #-}
-- ---------------
-- sum
-- ---------------

-- | Fold a 'Stream' of numbers into their sum
sum_ :: (Monad m, Num a) => Stream (Of a) m () -> m a
sum_ = fold_ (+) 0 id
{-# INLINE sum_ #-}

{-| Fold a 'Stream' of numbers into their sum with the return value

>  mapped S.sum :: Stream (Stream (Of Int)) m r -> Stream (Of Int) m r


>>> S.sum $ each [1..10]
55 :> ()

>>> (n :> rest)  <- S.sum $ S.splitAt 3 $ each [1..10]
>>> print n
6
>>> (m :> rest') <- S.sum $ S.splitAt 3 rest
>>> print m
15
>>> S.print rest'
7
8
9

-}
sum :: (Monad m, Num a) => Stream (Of a) m r -> m (Of a r)
sum = fold (+) 0 id
{-# INLINABLE sum #-}

-- ---------------
-- span
-- ---------------

-- | Stream elements until one fails the condition, return the rest.
span :: Monad m => (a -> Bool) -> Stream (Of a) m r
      -> Stream (Of a) m (Stream (Of a) m r)
span thePred = loop where
  loop str = case str of
    Return r         -> Return (Return r)
    Effect m          -> Effect $ fmap loop m
    Step (a :> rest) -> if thePred a
      then Step (a :> loop rest)
      else Return (Step (a :> rest))
{-# INLINABLE span #-}


{-| Split a stream of elements wherever a given element arises.
    The action is like that of 'Prelude.words'.

>>> S.stdoutLn $ mapped S.toList $ S.split ' ' $ each "hello world  "
hello
world

-}

split :: (Eq a, Monad m) =>
      a -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r
split t  = loop  where
  loop stream = case stream of
    Return r -> Return r
    Effect m -> Effect (fmap loop m)
    Step (a :> rest) ->
         if a /= t
            then Step (fmap loop (yield a >> break (== t) rest))
            else loop rest
{-#INLINABLE split #-}

{-| Split a succession of layers after some number, returning a streaming or
    effectful pair. This function is the same as the 'splitsAt' exported by the
    @Streaming@ module, but since this module is imported qualified, it can
    usurp a Prelude name. It specializes to:

>  splitAt :: (Monad m, Functor f) => Int -> Stream (Of a) m r -> Stream (Of a) m (Stream (Of a) m r)

-}
splitAt :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
splitAt = splitsAt
{-# INLINE splitAt #-}

-- -------
-- subst
-- -------
{-| Replace each element in a stream of individual values with a functorial
    layer of any sort. @subst = flip with@ and is more convenient in
    a sequence of compositions that transform a stream.

> with = flip subst
> for str f = concats $ subst f str
> subst f = maps (\(a:>r) -> r <$ f a)
> S.concat = concats . subst each
-}
subst :: (Monad m, Functor f) =>  (a -> f x) -> Stream (Of a) m r -> Stream f m r
subst f s = loop s where
  loop str = case str of
    Return r         -> Return r
    Effect m         -> Effect (fmap loop m)
    Step (a :> rest) -> Step (loop rest <$ f a)
{-#INLINABLE subst #-}
-- ---------------
-- take
-- ---------------

{-| End a stream after n elements; the original return value is thus lost.
    'splitAt' preserves this information. Note that, like @splitAt@, this
    function is functor-general, so that, for example, you can @take@ not
    just a number of items from a stream of elements, but a number
    of substreams and the like.

>>> S.toList $ S.take 3 $ each "with"
"wit" :> ()

>>> runResourceT $ S.stdoutLn $ S.take 3 $ S.readFile "stream.hs"
import Streaming
import qualified Streaming.Prelude as S
import Streaming.Prelude (each, next, yield)


-}

take :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
take n0 _ | n0 <= 0 = return ()
take n0 str = loop n0 str where
  loop 0 _ = return ()
  loop n p =
    case p of
      Step fas -> Step (fmap (loop (n-1)) fas)
      Effect m -> Effect (fmap (loop n) m)
      Return _ -> Return ()
{-# INLINABLE take #-}

-- ---------------
-- takeWhile
-- ---------------

{-| End stream when an element fails a condition; the original return value is lost.
    By contrast 'span' preserves this information, and is generally more desirable.

> S.takeWhile thus = void . S.span thus

    To preserve the information - but thus also force the rest of the stream to be
    developed - write

> S.drained . S.span thus

    as @dropWhile thus@ is

> S.effects . S.span thus

-}
takeWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhile thePred = loop where
  loop str = case str of
    Step (a :> as) -> when (thePred a) (Step (a :> loop as))
    Effect m -> Effect (fmap loop m)
    Return _ -> Return ()
{-# INLINE takeWhile #-}

{-| Like 'takeWhile', but takes a monadic predicate. -}
takeWhileM :: Monad m => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhileM thePred = loop where
  loop str = case str of
    Step (a :> as) -> do
      b <- lift (thePred a)
      when b (Step (a :> loop as))
    Effect m -> Effect (fmap loop m)
    Return _ -> Return ()
{-# INLINE takeWhileM #-}


{-| Convert an effectful 'Stream (Of a)' into a list of @as@

    Note: Needless to say, this function does not stream properly.
    It is basically the same as Prelude 'mapM' which, like 'replicateM',
    'sequence' and similar operations on traversable containers
    is a leading cause of space leaks.

-}
toList_ :: Monad m => Stream (Of a) m r -> m [a]
toList_ = fold_ (\diff a ls -> diff (a: ls)) id (\diff -> diff [])
{-# INLINE toList_ #-}


{-| Convert an effectful 'Stream' into a list alongside the return value

>  mapped toList :: Stream (Stream (Of a)) m r -> Stream (Of [a]) m

    Like 'toList_', 'toList' breaks streaming; unlike 'toList_' it /preserves the return value/ 
    and thus is frequently useful with e.g. 'mapped'

>>> S.print $ mapped S.toList $ chunksOf 3 $ each [1..9]
[1,2,3]
[4,5,6]
[7,8,9]
>>> S.print $ mapped S.toList $ chunksOf 2 $ S.replicateM 4 getLine
s<Enter>
t<Enter>
["s","t"]
u<Enter>
v<Enter>
["u","v"] 
-}
toList :: Monad m => Stream (Of a) m r -> m (Of [a] r)
toList = fold (\diff a ls -> diff (a: ls)) id (\diff -> diff [])
{-# INLINE toList #-}


{-| Inspect the first item in a stream of elements, without a return value.
    @uncons@ provides convenient exit into another streaming type:

> IOStreams.unfoldM uncons :: Stream (Of a) IO b -> IO (InputStream a)
> Conduit.unfoldM uncons   :: Stream (Of a) m r -> Conduit.Source m a

-}
uncons :: Monad m => Stream (Of a) m r -> m (Maybe (a, Stream (Of a) m r))
uncons = loop where
  loop stream = case stream of
    Return _         -> return Nothing
    Effect m          -> m >>= loop
    Step (a :> rest) -> return (Just (a,rest))
{-# INLINABLE uncons #-}


{-| Build a @Stream@ by unfolding steps starting from a seed. In particular note
    that @S.unfoldr S.next = id@.

    The seed can of course be anything, but this is one natural way
    to consume a @pipes@ 'Pipes.Producer'. Consider:

>>> S.stdoutLn $ S.take 2 $ S.unfoldr Pipes.next Pipes.stdinLn
hello<Enter>
hello
goodbye<Enter>
goodbye

>>> S.stdoutLn $ S.unfoldr Pipes.next (Pipes.stdinLn >-> Pipes.take 2)
hello<Enter>
hello
goodbye<Enter>
goodbye

>>> S.effects $ S.unfoldr Pipes.next (Pipes.stdinLn >-> Pipes.take 2 >-> Pipes.stdoutLn)
hello<Enter>
hello
goodbye<Enter>
goodbye

    @Pipes.unfoldr S.next@ similarly unfolds a @Pipes.Producer@ from a stream.

-}
unfoldr :: Monad m
        => (s -> m (Either r (a, s))) -> s -> Stream (Of a) m r
unfoldr step = loop where
  loop s0 = Effect (do
    e <- step s0
    case e of
      Left r      -> return (Return r)
      Right (a,s) -> return (Step (a :> loop s)))
{-# INLINABLE unfoldr #-}

-- ---------------------------------------
-- untilRight
-- ---------------------------------------
untilRight :: Monad m => m (Either a r) -> Stream (Of a) m r
untilRight act = Effect loop where
  loop = do
    e <- act
    case e of
      Right r -> return (Return r)
      Left a -> return (Step (a :> Effect loop))
{-#INLINABLE untilRight #-}

-- ---------------------------------------
-- with
-- ---------------------------------------

{-| Replace each element in a stream of individual Haskell values (a @Stream (Of a) m r@) with an associated 'functorial' step.

> for str f  = concats (with str f)
> with str f = for str (yields . f)
> with str f = maps (\(a:>r) -> r <$ f a) str
> with = flip subst
> subst = flip with

>>> with (each [1..3]) (yield . show) & intercalates (yield "--") & S.stdoutLn
1
--
2
--
3
 -}
with :: (Monad m, Functor f) => Stream (Of a) m r -> (a -> f x) -> Stream f m r
with s f = loop s where
  loop str = case str of
    Return r         -> Return r
    Effect m         -> Effect (fmap loop m)
    Step (a :> rest) -> Step (loop rest <$ f a)
{-#INLINABLE with #-}

-- ---------------------------------------
-- yield
-- ---------------------------------------

{-| A singleton stream

>>> stdoutLn $ yield "hello"
hello

>>> S.sum $ do {yield 1; yield 2; yield 3}
6

>>> let number = lift (putStrLn "Enter a number:") >> lift readLn >>= yield :: Stream (Of Int) IO ()
>>> S.toList $ do {number; number; number}
Enter a number:
1<Enter>
Enter a number:
2<Enter>
Enter a number:
3<Enter>
[1,2,3] :> ()

-}

yield :: Monad m => a -> Stream (Of a) m ()
yield a = Step (a :> Return ())
{-# INLINE yield #-}

-- | Zip two 'Stream's
zip :: Monad m
    => (Stream (Of a) m r)
    -> (Stream (Of b) m r)
    -> (Stream (Of (a,b)) m r)
zip = zipWith (,)
{-# INLINE zip #-}

-- | Zip two 'Stream's using the provided combining function
zipWith :: Monad m
    => (a -> b -> c)
    -> (Stream (Of a) m r)
    -> (Stream (Of b) m r)
    -> (Stream (Of c) m r)
zipWith f = loop
  where
    loop str0 str1 = case str0 of
      Return r          -> Return r
      Effect m           -> Effect $ fmap (\str -> loop str str1) m
      Step (a :> rest0) -> case str1 of
        Return r          -> Return r
        Effect m           -> Effect $ fmap (loop str0) m
        Step (b :> rest1) -> Step (f a b :>loop rest0 rest1)
{-# INLINABLE zipWith #-}


-- | Zip three 'Stream's with a combining function
zipWith3 :: Monad m =>
       (a -> b -> c -> d)
       -> Stream (Of a) m r
       -> Stream (Of b) m r
       -> Stream (Of c) m r
       -> Stream (Of d) m r
zipWith3 op = loop where
  loop str0 str1 str2 = do
    e0 <- lift (next str0)
    case e0 of
      Left r0 -> return r0
      Right (a0,rest0) -> do
        e1 <- lift (next str1)
        case e1 of
          Left r1 -> return r1
          Right (a1,rest1) -> do
            e2 <- lift (next str2)
            case e2 of
              Left r2 -> return r2
              Right (a2,rest2) -> do
                yield (op a0 a1 a2)
                loop rest0 rest1 rest2
{-# INLINABLE zipWith3 #-}


-- | Zip three 'Stream's together
zip3 :: Monad m
    => (Stream (Of a) m r)
    -> (Stream (Of b) m r)
    -> (Stream (Of c) m r)
    -> (Stream (Of (a,b,c)) m r)
zip3 = zipWith3 (,,)
{-# INLINABLE zip3 #-}

-- --------------
-- IO fripperies
-- --------------

{-| View standard input as a @Stream (Of String) m r@. By contrast, 'stdoutLn' renders a @Stream (Of String) m r@ to standard output. The names
    follow @Pipes.Prelude@

>>> stdoutLn stdinLn
hello<Enter>
hello
world<Enter>
world
^CInterrupted.


>>> stdoutLn $ S.map reverse stdinLn
hello<Enter>
olleh
world<Enter>
dlrow
^CInterrupted.

-}
stdinLn :: MonadIO m => Stream (Of String) m ()
stdinLn = fromHandle IO.stdin
{-# INLINABLE stdinLn #-}

{-| Read values from 'IO.stdin', ignoring failed parses.

>>> :set -XTypeApplications
>>> S.sum $ S.take 2 (S.readLn @IO @Int)
10<Enter>
12<Enter>
22 :> ()

>>> S.toList $ S.take 2 (S.readLn @IO @Int)
10<Enter>
1@#$%^&*\<Enter>
12<Enter>
[10,12] :> ()

-}

readLn :: (MonadIO m, Read a) => Stream (Of a) m ()
readLn = loop where 
  loop = do
    eof <- liftIO IO.isEOF
    unless eof $ do
      str <- liftIO getLine
      case readMaybe str of
        Nothing -> readLn
        Just n  -> yield n >> loop
{-# INLINABLE readLn #-}


{-| Read 'String's from a 'IO.Handle' using 'IO.hGetLine'

    Terminates on end of input

>>> IO.withFile "/usr/share/dict/words" IO.ReadMode $ S.stdoutLn . S.take 3 . S.drop 50000 .  S.fromHandle
deflagrator
deflate
deflation

-}
fromHandle :: MonadIO m => IO.Handle -> Stream (Of String) m ()
fromHandle h = go
  where
    go = do
        eof <- liftIO $ IO.hIsEOF h
        unless eof $ do
            str <- liftIO $ IO.hGetLine h
            yield str
            go
{-# INLINABLE fromHandle #-}

{-| Write a succession of strings to a handle as separate lines.

>>> S.toHandle IO.stdout $ each (words "one two three")
one
two
three
-}
toHandle :: MonadIO m => IO.Handle -> Stream (Of String) m r -> m r
toHandle handle = loop where
  loop str = case str of
    Return r         -> return r
    Effect m          -> m >>= loop
    Step (s :> rest) -> do
      liftIO (IO.hPutStrLn handle s)
      loop rest
{-# INLINABLE toHandle #-}

{-| Print the elements of a stream as they arise.

>>> S.print $ S.take 2 S.stdinLn
hello<Enter>
"hello"
world<Enter>
"world"
>>>

-}
print :: (MonadIO m, Show a) => Stream (Of a) m r -> m r
print = loop where
  loop stream = case stream of
    Return r         -> return r
    Effect m         -> m >>= loop
    Step (a :> rest) -> do
      liftIO (Prelude.print a)
      loop rest


{-| Write 'String's to 'IO.stdout' using 'putStrLn'; terminates on a broken output pipe
    (The name and implementation are modelled on the @Pipes.Prelude@ @stdoutLn@).

>>> S.stdoutLn $ S.take 3 $ S.each $ words "one two three four five"
one
two
three
-}
stdoutLn :: MonadIO m => Stream (Of String) m () -> m ()
stdoutLn = loop
  where
    loop stream = case stream of
      Return _         -> return ()
      Effect m          -> m >>= loop
      Step (s :> rest) -> do
        x   <- liftIO $ try (putStrLn s)
        case x of
           Left (G.IOError { G.ioe_type  = G.ResourceVanished
                           , G.ioe_errno = Just ioe })
                | Errno ioe == ePIPE
                    -> return ()
           Left  e  -> liftIO (throwIO e)
           Right () -> loop rest
{-# INLINABLE stdoutLn #-}

{-| Read the lines of a file, using a function of the type: \'@'Stream' ('Of' 'String') 'IO' () -> 'IO' a@\'
    to turn the stream into a value of type \''IO' a\'.

>>> S.writeFile "lines.txt" $ S.take 2 S.stdinLn
hello<Enter>
world<Enter>
>>> S.readFile "lines.txt" S.print
"hello"
"world"

-}
readFile :: FilePath -> (Stream (Of String) IO () -> IO a) -> IO a
readFile f s = IO.withFile f IO.ReadMode $ \h -> s (fromHandle h)

{-| Write a series of 'String's as lines to a file.

>>> S.writeFile "lines.txt" $ S.take 2 S.stdinLn
hello<Enter>
world<Enter>

>>> S.stdoutLn $ S.readFile "lines.txt"
hello
world

-}
writeFile :: FilePath -> Stream (Of String) IO r -> IO r
writeFile f = IO.withFile f IO.WriteMode . flip toHandle

{-| Write 'String's to 'IO.stdout' using 'putStrLn'

    Unlike @stdoutLn@, @stdoutLn'@ does not handle a broken output pipe. Thus it can have a polymorphic return
    value, rather than @()@, and this kind of \"connect and resume\" is possible:

>>> rest <- S.stdoutLn' $ S.show $ S.splitAt 3 (each [1..5])
1
2
3
>>> S.toList rest
[4,5] :> ()

-}

stdoutLn' :: MonadIO m => Stream (Of String) m r -> m r
stdoutLn' = toHandle IO.stdout


-- -- * Producers
-- -- $producers
--   stdinLn  --
-- , readLn --
-- , fromHandle --
-- , repeatM --
-- , replicateM --
--
-- -- * Consumers
-- -- $consumers
-- , stdoutLn --
-- , stdoutLn' --
-- , mapM_ --
-- , print --
-- , toHandle --
-- , effects --
--
-- -- * Pipes
-- -- $pipes
-- , map --
-- , mapM --
-- , sequence --
-- , mapFoldable --
-- , filter --
-- , filterM --
-- , take --
-- , takeWhile --
-- , takeWhile' --
-- , drop --
-- , dropWhile --
-- , concat --
-- , elemIndices
-- , findIndices
-- , scan --
-- , scanM --
-- , chain --
-- , read --
-- , show --
-- , seq --
--
-- -- * Folds
-- -- $folds
-- , fold --
-- , fold' --
-- , foldM --
-- , foldM' --
-- , all
-- , any
-- , and
-- , or
-- , elem
-- , notElem
-- , find
-- , findIndex
-- , head
-- , index
-- , last
-- , length
-- , maximum
-- , minimum
-- , null
-- , sum --
-- , product --
-- , toList --
-- , toListM --
-- , toListM' --
--
-- -- * Zips
-- , zip --
-- , zipWith --
--

distinguish :: (a -> Bool) -> Of a r -> Sum (Of a) (Of a) r
distinguish predicate (a :> b) = if predicate a then InR (a :> b) else InL (a :> b)
{-#INLINE distinguish #-}

sumToEither ::Sum (Of a) (Of b) r ->  Of (Either a b) r
sumToEither s = case s of
  InL (a :> r) -> Left a :> r
  InR (b :> r) -> Right b :> r
{-#INLINE sumToEither #-}

eitherToSum :: Of (Either a b) r -> Sum (Of a) (Of b) r
eitherToSum s = case s of
  Left a :> r  -> InL (a :> r)
  Right b :> r -> InR (b :> r)
{-#INLINE eitherToSum #-}

composeToSum ::  Compose (Of Bool) f r -> Sum f f r
composeToSum x = case x of
  Compose (True :> f) -> InR f
  Compose (False :> f) -> InL f
{-#INLINE composeToSum #-}

sumToCompose :: Sum f f r -> Compose (Of Bool) f r
sumToCompose x = case x of
  InR f -> Compose (True :> f)
  InL f -> Compose (False :> f)
{-#INLINE sumToCompose #-}

{-| Store the result of any suitable fold over a stream, keeping the stream for
    further manipulation. @store f = f . copy@ :

>>> S.print $ S.store S.product $ each [1..4]
1
2
3
4
24 :> ()

>>> S.print $ S.store S.sum $ S.store S.product $ each [1..4]
1
2
3
4
10 :> (24 :> ())

   Here the sum (10) and the product (24) have been \'stored\' for use when
   finally we have traversed the stream with 'print' . Needless to say,
   a second 'pass' is excluded conceptually, so the
   folds that you apply successively with @store@ are performed
   simultaneously, and in constant memory -- as they would be if,
   say, you linked them together with @Control.Fold@:

>>> L.impurely S.foldM (liftA3 (\a b c -> (b,c)) (L.sink print) (L.generalize L.sum) (L.generalize L.product)) $ each [1..4]
1
2
3
4
(10,24) :> ()

   Fusing folds after the fashion of @Control.Foldl@ will generally be a bit faster
   than the corresponding succession of uses of 'store', but by
   constant factor that will be completely dwarfed when any IO is at issue.

   But 'store' / 'copy' is /much/ more powerful, as you can see by reflecting on
   uses like this:

>>> S.sum $ S.store (S.sum . mapped S.product . chunksOf 2) $ S.store (S.product . mapped S.sum . chunksOf 2 )$ each [1..6]
21 :> (44 :> (231 :> ()))

   It will be clear that this cannot be reproduced with any combination of lenses,
   @Control.Fold@ folds, or the like.  (See also the discussion of 'copy'.)

   It would conceivable be clearer to import a series of specializations of 'store'.
   It is intended to be used at types like these:

> storeM ::  (forall s m . Monad m => Stream (Of a) m s -> m (Of b s))
>         -> (Monad n => Stream (Of a) n r -> Stream (Of a) n (Of b r))
> storeM = store
>
> storeMIO :: (forall s m . MonadIO m => Stream (Of a) m s -> m (Of b s))
>          -> ( MonadIO n => Stream (Of a) n r -> Stream (Of a) n (Of b r)
> storeMIO = store

    It is clear from these types that we are just using the general instances:

> instance (Functor f, Monad m )  => Monad (Stream f m)
> instance (Functor f, MonadIO m) => MonadIO (Stream f m)

    We thus can't be touching the elements of the stream, or the final return value.
    It is the same with other constraints that @Stream (Of a)@ inherits from the underlying monad,
    like 'MonadResource'.  Thus I can independently filter and write to one file, but
    nub and write to another, or interact with a database and a logfile and the like:

>>> runResourceT $ (S.writeFile "hello2.txt" . S.nubOrd) $ store (S.writeFile "hello.txt" . S.filter (/= "world")) $ each ["hello", "world", "goodbye", "world"]
>>> :! cat hello.txt
hello
goodbye
>>> :! cat hello2.txt
hello
world
goodbye


-}
store
  :: Monad m =>
     (Stream (Of a) (Stream (Of a) m) r -> t) -> Stream (Of a) m r -> t
store f x = f (copy x)
{-#INLINE store #-}

{-| Duplicate the content of stream, so that it can be acted on twice in different ways,
    but without breaking streaming. Thus, with @each [1,2]@ I might do:

>>> S.print $ each ["one","two"]
"one"
"two"
>>> S.stdoutLn $ each ["one","two"]
one
two

    With copy, I can do these simultaneously:

>>> S.print $ S.stdoutLn $ S.copy $ each ["one","two"]
one
"one"
two
"two"

    'copy' should be understood together with 'effects' and is subject to the rules

> S.effects . S.copy       = id
> hoist S.effects . S.copy = id

    The similar operations in 'Data.ByteString.Streaming' obey the same rules.

    Where the actions you are contemplating are each simple folds over
    the elements, or a selection of elements, then the coupling of the
    folds is often more straightforwardly effected with `Control.Foldl`,
    e.g.

>>> L.purely S.fold (liftA2 (,) L.sum L.product) $ each [1..10]
(55,3628800) :> ()

    rather than

>>> S.sum $ S.product . S.copy $ each [1..10]
55 :> (3628800 :> ())

    A @Control.Foldl@ fold can be altered to act on a selection of elements by
    using 'Control.Foldl.handles' on an appropriate lens. Some such
    manipulations are simpler and more 'Data.List'-like, using 'copy':

>>> L.purely S.fold (liftA2 (,) (L.handles (filtered odd) L.sum) (L.handles (filtered even) L.product)) $ each [1..10]
(25,3840) :> ()

     becomes

>>> S.sum $ S.filter odd $ S.product $ S.filter even $ S.copy $ each [1..10]
25 :> (3840 :> ())

    or using 'store'

>>> S.sum $ S.filter odd $ S.store (S.product . S.filter even) $ each [1..10]
25 :> (3840 :> ())

    But anything that fold of a @Stream (Of a) m r@ into e.g. an @m (Of b r)@
    that has a constraint on @m@ that is carried over into @Stream f m@ -
    e.g. @Monad@, @MonadIO@, @MonadResource@, etc. can be used on the stream.
    Thus, I can fold over different groupings of the original stream:

>>>  (S.toList . mapped S.toList . chunksOf 5) $  (S.toList . mapped S.toList . chunksOf 3) $ S.copy $ each [1..10]
[[1,2,3,4,5],[6,7,8,9,10]] :> ([[1,2,3],[4,5,6],[7,8,9],[10]] :> ())

    The procedure can be iterated as one pleases, as one can see from this (otherwise unadvisable!) example:

>>>  (S.toList . mapped S.toList . chunksOf 4) $ (S.toList . mapped S.toList . chunksOf 3) $ S.copy $ (S.toList . mapped S.toList . chunksOf 2) $ S.copy $ each [1..12]
[[1,2,3,4],[5,6,7,8],[9,10,11,12]] :> ([[1,2,3],[4,5,6],[7,8,9],[10,11,12]] :> ([[1,2],[3,4],[5,6],[7,8],[9,10],[11,12]] :> ()))


@copy@ can be considered a special case of 'expand':

@
  copy = 'expand' $ \p (a :> as) -> a :> p (a :> as)
@

If 'Of' were an instance of 'Control.Comonad.Comonad', then one could write

@
  copy = 'expand' extend
@
-}
copy
  :: Monad m =>
     Stream (Of a) m r -> Stream (Of a) (Stream (Of a) m) r
copy = Effect . return . loop where
  loop str = case str of
    Return r         -> Return r
    Effect m         -> Effect (fmap loop (lift m))
    Step (a :> rest) -> Effect (Step (a :> Return (Step (a :> loop rest))))
{-#INLINABLE copy#-}

duplicate
  :: Monad m =>
     Stream (Of a) m r -> Stream (Of a) (Stream (Of a) m) r
duplicate = copy
{-#INLINE duplicate #-}

{-| The type

> Data.List.unzip     :: [(a,b)] -> ([a],[b])

   might lead us to expect

> Streaming.unzip :: Stream (Of (a,b)) m r -> Stream (Of a) m (Stream (Of b) m r)

   which would not stream, since it would have to accumulate the second stream (of @b@s).
   Of course, @Data.List@ 'Data.List.unzip' doesn't stream either.

   This @unzip@ does
   stream, though of course you can spoil this by using e.g. 'toList':

>>> let xs =  map (\x-> (x,show x)) [1..5::Int]

>>> S.toList $ S.toList $ S.unzip (S.each xs)
["1","2","3","4","5"] :> ([1,2,3,4,5] :> ())

>>> Prelude.unzip xs
([1,2,3,4,5],["1","2","3","4","5"])

    Note the difference of order in the results. It may be of some use to think why.
    The first application of 'toList' was applied to a stream of integers:

>>> :t S.unzip $ S.each xs
S.unzip $ S.each xs :: Monad m => Stream (Of Int) (Stream (Of String) m) ()

    Like any fold, 'toList' takes no notice of the monad of effects.

> toList :: Monad m => Stream (Of a) m r -> m (Of [a] r)

    In the case at hand (since I am in @ghci@) @m = Stream (Of String) IO@.
    So when I apply 'toList', I exhaust that stream of integers, folding
    it into a list:

>>> :t S.toList $ S.unzip $ S.each xs
S.toList $ S.unzip $ S.each xs
  :: Monad m => Stream (Of String) m (Of [Int] ())

    When I apply 'toList' to /this/, I reduce everything to an ordinary action in @IO@,
    and return a list of strings:

>>> S.toList $ S.toList $ S.unzip (S.each xs)
["1","2","3","4","5"] :> ([1,2,3,4,5] :> ())

'unzip' can be considered a special case of either 'unzips' or 'expand':

@
  unzip = 'unzips' . 'maps' (\((a,b) :> x) -> Compose (a :> b :> x))
  unzip = 'expand' $ \p ((a,b) :> abs) -> b :> p (a :> abs)
@
-}
unzip :: Monad m =>  Stream (Of (a,b)) m r -> Stream (Of a) (Stream (Of b) m) r
unzip = loop where
 loop str = case str of
   Return r -> Return r
   Effect m -> Effect (fmap loop (lift m))
   Step ((a,b):> rest) -> Step (a :> Effect (Step (b :> Return (loop rest))))
{-#INLINABLE unzip #-}



{- $merging
   These functions combine two sorted streams of orderable elements
   into one sorted stream. The elements of the merged stream are
   guaranteed to be in a sorted order if the two input streams are
   also sorted.

   The merge operation is /left-biased/: when merging two elements
   that compare as equal, the left element is chosen first.
-}

{- | Merge two streams of elements ordered with their 'Ord' instance.

   The return values of both streams are returned.

>>> S.print $ merge (each [1,3,5]) (each [2,4])
1
2
3
4
5
((), ())

-}
merge :: (Monad m, Ord a)
  => Stream (Of a) m r
  -> Stream (Of a) m s
  -> Stream (Of a) m (r, s)
merge = mergeBy compare
{-# INLINE merge #-}

{- | Merge two streams, ordering them by applying the given function to
   each element before comparing.

   The return values of both streams are returned.
-}
mergeOn :: (Monad m, Ord b)
  => (a -> b)
  -> Stream (Of a) m r
  -> Stream (Of a) m s
  -> Stream (Of a) m (r, s)
mergeOn f = mergeBy (comparing f)
{-# INLINE mergeOn #-}

{- | Merge two streams, ordering the elements using the given comparison function.

   The return values of both streams are returned.
-}
mergeBy :: Monad m
  => (a -> a -> Ordering)
  -> Stream (Of a) m r
  -> Stream (Of a) m s
  -> Stream (Of a) m (r, s)
mergeBy cmp = loop
  where
    loop str0 str1 = case str0 of
      Return r0         -> (\ r1 -> (r0, r1)) <$> str1
      Effect m          -> Effect $ fmap (\ str -> loop str str1) m
      Step (a :> rest0) -> case str1 of
        Return r1         -> (\ r0 -> (r0, r1)) <$> str0
        Effect m          -> Effect $ fmap (loop str0) m
        Step (b :> rest1) -> case cmp a b of
          LT -> Step (a :> loop rest0 str1)
          EQ -> Step (a :> loop rest0 str1) -- left-biased
          GT -> Step (b :> loop str0 rest1)
{-# INLINABLE mergeBy #-}        

{- $maybes
    These functions discard the 'Nothing's that they encounter. They are analogous
    to the functions from @Data.Maybe@ that share their names.
-}

{-| The 'catMaybes' function takes a 'Stream' of 'Maybe's and returns
    a 'Stream' of all of the 'Just' values. 'concat' has the same behavior,
    but is more general; it works for any foldable container type. 
-}
catMaybes :: Monad m => Stream (Of (Maybe a)) m r -> Stream (Of a) m r
catMaybes = loop where
  loop stream = case stream of
    Return r -> Return r
    Effect m -> Effect (fmap loop m)
    Step (ma :> snext) -> case ma of
      Nothing -> loop snext
      Just a -> Step (a :> loop snext)
{-#INLINABLE catMaybes #-}

{-| The 'mapMaybe' function is a version of 'map' which can throw out elements. In particular,
    the functional argument returns something of type @'Maybe' b@. If this is 'Nothing', no element
    is added on to the result 'Stream'. If it is @'Just' b@, then @b@ is included in the result 'Stream'.
    
-}
mapMaybe :: Monad m => (a -> Maybe b) -> Stream (Of a) m r -> Stream (Of b) m r
mapMaybe phi = loop where
  loop stream = case stream of
    Return r -> Return r
    Effect m -> Effect (fmap loop m)
    Step (a :> snext) -> case phi a of
      Nothing -> loop snext
      Just b -> Step (b :> loop snext)
{-#INLINABLE mapMaybe #-}

{-| 'slidingWindow' accumulates the first @n@ elements of a stream, 
     update thereafter to form a sliding window of length @n@.
     It follows the behavior of the slidingWindow function in 
     <https://hackage.haskell.org/package/conduit-combinators-1.0.4/docs/Data-Conduit-Combinators.html#v:slidingWindow conduit-combinators>.

>>> S.print $ slidingWindow 4 $ S.each "123456"
fromList "1234"
fromList "2345"
fromList "3456"

-}

slidingWindow :: Monad m 
  => Int 
  -> Stream (Of a) m b 
  -> Stream (Of (Seq.Seq a)) m b
slidingWindow n = setup (max 1 n :: Int) mempty 
  where 
    window !sequ str = do 
      e <- lift (next str) 
      case e of 
        Left r -> return r
        Right (a,rest) -> do 
          yield (sequ Seq.|> a)
          window (Seq.drop 1 $ sequ Seq.|> a) rest
    setup 0 !sequ str = do
       yield sequ 
       window (Seq.drop 1 sequ) str 
    setup m sequ str = do 
      e <- lift $ next str 
      case e of 
        Left r ->  yield sequ >> return r
        Right (x,rest) -> setup (m-1) (sequ Seq.|> x) rest
{-#INLINABLE slidingWindow #-}

-- | Map monadically over a stream, producing a new stream
--   only containing the 'Just' values.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Stream (Of a) m r -> Stream (Of b) m r
mapMaybeM phi = loop where
  loop stream = case stream of
    Return r -> Return r
    Effect m -> Effect (fmap loop m)
    Step (a :> snext) -> Effect $ do
      flip fmap (phi a) $ \x -> case x of
        Nothing -> loop snext
        Just b -> Step (b :> loop snext)
{-#INLINABLE mapMaybeM #-}

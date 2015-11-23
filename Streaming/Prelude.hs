{-| This module is very closely modeled on Pipes.Prelude; it attempts to 
    simplify and optimize the conception of Producer manipulation contained
    in Pipes.Group, Pipes.Parse and the like. This is very simple and unmysterious;
    it is independent of piping and conduiting, and can be used with any 
    rational \"streaming IO\" system. 

    Import qualified thus:

> import Streaming
> import qualified Streaming.Prelude as S

    For the examples below, one sometimes needs

> import Streaming.Prelude (each, yield, stdoutLn, stdinLn)
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
{-# LANGUAGE RankNTypes, BangPatterns, DeriveDataTypeable, TypeFamilies,
             DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
             
module Streaming.Prelude (
    -- * Types
    Of (..)

    -- * Introducing streams of elements
    -- $producers
    , yield
    , each
    , unfoldr
    , stdinLn
    , readLn
    , fromHandle
    , readFile
    , iterate
    , repeat
    , replicate
    , cycle
    , repeatM
    , replicateM
    , enumFrom
    , enumFromThen
    , seconds
    
    -- * Consuming streams of elements
    -- $consumers
    , stdoutLn
    , stdoutLn'
    , mapM_
    , print
    , toHandle
    , writeFile
    , effects
    , drained
    

    -- * Stream transformers
    -- $pipes
    , map
    , mapM
    , chain
    , maps
    , sequence
    , nub
    , filter
    , filterM
    , for
    , delay
    , intersperse
    , take
    , takeWhile
--    , takeWhile'
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
    , duplicate

    -- * Splitting and inspecting streams of elements
    , next
    , uncons
    , splitAt
    , split
--    , breaks
    , break
    , breakWhen
    , span
    , group
    , groupBy
 --   , groupedBy
 --   , split
 

    -- * Sum and Compose manipulation
    
    , distinguish   
    , switch
    , separate
    , unseparate
    , eitherToSum
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
    , last,
    , last_
    , elem
    , elem_
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

    -- * Zips and unzips
    , zip
    , zipWith
    , zip3
    , zipWith3
    , unzip
    
    -- * Pair manipulation
    , lazily
    , strictly
    , fst'
    , snd'
    
    -- * Interoperation
    , reread
    
    -- * Basic Type
    , Stream

  ) where
import Streaming.Internal

import Control.Monad hiding (filterM, mapM, mapM_, foldM, foldM_, replicateM, sequence)
import Data.Data ( Data, Typeable )
import Data.Functor.Identity
import Data.Functor.Sum
import Control.Monad.Trans
import Control.Applicative (Applicative (..))
import Data.Functor (Functor (..), (<$))

import qualified Prelude as Prelude                
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Foldable as Foldable
import Text.Read (readMaybe)
import Prelude hiding (map, mapM, mapM_, filter, drop, dropWhile, take, mconcat, sum, product
                      , iterate, repeat, cycle, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo, enumFromThen, length
                      , print, zipWith, zip, zipWith3, zip3, unzip, seq, show, read
                      , readLn, sequence, concat, span, break, readFile, writeFile
                      , minimum, maximum, elem, intersperse, all, any, head, last)

import qualified GHC.IO.Exception as G
import qualified System.IO as IO
import Foreign.C.Error (Errno(Errno), ePIPE)
import Control.Exception (throwIO, try)
import Data.Monoid (Monoid (mappend, mempty))
import Data.String (IsString (..))
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime, diffUTCTime, picosecondsToDiffTime)
import Data.Functor.Classes
import Data.Functor.Compose
import Control.Monad.Trans.Resource
import qualified Data.Set as Set

import GHC.Exts ( SpecConstrAnnotation(..) )


data SPEC = SPEC | SPEC2 

{-# ANN type SPEC ForceSpecConstr #-}
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

instance (r ~ (), Monad m, f ~ Of Char) => IsString (Stream f m r) where
  fromString = each

instance (Eq a) => Eq1 (Of a) where eq1 = (==)
instance (Ord a) => Ord1 (Of a) where compare1 = compare
instance (Read a) => Read1 (Of a) where readsPrec1 = readsPrec
instance (Show a) => Show1 (Of a) where showsPrec1 = showsPrec

{-| Note that 'lazily', 'strictly', 'fst'', and 'mapOf' are all so-called /natural transformations/ on the primitive @Of a@ functor
    If we write 
  
>  type f ~~> g = forall x . f x -> g x
  
   then we can restate some types as follows:
  
>  mapOf            :: (a -> b) -> Of a ~~> Of b   -- bifunctor lmap
>  lazily           ::             Of a ~~> (,) a
>  Identity . fst'  ::             Of a ~~> Identity a

   Manipulation of a @Stream f m r@ by mapping often turns on recognizing natural transformations of @f@,
   thus @maps@ is far more general the the @map@ of the present module, which can be
   defined thus:

>  S.map :: (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
>  S.map f = maps (mapOf f)
  
  This rests on recognizing that @mapOf@ is a natural transformation; note though
  that it results in such a transformation as well:
  
>  S.map :: (a -> b) -> Stream (Of a) m ~> Stream (Of b) m   

-}
lazily :: Of a b -> (a,b)
lazily = \(a:>b) -> (a,b)
{-# INLINE lazily #-}

strictly :: (a,b) -> Of a b
strictly = \(a,b) -> a :> b
{-# INLINE strictly #-}

fst' :: Of a b -> a
fst' (a :> b) = a

snd' :: Of a b -> b
snd' (a :> b) = b

mapOf :: (a -> b) -> Of a r -> Of b r
mapOf f (a:> b) = (f a :> b)


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
    Return r -> return b
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
    Return r -> return b
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
break pred = loop where
  loop str = case str of 
    Return r         -> Return (Return r)
    Effect m          -> Effect $ liftM loop m
    Step (a :> rest) -> if (pred a) 
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
breakWhen step begin done pred = loop0 begin
  where
    loop0 x stream = case stream of 
        Return r -> return (return r)
        Effect mn  -> Effect $ liftM (loop0 x) mn
        Step (a :> rest) -> loop a (step x a) rest
    loop a !x stream = do
      if pred (done x) 
        then return (yield a >> stream) 
        else case stream of 
          Return r -> yield a >> return (return r)
          Effect mn  -> Effect $ liftM (loop a x) mn
          Step (a' :> rest) -> do
            yield a
            loop a' (step x a') rest
{-# INLINABLE breakWhen #-}

-- -- Break during periods where the predicate is not satisfied, grouping the periods when it is.
--
-- >>> S.print $ mapsM S.toList $ S.breaks not $ S.each [False,True,True,False,True,True,False]
-- [True,True]
-- [True,True]
-- >>> S.print $ mapsM S.toList $ S.breaks id $ S.each [False,True,True,False,True,True,False]
-- [False]
-- [False]
-- [False]
--
-- -}
-- breaks
--   :: Monad m =>
--      (a -> Bool) -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r
-- breaks thus  = loop  where
--   loop stream = Effect $ do
--     e <- next stream
--     return $ case e of
--       Left   r      -> Return r
--       Right (a, p') ->
--        if not (thus a)
--           then Step $ fmap loop (yield a >> break thus p')
--           else loop p'
-- {-#INLINABLE breaks #-}

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
    Effect mn  -> Effect (liftM loop mn)
    Step (a :> rest) -> Effect $ do
      f a
      return (Step (a :> loop rest))
{-# INLINABLE chain #-}

{-| Make a stream of traversable containers into a stream of their separate elements.
    This is just 

> concat = for str each

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
concat str = for str each
{-# INLINE concat #-}

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

>>> rest <- S.print $ S.splitAt 3 $ S.cycle (yield 0 >> yield 1)
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


{-| A


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

-- ---------------
-- effects
-- ---------------

{- | Reduce a stream, performing its actions but ignoring its elements. 
    
>>> rest <- S.effects $ S.splitAt 2 $ each [1..5]
>>> S.print rest
3
4
5


-}
effects :: Monad m => Stream (Of a) m r -> m r
effects = loop where
  loop stream = case stream of 
    Return r         -> return r
    Effect m          -> m >>= loop 
    Step (_ :> rest) -> loop rest
{-#INLINABLE effects #-}
  
{-| Where a transformer returns a stream, run the effects of the stream, keeping
   the return value. This is usually used at the type

> drained :: Monad m => Stream (Of a) m (Stream (Of b) m r) -> Stream (Of a) m r
> drained = join . fmap (lift . effects)
 
   Here we split a stream twice and throw out the middle segment:
 
>>> rest <- S.print $ S.drained $ S.splitAt 2 $ S.splitAt 5 $ each [1..7]
1
2
>>> S.print rest
6
7

-}
drained :: (Monad m, Monad (t m), Functor (t m), MonadTrans t) => t m (Stream (Of a) m r) -> t m r
drained = join . fmap (lift . effects)
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
drop = loop where
  loop 0 stream = stream
  loop n stream = case stream of
      Return r       -> Return r
      Effect ma      -> Effect (liftM (loop n) ma)
      Step (a :> as) -> loop (n-1) as
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
dropWhile pred = loop where 
  loop stream = case stream of
    Return r       -> Return r
    Effect ma       -> Effect (liftM loop ma)
    Step (a :> as) -> if pred a 
      then loop as
      else Step (a :> as)
{-# INLINABLE dropWhile #-}

-- ---------------
-- each 
-- ---------------

{- | Stream the elements of a pure, foldable container.

>>> each [1..3] & S.print
1
2
3
>>> S.replicateM 5 getLine & chunksOf 3 & mapsM S.toList & S.print
s
t
u
["s","t","u"]
v
w
["v","w"]

-}
each :: (Monad m, Foldable.Foldable f) => f a -> Stream (Of a) m ()
each = Foldable.foldr (\a p -> (Step (a :> p))) (Return ())
{-# INLINABLE each #-}

{-| Exhaust a stream remembering only whether @a@ was an element.

-}

elem :: (Monad m, Eq a) => a -> Stream (Of a) m r -> m (Of Bool r)
elem a = fold op False id where
  op True _ = True
  op False a' | a == a' = True
  op _ _ = False
{-#INLINABLE elem #-}
  
elem_ :: (Monad m, Eq a) => a -> Stream (Of a) m r -> m Bool
elem_ a = fold_ op False id where
  op True _ = True
  op False a' | a == a' = True
  op _ _ = False
{-#INLINABLE elem_ #-}

-- -----
-- enumFrom
-- ------

{-| An infinite stream of enumerable values, starting from a given value.
    It is the same as `S.iterate succ`. 
   Because their return type is polymorphic, @enumFrom@ and @enumFromThen@
   (and @iterate@ are useful for example with @zip@
   and @zipWith@, which require the same return type in the zipped streams. 
   With @each [1..]@ the following bit of connect-and-resume would be impossible:

>>> rest <- S.print $  S.zip (S.enumFrom 'a') $ S.splitAt 3 $ S.enumFrom 1
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
  loop !n = Step (n :> loop (succ n))
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
-- filter 
-- ---------------

-- | Skip elements of a stream that fail a predicate
filter  :: (Monad m) => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filter pred = loop where
  loop str = case str of
    Return r       -> Return r
    Effect m        -> Effect (liftM loop m)
    Step (a :> as) -> if pred a 
                         then Step (a :> loop as)
                         else loop as
{-# INLINABLE filter #-}

-- ---------------
-- filterM
-- ---------------

-- | Skip elements of a stream that fail a monadic test
filterM  :: (Monad m) => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filterM pred = loop where
  loop str = case str of
    Return r       -> Return r
    Effect m        -> Effect $ liftM loop m
    Step (a :> as) -> Effect $ do 
      bool <- pred a
      if bool 
        then return $ Step (a :> loop as)
        else return $ loop as
{-# INLINABLE filterM #-}

-- ---------------
-- fold
-- ---------------

{- $folds
    Use these to fold the elements of a 'Stream'.  

>>> S.fold_ (+) 0 id $ S.each [1..0]
50

    The general folds 'fold', fold_', 'foldM' and 'foldM_' are arranged 
    for use with 'Control.Foldl'

>>> L.purely fold_ L.sum $ each [1..10]
55
>>> L.purely fold_ (liftA3 (,,) L.sum L.product L.list) $ each [1..10]
(55,3628800,[1,2,3,4,5,6,7,8,9,10])

    All functions marked with an underscore omit 
    (e.g. @fold_@, @sum_@) the stream's return value in a left-strict pair.
    They are good for exiting streaming completely, 
    but when you are, e.g. @mapsM@-ing over a @Stream (Stream (Of a) m) m r@, 
    which is to be compared with @[[a]]@. Specializing, we have e.g.

>  mapsM sum :: (Monad m, Num n) => Stream (Stream (Of Int)) IO () -> Stream (Of n) IO ()
>  mapsM (fold mappend mempty id) :: Stream (Stream (Of Int)) IO () -> Stream (Of Int) IO ()

>>> S.print $ mapsM S.sum $ chunksOf 3 $ S.each [1..10]
6
15
24
10

>>> let three_folds = L.purely S.fold (liftA3 (,,) L.sum L.product L.list)
>>> S.print $ mapsM three_folds $ chunksOf 3 (each [1..10])
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
fold_ step begin done = liftM (\(a:>rest) -> a) . fold step begin done
{-#INLINE fold_ #-}

{-| Strict fold of a 'Stream' of elements that preserves the return value. 
    The third parameter will often be 'id' where a fold is written by hand:

>>> S.fold (+) 0 id $ each [1..10]
55 :> ()

>>> S.fold (*) 1 id $ S.fold (+) 0 id $ S.duplicate $ each [1..10]
3628800 :> (55 :> ())

>>> S.fold (*) 1 id $ S.fold_ (+) 0 id $ S.duplicate $ each [1..10]
3628800 :> 55 

    It can be used to replace a standard Haskell type with one more suited to 
    writing a strict accumulation function. It is also crucial to the 
    Applicative instance for @Control.Foldl.Fold@  We can apply such a fold
    @purely@

> Control.Foldl.purely S.fold :: Monad m => Fold a b -> Stream (Of a) m r -> m (Of b r)

    Thus, specializing a bit:

> L.purely S.fold L.sum :: Stream (Of Int) Int r -> m (Of Int r)
> maps (L.purely S.fold L.sum) :: Stream (Stream (Of Int)) IO r -> Stream (Of Int) IO r

>>> S.print $ mapsM (L.purely S.fold (liftA3 (,,) L.list L.product L.sum)) $ chunksOf 3 $ each [1..10]
([1,2,3],6,6)
([4,5,6],120,15)
([7,8,9],504,24)
([10],10,10)

-}

fold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r -> m (Of b r)
fold step begin done str = fold_loop str begin
  where
    fold_loop stream x = case stream of 
      Return r         -> return (done x :> r)
      Effect m         -> m >>= \str' -> fold_loop str' x
      Step (a :> rest) -> fold_loop rest $! step x a
{-# INLINABLE fold #-}


{-| Strict, monadic fold of the elements of a 'Stream (Of a)'

> Control.Foldl.impurely foldM :: Monad m => FoldM a b -> Stream (Of a) m () -> m b
-}
foldM_
    :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream (Of a) m r -> m b
foldM_ step begin done  = liftM (\(a:>rest) -> a) . foldM step begin done
{-#INLINE foldM_ #-}

{-| Strict, monadic fold of the elements of a 'Stream (Of a)'

> Control.Foldl.impurely foldM' :: Monad m => FoldM a b -> Stream (Of a) m r -> m (b, r)
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



{-| A natural right fold for consuming a stream of elements. 
    See also the more general 'iterTM' in the 'Streaming' module 
    and the still more general 'destroy'

> foldrT (\a p -> Pipes.yield a >> p) :: Monad m => Stream (Of a) m r -> Producer a m r
> foldrT (\a p -> Conduit.yield a >> p) :: Monad m => Stream (Of a) m r -> Conduit a m r

-}

foldrT :: (Monad m, MonadTrans t, Monad (t m)) 
       => (a -> t m r -> t m r) -> Stream (Of a) m r -> t m r
foldrT step = loop where
  loop stream = case stream of
    Return r       -> return r
    Effect m        -> lift m >>= loop
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
    Effect m        -> m >>= loop
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
    Effect m          -> Effect $ liftM loop m
    Step (a :> rest) -> do
      act a
      loop rest
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
--   span' pred = loop where
--     loop str = case str of
--       Return r         -> Return (Return r)
--       Effect m          -> Effect $ liftM loop m
--       Step s@(Compose (a :> rest)) -> case pred a  of
--         True  -> Step (Compose (a :> fmap loop rest))
--         False -> Return (Step s)
-- {-# INLINABLE groupedBy #-}

{-| Group elements of a stream in accordance with the supplied comparison. 


>>> S.print $ mapsM S.toList $ S.groupBy (>=) $ each [1,2,3,1,2,3,4,3,2,4,5,6,7,6,5]
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

>>> S.toList $ mapsM S.toList $ S.group $ each "baaaaad"
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
  Return r            -> return Nothing
  Effect m            -> m >>= head_
  Step (a :> rest)    -> return (Just a)
{-#INLINABLE head_ #-}
  
intersperse :: Monad m => a -> Stream (Of a) m r -> Stream (Of a) m r
intersperse x str = case str of
    Return r -> Return r
    Effect m -> Effect (liftM (intersperse x) m)
    Step (a :> rest) -> loop a rest
  where
  loop !a str = case str of
    Return r -> Step (a :> Return r)
    Effect m -> Effect (liftM (loop a) m)
    Step (b :> rest) -> Step (a :> Step (x :> loop b rest))
{-#INLINABLE intersperse #-}
    
    
  

-- ---------------
-- iterate
-- ---------------

{-| Iterate a pure function from a seed value, streaming the results forever
    


-}
iterate :: (a -> a) -> a -> Stream (Of a) m r
iterate f = loop where
  loop a' = Step (a' :> loop (f a'))
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
  loop m str = case str of
    Return r            -> case m of 
      Nothing_ -> return (Nothing :> r)
      Just_ a  -> return (Just a :> r)
    Effect m            -> m >>= last
    Step (a :> rest)  -> loop (Just_ a) rest
{-#INLINABLE last #-}
    


last_ :: Monad m => Stream (Of a) m r -> m (Maybe a) 
last_ = loop Nothing_ where
  loop m str = case str of
    Return r            -> case m of 
      Nothing_ -> return Nothing 
      Just_ a  -> return (Just a)
    Effect m            -> m >>= last_
    Step (a :> rest)  -> loop (Just_ a) rest
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

>>> S.print $ mapsM S.length $ chunksOf 3 $ S.each [1..10]
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
map f = maps (\(x :> rest) -> f x :> rest)
  -- loop where
  -- loop stream = case stream of
  --   Return r -> Return r
  --   Effect m -> Effect (liftM loop m)
  --   Step (a :> as) -> Step (f a :> loop as)
{-# INLINABLE map #-}


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
    Effect m        -> Effect (liftM loop m)
    Step (a :> as) -> Effect $ do 
      a' <- f a 
      return (Step (a' :> loop as) )
{-# INLINABLE mapM #-}



{-| Reduce a stream to its return value with a monadic action.

>>> S.mapM_ Prelude.print $ each [1..5]
1
2
3
4
5
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
    Return r       -> return r 
    Effect m        -> m >>= loop
    Step (a :> as) -> do 
      f a 
      loop as 
{-# INLINABLE mapM_ #-}

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

> IOStreams.unfoldM (liftM (either (const Nothing) Just) . next) :: Stream (Of a) IO b -> IO (InputStream a)
> Conduit.unfoldM (liftM (either (const Nothing) Just) . next)   :: Stream (Of a) m r -> Source a m r

     But see 'uncons', which is better fitted to these @unfoldM@s
-}
next :: Monad m => Stream (Of a) m r -> m (Either r (a, Stream (Of a) m r))
next = loop where
  loop stream = case stream of
    Return r         -> return (Left r)
    Effect m          -> m >>= loop
    Step (a :> rest) -> return (Right (a,rest))
{-# INLINABLE next #-}


nub :: (Monad m, Ord a) => Stream (Of a) m r -> Stream (Of a) m r
nub = loop Set.empty where
  loop !set stream = case stream of 
    Return r         -> Return r
    Effect m         -> Effect (liftM (loop set) m)
    Step (a :> rest) -> if Set.member a set 
      then loop set rest
      else Step (a :> loop (Set.insert a set) rest)

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

repeat :: a -> Stream (Of a) m r
repeat a = loop where loop = Step (a :> loop)
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

-- | Repeat an element several times
replicate :: Monad m => Int -> a -> Stream (Of a) m ()
replicate n a = loop n where
  loop 0 = Return ()
  loop m = Step (a :> loop (m-1))
{-# INLINABLE replicate #-}

{-| Repeat an action several times, streaming the results.

>>> S.print $ S.replicateM 2 getCurrentTime
2015-08-18 00:57:36.124508 UTC
2015-08-18 00:57:36.124785 UTC

-}
replicateM :: Monad m => Int -> m a -> Stream (Of a) m ()
replicateM n ma = loop n where 
  loop 0 = Return ()
  loop n = Effect $ do 
    a <- ma 
    return (Step $ a :> loop (n-1))
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

{-| Strict left scan, streaming, e.g. successive partial results.


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
scan step begin done = loop begin
  where
    loop !x stream = case stream of 
        Return r -> Return r
        Effect m  -> Effect $ liftM (loop x) m
        Step (a :> rest) -> Step (
          done x :>  do
            let !x' = step x a
            loop x' rest)
{-# INLINABLE scan #-}

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
scanM step begin done str = do
    x <- lift begin
    loop x str
  where
    loop !x stream = do 
      b <- lift (done x)
      yield b
      case stream of 
        Return r -> Return r
        Effect m  -> Effect (do 
          stream' <- m
          return (loop x stream')
          )
        Step (a :> rest) -> Effect (do
          x' <- step x a
          return (loop x' rest)
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
        Effect mn  -> Effect $ liftM (loop m x) mn
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
a
b
c
[(0.0,"a"),(1.088711,"b"),(3.7289649999999996,"c")] :> ()
  
   To restrict user input to some number of seconds, we might write:
  
>>> S.toList $ S.zipWith (flip const) (S.takeWhile (< 5) S.seconds) S.stdinLn
one
two
three
four
five
["one","two","three","four","five"] :> ()

  -}
  
seconds :: Stream (Of Double) IO r
seconds = do 
    e <- lift $ next preseconds
    case e of
      Left r -> return r
      Right (t, rest) -> do
        yield 0
        map (subtract t) rest
 where      
  preseconds :: Stream (Of Double) IO r
  preseconds = do
    utc <- liftIO getCurrentTime
    map ((/1000000000) . nice utc) (repeatM getCurrentTime)
   where
     nice u u' = fromIntegral $ truncate (1000000000 * diffUTCTime u' u)

-- ---------------
-- sequence
-- ---------------

{-| Like the 'Data.List.sequence' but streaming. The result type is a
    stream of a\'s, /but is not accumulated/; the effects of the elements
    of the original stream are interleaved in the resulting stream. Compare:

> sequence :: Monad m =>       [m a]           -> m [a]
> sequence :: Monad m => Stream (Of (m a)) m r -> Stream (Of a) m r
-}
sequence :: Monad m => Stream (Of (m a)) m r -> Stream (Of a) m r
sequence = loop where
  loop stream = case stream of
    Return r          -> Return r
    Effect m           -> Effect $ liftM loop m
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

>  maps' sum' :: Stream (Stream (Of Int)) m r -> Stream (Of Int) m r


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
span pred = loop where
  loop str = case str of 
    Return r         -> Return (Return r)
    Effect m          -> Effect $ liftM loop m
    Step (a :> rest) -> if pred a 
      then Step (a :> loop rest)
      else Return (Step (a :> rest))
{-# INLINABLE span #-}

                            
{-| Split a stream of elements wherever a given element arises.
    The action is like that of 'Prelude.words'. 

>>> S.stdoutLn $ mapsM S.toList $ S.split ' ' $ each "hello world  "
hello
world

-}

split :: (Eq a, Monad m) =>
      a -> Stream (Of a) m r -> Stream (Stream (Of a) m) m r
split t  = loop  where
  loop stream = case stream of 
    Return r -> Return r
    Effect m -> Effect (liftM loop m)
    Step (a :> rest) -> 
         if a /= t
            then Step (fmap loop (yield a >> break (== t) rest))
            else loop rest
{-#INLINABLE split #-}

{-| Split a succession of layers after some number, returning a streaming or
--   effectful pair. This function is the same as the 'splitsAt' exported by the
--   @Streaming@ module, but since this module is imported qualified, it can 
--   usurp a Prelude name. It specializes to:

>  splitAt :: (Monad m, Functor f) => Int -> Stream (Of a) m r -> Stream (Of a) m (Stream (Of a) m r)

-}
splitAt :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
splitAt = splitsAt
{-# INLINE splitAt #-}


-- ---------------
-- take
-- ---------------

{-| End a stream after n elements; the original return value is thus lost.
    'splitAt' preserves this information. Note that, like @splitAt@, this
    function is functor-general, so that, for example, you can @take@ not
    just a number of items from a stream of elements, but a number 
    of substreams and the like.

>>> S.toList $ S.take 3 $ each "pennsylvania"
"pen" :> ()

>>> total <- S.sum_ $ S.take 3 S.readLn :: IO Int
1<Enter>
10<Enter>
100<Enter>
>>> print total
111

-}

take :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
take = loop where
  loop 0 p = return ()
  loop n p = 
    case p of Step fas -> Step (fmap (loop (n-1)) fas)
              Effect m -> Effect (liftM (loop n) m)
              Return r -> Return ()
{-# INLINABLE take #-}

-- ---------------
-- takeWhile
-- ---------------

-- | End stream when an element fails a condition; the original return value is lost
-- 'span' preserves this information.
takeWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhile pred = loop where
  loop str = case str of 
    Step (a :> as) -> when (pred a) (Step (a :> loop as))
    Effect m              -> Effect (liftM loop m)
    Return r              -> Return ()
{-# INLINE takeWhile #-}


{-| Convert an effectful 'Stream (Of a)' into a list of @as@

    Note: Needless to say this function does not stream properly.
    It is basically the same as 'mapM' which, like 'replicateM',
    'sequence' and similar operations on traversable containers
    is a leading cause of space leaks.
    
-}
toList_ :: Monad m => Stream (Of a) m () -> m [a]
toList_ = fold_ (\diff a ls -> diff (a: ls)) id (\diff -> diff [])
{-# INLINE toList_ #-}


{-| Convert an effectful 'Stream' into a list alongside the return value

>  mapsM toListM :: Stream (Stream (Of a)) m r -> Stream (Of [a]) m 
-}
toList :: Monad m => Stream (Of a) m r -> m (Of [a] r)
toList = fold (\diff a ls -> diff (a: ls)) id (\diff -> diff [])
{-# INLINE toList #-}


{-| Inspect the first item in a stream of elements, without a return value. 
    @uncons@ provides convenient exit into another streaming type:

> IOStreams.unfoldM uncons :: Stream (Of a) IO b -> IO (InputStream a)
> Conduit.unfoldM uncons   :: Stream (Of a) m r -> Conduit.Source m a

-}
uncons :: Monad m => Stream (Of a) m () -> m (Maybe (a, Stream (Of a) m ()))
uncons = loop where
  loop stream = case stream of
    Return ()        -> return Nothing
    Effect m          -> m >>= loop
    Step (a :> rest) -> return (Just (a,rest))
{-# INLINABLE uncons #-}


{-| Build a @Stream@ by unfolding steps starting from a seed. 

    The seed can of course be anything, but this is one natural way 
    to consume a @pipes@ 'Pipes.Producer'. Consider:

>>> S.stdoutLn $ S.take 2 (S.unfoldr P.next P.stdinLn)
hello<Enter>
hello
goodbye<Enter>
goodbye

>>> S.stdoutLn $ S.unfoldr P.next (P.stdinLn P.>-> P.take 2)
hello<Enter>
hello
goodbye<Enter>
goodbye

>>> S.effects $ S.unfoldr P.next (P.stdinLn P.>-> P.take 2 P.>-> P.stdoutLn)
hello<Enter>
hello
goodbye<Enter>
goodbye

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
-- yield
-- ---------------------------------------

{-| A singleton stream

>>> stdoutLn $ yield "hello"
hello

>>> S.sum $ do {yield 1; yield 2}
3
    
>>> let prompt = putStrLn "Enter a number:" 
>>> let number = lift (prompt >> readLn) >>= yield :: Stream (Of Int) IO ()
>>> S.toList $ do {number; number; number}
Enter a number:
1
Enter a number:
2
Enter a number:
3
[1,2,3] :> ()

-}

yield :: Monad m => a -> Stream (Of a) m ()
yield a = Step (a :> Return ())
{-# INLINE yield #-}

-- | Zip two 'Streams's 
zip :: Monad m
    => (Stream (Of a) m r)
    -> (Stream (Of b) m r)
    -> (Stream (Of (a,b)) m r)
zip = zipWith (,)
{-# INLINE zip #-}

-- | Zip two 'Streams's using the provided combining function
zipWith :: Monad m
    => (a -> b -> c)
    -> (Stream (Of a) m r)
    -> (Stream (Of b) m r)
    -> (Stream (Of c) m r)
zipWith f = loop
  where
    loop str0 str1 = case str0 of
      Return r          -> Return r
      Effect m           -> Effect $ liftM (\str -> loop str str1) m 
      Step (a :> rest0) -> case str1 of
        Return r          -> Return r
        Effect m           -> Effect $ liftM (loop str0) m
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
            
            
-- | Zip three streams together 
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

{-| repeatedly stream lines as 'String' from stdin

>>> stdoutLn $ S.show (S.each [1..3])
1
2
3

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

{-| Read values from 'IO.stdin', ignoring failed parses

>>> S.sum_ $ S.take 2 S.readLn :: IO Int
10<Enter>
12<Enter>
22

>>> S.toList $ S.take 3 (S.readLn :: Stream (Of Int) IO ())
1<Enter>
2<Enter>
1@#$%^&*\<Enter>
3<Enter>
[1,2,3] :> ()


-}

readLn :: (MonadIO m, Read a) => Stream (Of a) m ()
readLn = for stdinLn $ \str -> case readMaybe str of 
  Nothing -> return ()
  Just n  -> yield n
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

>>> S.toHandle IO.stdout $ each $ words "one two three"
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
hello
"hello"
world
"world"
>>> 

-}
print :: (MonadIO m, Show a) => Stream (Of a) m r -> m r
print = loop where
  loop stream = case stream of 
    Return r         -> return r 
    Effect m          -> m >>= loop
    Step (a :> rest) -> do 
      liftIO (Prelude.print a)
      loop rest


{-| Write 'String's to 'IO.stdout' using 'putStrLn'; terminates on a broken output pipe
    (This operation is modelled on 'Pipes.Prelude.stdoutLn').

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




{-| Write 'String's to 'IO.stdout' using 'putStrLn'

    This does not handle a broken output pipe, but has a polymorphic return
    value, which makes this possible:

>>> rest <- S.stdoutLn' $ S.show $ S.splitAt 3 (each [1..5])
1
2
3
>>> S.print rest
4
5

-}

stdoutLn' :: MonadIO m => Stream (Of String) m r -> m r
stdoutLn' = loop where 
  loop stream = case stream of 
    Return r         -> return r 
    Effect m          -> m >>= loop
    Step (s :> rest) -> liftIO (putStrLn s) >> loop rest
{-# INLINE stdoutLn' #-}

{-| Read a series of strings as lines to a file. The handle is crudely 
    managed with 'ResourceT':

>>> runResourceT $ S.writeFile "lines.txt" $ S.take 2 S.stdinLn
hello<Enter>
world<Enter>
>>> runResourceT $ S.print $ S.readFile "lines.txt" 
"hello"
"world"

-}

readFile :: MonadResource m => FilePath -> Stream (Of String) m ()
readFile f = bracketStream (IO.openFile f IO.ReadMode) (IO.hClose) fromHandle

{-| Write a series of strings as lines to a file. The handle is crudely 
    managed with 'ResourceT':

>>> runResourceT $ S.writeFile "lines.txt" $ S.take 2 S.stdinLn
hello<Enter>
world<Enter>
>>> runResourceT $ S.print $ S.readFile "lines.txt" 
"hello"
"world"

-}
writeFile :: MonadResource m => FilePath -> Stream (Of String) m r -> m r
writeFile f str = do 
  (key, handle) <- allocate (IO.openFile f IO.WriteMode) (IO.hClose) 
  r <- toHandle handle str
  release key
  return r

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

duplicate
  :: Monad m =>
     Stream (Of a) m r -> Stream (Of a) (Stream (Of a) m) r
duplicate = loop where
  loop str = case str of
    Return r         -> Return r
    Effect m         -> Effect (liftM loop (lift m))
    Step (a :> rest) -> Step (a :> Effect (Step (a :> Return (loop rest))))
{-#INLINABLE duplicate#-}

{-| The type

> Data.List.unzip     :: [(a,b)] -> ([a],[b])

   might lead us to expect 

> Streaming.unzip :: Stream (Of (a,b)) m r -> Stream (Of a) m (Stream (Of b) m r)
 
   which would not stream. Of course, neither does 'Data.List.unzip'

-}
unzip :: Monad m =>  Stream (Of (a,b)) m r -> Stream (Of a) (Stream (Of b) m) r
unzip = loop where
 loop str = case str of 
   Return r -> Return r
   Effect m -> Effect (liftM loop (lift m))
   Step ((a,b):> rest) -> Step (a :> Effect (Step (b :> Return (loop rest))))
{-#INLINABLE unzip #-}


-- "fold/map" forall step begin done f str .
-- fold step begin done (map f str) = fold (\x a -> step x $! f a) begin done str;
--
-- "fold/filter" forall step begin done pred str .
-- fold step begin done (filter pred str) = fold (\x a -> if pred a then step x a else x) begin done str;
--
-- "scan/map" forall step begin done f str .
-- scan step begin done (map f str) = scan (\x a -> step x $! f a) begin done str
--

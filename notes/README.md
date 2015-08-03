series
======

The `Series` type defined here is an attempt to represent
effectful sequences in the style of `Pipes.Producer`, "`ListT`
done right", `FreeT ((,) a)` and the like.

Some benchmarks on more and less complex compositions of
functions can be seen here:

![ ](http://michaelt.github.io/images/bench.png)

The rest of the report is
[here](http://michaelt.github.io/bench/seriesbench.html). Lines
marked 'f.g.h/fused' bench compositions of functions defined
through the fusion framework described below; those marked
'f.g.h/naive' bench compositions of functions given ordinary
recursive definitions using the constructors of the Series
datatype. The corresponding programs written with `Data.List` and
`Data.Vector.Unboxed` are marked 'list' and 'vector'

The benchmarks are pure and thus use `Series (Of a) Identity ()`,
which is isomorphic to Haskell lists. It is interesting that the
present fusion framework is *always* faster than Data.List. It is
also more reliable than both vector and Data.List (though vector
is of course much faster where fusion succeeds.) But these cases
are perhaps somewhat stylized. I am also surprised so far that
newtype wrapping makes the fusion rules more reliable.

-----------------------------------------------------------------

The standard `FreeT` module is irremediably slow and lacks
crucial combinators. In particular it does not develop the
important case in which the functor -- e.g `(a, _)`, here
`Of a _` -- generates a list-like structure. Though the
`Series f m a` type here aspires to be an optimized
`FreeT f m a` -- and thus can take any functor f -- the aim is to
represent *effectful sequences* of various sorts, such as the
`Pipes.Producer` type ( \~ `FreeT ((,) a) m r` \~
`FreeT (Of a) m r` \~ `Series (Of a) m r`)

In some respects we follow the model of `ertes`'s experimental
[`fuse` package](http://hub.darcs.net/ertes/fuse), which may hold
more interest; in particular the device of calling the strict
pair `Of a b` is found there; his `FreeT` type is called `List`.
We are also indirectly following some remarks of Atkey mentioned
below.

The first optimization is in the datatype `Series`:

    data Series f m r = Construct (f (Series f m r))
                      | Wrap (m (Series f m r))
                      | Done r

It requires a suitable quotient to be seen as isomorphic to
`FreeT`. This will lead to some well-understood correctness
subtleties; they must be handled by resisting direct use of the
constructors etc., but the procedure is familiar.

The next is to develop an optimization infrastructure in terms of
a corresponding Church encoded type. Ideally this will follow the
model of `Data.List`. At the moment, it is using two
easier-to-implement variants in which a church encoded version of
`Series`, i.e.:

    type Fold_ f m a = 
       forall r. (f r -> r) -> (m r -> r) -> (a -> r) -> r

or rather

    newtype Fold f m r = Fold {getFold :: Fold_ f m r}

is used. `ertes` and the Church-encoded module of the `free`
package use an inexplicably more complex type; in `free` it is

    newtype FT f m a = FT {
         runFT :: forall r. (a -> m r) -> (f (m r) -> m r) -> m r
         }

As reflection on Atkey's discussion will I think show, this
constrains possibilities of definition unnecessarily. Thus we
would like to define
`take :: Int -> Fold (Of a) m r -> Fold (Of a) m r` by
instantiating the rank-2 fold at `Int -> Fold_ (Of a) m r`:

    take :: (Monad m, Functor f) => Int -> Series f m r -> Series f m ()
    take n =  buildSeries . (\(Fold phi) -> Fold (pretake n phi)) . foldSeries 
      where
        pretake :: (Monad m, Functor f) => Int -> Fold_ f m r -> Fold_ f m ()
        pretake n phi = \construct wrap done -> phi 
              (\fx n -> if n <= 0 then done () else construct (fmap ($(n-1)) fx))
              (\mx n -> if n <= 0 then done () else wrap (liftM ($n) mx)) 
              (\r n -> done ()) 
              n
        {-# INLINE pretake #-}
    {-# INLINE take #-}

At the moment, then, all functions are thus basically of one of
the forms

        build . wrapped-churched-definition . fold
        wrapped-churched-definition . fold
        build . wrapped-churched-definition
        

so that we can eliminate `fold . build` upon composition, in the
style of the `stream . unstream  = id` rule in `vector`. This
seems to account for the success of the style in getting the
compiler to recognize fusion opportunities, attested by the
benchmarks so far and by counting appearances with
`-ddump-rule-firings`

Of the two principal fusion operations,

    buildSeries :: Fold f m r -> Series f m r 
    buildSeries = \(Fold phi) -> phi Construct Wrap Done

and

    foldSeries ::  (Functor f, Monad m) => Series f m r -> Fold f m r

the latter is a flipped and wrapped variant of Atkey's

    effectfulFold :: (Functor f, Monad m) =>
       (m x -> x) -> (r -> x) -> (f x -> x) -> Series f m r -> x

(modulo the 'Done' constructor, which implicitly restricts the
available class of Functors.) See
http://bentnib.org/posts/2012-01-06-streams.html and the
(gruesomely technical) associated paper. The examples of
implementing functions by way of `effectfulFold` are extremely
surprising and illuminating and are emulated here, where I can
figure it out. The definition of `pretake` above is an example.
Our effort is to define every function on a `Series/FreeT` as
such a fold.

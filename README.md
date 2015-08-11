streaming
=========

This library defines an optimized `FreeT` with an eye to use with 
streaming libraries, namely:

    data Stream f m r
         = Return r
         | Step !(f (Stream f m r))
         | Delay (m (Stream f m r))

in place of the standard version in the `free` library, which 
is approximately: 

    newtype FreeT f m r = FreeT {runFreeT :: m (Either r (f (FreeT f m r)))}

Such an optimization is adopted internally by the `pipes` library. 

The maneuver is very friendly to the compiler, but requires a bit of 
subtlety to protect a sound monad instance: as in `pipes`, 
the constructors are here left in an `Internal` module; 
the main `Streaming` module exporting the type itself and various 
operations and instances. 

There is also a still-incomplete `Prelude` of functions, some 
`FreeT` or `Stream` - general, some involving the functor 
`((,) a)` here called `Of a`.  `FreeT ((,) a) m r` or `Stream (Of a) m r`
is equivalent to the `pipes` `Producer a m r` type, as
`FreeT ((,) a) m ()` or `Stream (Of a) m ()` are equivalent
to the various implementations of `ListT done right`. 

-------

The prelude of functions included here experimentally uses a simple 
optimization scheme which is modeled on that used in `Data.List`: it
goes by way of the church encoding of the `Stream` type above, 
generally newtyped thus:

    newtype Folding f m a = Folding {getFolding::
         forall r. (f r -> r) -> (m r -> r) -> a -> r}

The latter wraps and generalizes the unwrapped type GHC uses to 
optimize `Data.List`

    forall r . (a -> r -> r) -> r -> r

which is equivalent to 

    Folding ((,) a) Identity ()

-- just as 

   [a]
   
is equivalent to 

    Stream (Of a) Identity ()
    
or 

    FreeT ((,) a) Identity ()
    
The `Data.List` scheme doesn't have the awesome good cases 
that e.g. stream-fusion optimization has, but it is noteworthy
for not having bad cases like `concatMap`; it does no harm. 
The hope is to employ this type for a fairly straightforward 
optimization of a number of types of the `ListT` and `Producer` 
sort, using the almost-correct equivalences

     Producer a m r ~ Folding ((,) a) m r
     FreeT f m r  ~ Folding f m r
     Stream f m r ~ Folding f m r
     
and a number of potential others, e.g. `LogicT`, `Conduit.Source`, etc 
which are equivalent to `Folding ((,) a) m ()`. The `Stream` type 
defined here is an attempt at an optimized `FreeT` aimed
at improving the pipes usage `FreeT (Producer a m) m r` and
the like. Some experimentation along these lines is included in
the ancillary modules in the `benchmarks` directory here. 

In each of the `Prelude`s included there, operations with types like

     f_producer :: Producer a m r -> Producer b m z
     f_freet :: FreeT f m r -> FreeT g m s
     f_series :: Stream (Of a) m r -> Stream (Of b) m r
     f_list :: [a] -> [a]

are implemented as

     buildProducer . f_folding . foldProducer
     buildFreeT . f_folding . foldFreeT
     buildStream . f_folding . foldStream
     buildList . f_folding . foldList

where `f_folding` is the appropriate function of `Folding`s. The different
`Prelude` s thus differ mostly by find-and-replace. Functions that enter or
exit space of 'serial' types use only one of the fusion operators. 

In each case the principal (only) "fusion" rule is of the form

     buildProducer (foldProducer phi) = phi
     buildFreeT (foldFreeT phi) = phi
     buildStream (foldStream phi) = phi
     buildList (foldList phi) = phi  
     
It may be that the resulting implementations are better at making 
it past the impediments of `criterion`, but some benchmarks on 
more and less complex compositions of functions `f.g.h`, with and without
defintions via `Folding`, can be seen here:

![ ](http://michaelt.github.io/images/seriesbench.png)

The rest of the report is
[here](http://michaelt.github.io/bench/seriesbench.html). Lines
marked 'f.g.h/FOLDING' bench compositions of functions defined
through the fusion framework described above; those marked
'f.g.h' bench compositions of functions given ordinary
definitions using the constructors or as they are exported by 
suitable libraries. Functions from `Data.Vector.Unboxed` are 
marked 'vector' and are included for comparison.

The benchmarks are pure and thus use `Folding (Of a) Identity ()`, 
`Stream (Of a) Identity ()` and `[a]`. It is interesting that for these benchmarks, the
present fusion framework is *always* faster than Data.List. It is
also more reliable than both vector and Data.List (though vector
is of course much faster where fusion succeeds.) But these cases
are perhaps somewhat stylized, and in my experience `criterion` is a bit 
cruel to anything that requires specialization and other optimization. 
I am surprised though that so far the newtype =
]wrapping makes the fusion 
rules more reliable.  

One objective is to optimize pipes functions with types like

     lines :: Monad m 
           => Producer ByteString m r 
           -> FreeT (Producer ByteString m) m r

which introduce a certain perceptible clunkiness. 

stream
======

This library defines two types, a simple FreeT, for use with
streaming libraries

    data Stream f m r
         = Return r
         | Step !(f (Stream f m r))
         | Delay (m (Stream f m r))

and newtypes its church encoding

    newtype Folding f m a = Folding {getFolding::
         forall r. (f r -> r) -> (m r -> r) -> a -> r}

for use with a simple foolproof optimization scheme. The latter wraps 
and generalizes the unwrapped type GHC uses to  optimize `Data.List`

    forall r . (a -> r -> r) -> r -> r

which is equivalent to 

    Folding ((,) a) Identity ()

This library defines a `Prelude` of functions on `Folding` especially
`Folding (Of a) m r`.  The hope is to employ this type for a fairly
straightforward optimization of a number of types of the `ListT` 
and `Producer` sort, using the almost-correct equivalences

     Producer a m r ~ Folding ((,) a) m r
     FreeT f m r  ~ Folding f m r
     Stream f m r ~ Folding f m r
     
and a number of potential others, e.g. `LogicT`, `Conduit.Source`, etc 
which are equivalent to `Folding ((,) a) m ()`. The `Stream` type 
defined here is an attempt at an optimized `FreeT` aimed
at improving the pipes usage `FreeT (Producer a m) m r` and
the like. (Some decisions have been made homogeneous with 
`ertes`'s similarly motivated [`fuse` package](http://hub.darcs.net/ertes/fuse), 
which calls it `FreeT` replacement `List`; it may be more interesting.)

In each of the `Prelude`s included here, operations with types like

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

The real objective is to optimize pipes functions with types like

     lines :: Monad m 
           => Producer ByteString m r 
           -> FreeT (Producer ByteString m) m r

which introduce a certain perceptible clunkiness. 

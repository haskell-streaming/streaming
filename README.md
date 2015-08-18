streaming
=========

This library defines an optimized `FreeT` with an eye to use with 
streaming libraries, namely:

    data Stream f m r
         = Return r
         | Step !(f (Stream f m r))
         | Delay (m (Stream f m r))

in place of the standard `FreeT` that we find in the `free` library, which 
is approximately: 

    newtype FreeT f m r = FreeT {runFreeT :: m (Either r (f (FreeT f m r)))}

Rather than wrapping each step in a monadic 'layer', such a layer is 
put alongside separate 'pure' constructors for a functor 'layer'
and a final return value.  The maneuver is very friendly to the compiler, 
but requires a bit of subtlety to protect a sound monad instance.  Just such an optimization is adopted internally by the `pipes` library.
As in `pipes`, the constructors are here left in an `Internal` module; 
the main `Streaming` module exporting the type itself and various 
operations and instances. 

There is also a still-incomplete `Prelude` of functions, some 
`FreeT` or `Stream` - general, some involving the functor 
`((,) a)` here called `Of a`. (`Stream (Of a) m r` like
`FreeT ((,) a) m r` is equivalent to the `pipes` 
`Producer a m r` type. Similarly, `Stream (Of a) m ()` and 
`FreeT ((,) a) m ()` are possible implementations 
of `ListT done right`. 

I ran a simple [benchmark](https://gist.github.com/michaelt/7f89dc8b366b30bb6acc) (adjusting a [script](https://github.com/jwiegley/streaming-tests) of John Weigly) using a very simple composition of functions:

    toList 
    . filter (\x -> x `mod` 2 == 0) 
    . map (+1) 
    . drop 1000 
    . map (+1) 
    . filter even 
    . each

The the results were fairly pleasing:

    benchmarking basic/streaming
    time                 84.50 ms   (79.81 ms .. 87.90 ms)

    benchmarking basic/iostreams
    time                 266.2 ms   (235.6 ms .. 292.0 ms)

    benchmarking basic/pipes
    time                 232.0 ms   (206.6 ms .. 246.7 ms)

    benchmarking basic/conduit
    time                 102.3 ms   (96.24 ms .. 110.0 ms)

This sequence of pre-packaged combinators is, I think, very friendly to the 
more recent conduit fusion framework. The framework of course doesn't apply to
user-defined operations, where we should expect times like those shown for pipes.
Since the combinators from `streaming` is defined with naive recursion, 
more or less as the user might, we have reason to think the result is characteristic, 
but much more benchmarking is needed before anything can be said with certainty.

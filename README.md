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

I ran a simple benchmark (adjusting a script of `johnw`) using a very simple 
composition of functions:

    toList 
    . filter (\x -> x `mod` 2 == 0) 
    . map (+1) 
    . drop 1000 
    . map (+1) 
    . filter even 
    . each

This all-prepackaged-combinator sequence is, I think, very friendly to the 
more recent conduit fusion framework. Since we are employing a very simple
direct implementation of the sort the user might, the results were fairly 
pleasing.

    benchmarking basic/stream
    time                 77.89 ms   (71.87 ms .. 83.38 ms)
                         0.990 R²   (0.978 R² .. 0.998 R²)
    mean                 80.32 ms   (76.59 ms .. 87.02 ms)
    std dev              8.186 ms   (3.234 ms .. 13.56 ms)
    variance introduced by outliers: 29% (moderately inflated)

    benchmarking basic/iostreams
    time                 267.1 ms   (232.0 ms .. 294.4 ms)
                         0.995 R²   (0.988 R² .. 1.000 R²)
    mean                 267.8 ms   (259.1 ms .. 277.1 ms)
    std dev              9.579 ms   (5.181 ms .. 13.46 ms)
    variance introduced by outliers: 16% (moderately inflated)

    benchmarking basic/pipes
    time                 228.4 ms   (187.7 ms .. 268.0 ms)
                         0.971 R²   (0.893 R² .. 0.999 R²)
    mean                 241.0 ms   (220.1 ms .. 263.4 ms)
    std dev              27.95 ms   (14.63 ms .. 39.26 ms)
    variance introduced by outliers: 31% (moderately inflated)

    benchmarking basic/conduit
    time                 101.9 ms   (90.46 ms .. 109.7 ms)
                         0.988 R²   (0.975 R² .. 0.997 R²)
    mean                 97.79 ms   (94.04 ms .. 102.4 ms)
    std dev              6.551 ms   (4.501 ms .. 10.04 ms)
    variance introduced by outliers: 20% (moderately inflated)


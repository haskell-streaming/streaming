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
    time                 84.50 ms   (79.81 ms .. 87.90 ms)
                         0.995 R²   (0.987 R² .. 1.000 R²)
    mean                 84.33 ms   (81.78 ms .. 89.29 ms)
    std dev              5.550 ms   (2.426 ms .. 8.829 ms)
    variance introduced by outliers: 19% (moderately inflated)

    benchmarking basic/iostreams
    time                 266.2 ms   (235.6 ms .. 292.0 ms)
                         0.996 R²   (0.990 R² .. 1.000 R²)
    mean                 265.6 ms   (258.0 ms .. 271.1 ms)
    std dev              7.510 ms   (2.602 ms .. 10.28 ms)
    variance introduced by outliers: 16% (moderately inflated)

    benchmarking basic/pipes
    time                 232.0 ms   (206.6 ms .. 246.7 ms)
                         0.993 R²   (0.974 R² .. 1.000 R²)
    mean                 242.8 ms   (233.5 ms .. 259.9 ms)
    std dev              16.41 ms   (3.249 ms .. 22.56 ms)
    variance introduced by outliers: 15% (moderately inflated)

    benchmarking basic/conduit
    time                 102.3 ms   (96.24 ms .. 110.0 ms)
                         0.991 R²   (0.978 R² .. 0.999 R²)
    mean                 99.48 ms   (95.40 ms .. 102.8 ms)
    std dev              5.632 ms   (3.876 ms .. 8.163 ms)
    variance introduced by outliers: 10% (moderately inflated)


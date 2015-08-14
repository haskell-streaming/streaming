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

Just such an optimization is adopted internally by the `pipes` library. 
The maneuver is very friendly to the compiler, but requires a bit of 
subtlety to protect a sound monad instance. As in `pipes`, 
the constructors are here left in an `Internal` module; 
the main `Streaming` module exporting the type itself and various 
operations and instances. 

There is also a still-incomplete `Prelude` of functions, some 
`FreeT` or `Stream` - general, some involving the functor 
`((,) a)` here called `Of a`. (`Stream (Of a) m r` like
`FreeT ((,) a) m r` is equivalent to the `pipes` 
`Producer a m r` type. Similarly, `Stream (Of a) m ()` and 
`FreeT ((,) a) m ()` are possible implementations 
of `ListT done right`. 

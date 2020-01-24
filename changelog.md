- 0.2.4.0

    Bifoldable and Bitraversable instances for Of.

- 0.2.3.0
    Add `wrapEffect`.

    Compatibility with base 4.13.
    
    Provide a MonadFail instance for Stream.

    Only depend on `semigroups` on old GHCs.

    Add `untilLeft` (counterpart to `untilRight`)

    Add doctests.

    Enable -Wall in cabal file.

    Build with ghc >= 7.10.3.

- 0.2.2.0

    Add `nubOrd`, `nubInt`, `nubOrdOn`, `nubIntOn`.

    Fix performance regression in `for`.
    
    Add `foldMap` and `foldMap_`.
    
    Fix the behaviour of `slidingWindow 1`.
    
    Reintroduce `readFile` and `writeFile`, using plain `System.IO`
    instead of `ResourceT` machinery.

    Add `merge`, `mergeOn`, and `mergeBy`.

    Improve performance of `concat`.

    Improve performance of (`*>`), getting rid of the default implementation.

    Generalise type signature of `toList_`.

- 0.2.1.0

    Adding `Semigroup` instances for GHC 8.4.

- 0.2.0.0

    Remove `bracketStream`, `MonadCatch` instance, and everything
    dealing with `ResourceT`. All of these things of sort of
    broken for `Stream` since there is no guarantee of linear
    consumption (functions like `take` can prevent finalizers
    from running). The `streaming-with` library is recommended
    to get this kind of behavior.

    Add `Semigroup` instances for `Of` and `Stream`.

    Drop unneeded dependency on exceptions.

- 0.2.0.0

    Made `zipsWith` and allied functions short-circuit; if the
    first stream is empty, ignore the second one.

    Deprecated `mapsExposed` and `mapsMExposed`. These were perfectly
    safe copies of `maps` and `mapsM` with scary names.

    Made the `Show` and `Eq` instances for `Stream` respect the
    abstraction. In effect, the streams are `unexposed` before
    being shown or tested for equality.

    Added `Eq1`, `Ord`, `Ord1`, and `Show1` instances for `Stream`.

    Added `Generic`, `Generic1`, `Eq1`, `Ord1`, `Show1`, `Eq2`, `Ord2`,
    and `Show2` instances for `Of`.

    Bump the lower bound on `transformers` to 0.5.

    Break compatibility with pre-AMP base. Prefer `fmap` to `liftM`.

- 0.1.3.0 

    Added `duplicate` and `store` for simultaneous folding.
    
    Added `mapped` for the ugly `mapsM`
    
    `mwrap` renamed `effect`

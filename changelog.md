- 0.2.0.0

    Remove `bracketStream`, `MonadCatch` instance, and everything
    dealing with `ResourceT`. All of these things of sort of
    broken for `Stream` since there is no guarantee of linear
    consumption (functions like `take` can prevent finalizers
    from running). The `streaming-with` library is not recommended
    to get this kind of behavior.

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

- 0.1.3.0 

    Added `duplicate` and `store` for simultaneous folding.
    
    Added `mapped` for the ugly `mapsM`
    
    `mwrap` renamed `effect`

- ???

    Made `zipsWith` and allied functions short-circuit; if the
    first stream is empty, ignore the second one.

    Deprecated `mapsExposed` and `mapsMExposed`. These were perfectly
    safe copies of `maps` and `mapsM` with scary names.

- 0.1.3.0 

    Added `duplicate` and `store` for simultaneous folding.
    
    Added `mapped` for the ugly `mapsM`
    
    `mwrap` renamed `effect`

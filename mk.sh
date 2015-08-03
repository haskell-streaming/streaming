cabal configure  --package-db=../.cabal-sandbox/x86_64-osx-ghc-7.10.0.20141222-packages.conf.d/ --enable-bench 

cabal build

dist/build/benchmarks/benchmarks -o bench.html

open bench.html
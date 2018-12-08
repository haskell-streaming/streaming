nix-shell -I nixpkgs=https://github.com/nixos/nixpkgs-channels/archive/nixos-17.09.tar.gz -p haskell.compiler.ghc7103 --run "cabal new-build"
nix-shell -I nixpkgs=https://github.com/nixos/nixpkgs-channels/archive/nixos-17.09.tar.gz -p haskell.compiler.ghc802 --run "cabal new-build"
nix-shell -I nixpkgs=https://github.com/nixos/nixpkgs-channels/archive/nixos-18.09.tar.gz -p haskell.compiler.ghc822 --run "cabal new-build"
nix-shell -I nixpkgs=https://github.com/nixos/nixpkgs-channels/archive/nixos-18.09.tar.gz -p haskell.compiler.ghc844 --run "cabal new-build"
nix-shell -I nixpkgs=https://github.com/nixos/nixpkgs-channels/archive/nixos-18.09.tar.gz -p haskell.compiler.ghc862 --run "cabal new-build"


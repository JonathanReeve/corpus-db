{ haskellPackages ? (import <nixpkgs> {}).haskellPackages}:
haskellPackages.callCabal2nix "corpus-db" ./. {}

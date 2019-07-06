{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcHEAD" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./corpus-db.nix { }

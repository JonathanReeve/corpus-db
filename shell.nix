{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, blaze-html, blaze-markup, clay
      , containers, convertible, HDBC, HDBC-sqlite3, monad-logger
      , persistent, persistent-sqlite, persistent-template
      , regex-pcre-builtin, resourcet, scotty, stdenv, text, time
      , transformers, wai-extra, wai-middleware-static
      }:
      mkDerivation {
        pname = "corpus-db";
        version = "0.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base blaze-html blaze-markup clay containers convertible HDBC
          HDBC-sqlite3 monad-logger persistent persistent-sqlite
          persistent-template regex-pcre-builtin resourcet scotty text time
          transformers wai-extra wai-middleware-static
        ];
        homepage = "https://github.com/JonathanReeve/corpus-db#readme";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

# -*- compile-command: "nix-shell --run 'cabal exec -- ghc-pkg list'"; -*-
{ pkgs ? import (import ./nix/sources.nix {}).nixpkgs {}, sources ? import ./nix/sources.nix {} }:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix
with pkgs.haskell.lib.compose;
pkgs.haskellPackages.developPackage {
    root = ./.;
    withHoogle = false;
    returnShellEnv = false;
    overrides = self: _:  let
      minimalCompilation = x: disableLibraryProfiling (dontHaddock x);
      minSource = name: minimalCompilation (self.callCabal2nix name sources.${name} {});
    in {
      yahp      = minSource "yahp";
      interval  = minSource "interval";
    };
    source-overrides = {
      # vector-algorithms = "0.9.0.1";
      chronos = sources.chronos;
    };
    modifier = with pkgs.haskell.lib; drv:
      disableLibraryProfiling (dontHaddock (addBuildTools drv
        (with pkgs.haskellPackages; [cabal-install ghcid])));
  }

{ arch ? builtins.currentSystem }:

# This module provides default settings for the project for easy upgrading.
let
  compiler = "ghc8107";
  pkgs = import ./nixpkgs.nix {
    localSystem.system = arch;
    overlays = [
      (hself: hsuper: {
        mkDerivation = args: hsuper.mkDerivation (args // {
          doCheck = false;
          doHaddock = false;
        });
      })
    ];
  };
in
{
  inherit compiler;
  compiler-dots = "ghc-8.10.7";

  inherit pkgs;
  hpkgs = pkgs.haskell.packages.${compiler};

  # library profiling is disabled by default it causes a profile build as well
  # as a normal build.
  enableProfiling = false;
}

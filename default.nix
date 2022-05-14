{ settings ? import ./nix/settings.nix }:

let

  yesod-contact-form =
    settings.hpkgs.callCabal2nix "yesod-contact-form" ./. {};

  pkgs = settings.pkgs;

in

  pkgs.haskell.lib.overrideCabal yesod-contact-form (drv: {
    src = ./.;
    configureFlags = ["-f-library-only"];
    doCheck = false;
    doHaddock = false;
    enableLibraryProfiling = false;
    enableSeparateDataOutput = false;
    enableSharedExecutables = false;
    isLibrary = true;
    isExecutable = true;
    postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
    testHaskellDepends = [];
  })

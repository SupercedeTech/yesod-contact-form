{ settings ? import ./nix/settings.nix {} }:

let
  pkgs = settings.pkgs;
  hpkgs = settings.hpkgs;
  yesod-contact-form =
    hpkgs.callCabal2nix "yesod-contact-form" ./. {};

in
  hpkgs.shellFor {
    packages = ps : [ yesod-contact-form ];
    NIX_PATH="nixpkgs=${pkgs.path}:.";
  }

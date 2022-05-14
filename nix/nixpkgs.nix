let
  # nixos-21.05, committed at 1639130158 (Fri Dec 10 2021)
  rev = "3b422991781220853035dbe353ba8b7cb279b6b3";
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
in
  import (builtins.fetchTarball url)

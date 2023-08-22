{ pkgs }:
  {
    asli-plugin = pkgs.callPackage ./bap-asli-plugin.nix {};
    bap = pkgs.callPackage ./bap.nix { plugins = [ pkgs.asli-plugin ]; };
  }

{ system ? builtins.currentSystem }:
  (import ./nix.nix { inherit system; }).shell

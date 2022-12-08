# purs-nix package information

{ ps-pkgs, ... }: {

  src = "src";

  dependencies =
    with ps-pkgs;
    [
      prelude
      console
      effect
      maybe
      either
      tuples
      newtype
      refs
      foldable-traversable
      profunctor-lenses
      node-fs
      unordered-collections  # class Hashable
    ];

}

# purs-nix package information

{ ps-pkgs, ... }: {

  src = "mation";

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
    ];

}

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
      strings
      foreign
      foldable-traversable
      profunctor-lenses
      unlift
      nullable

      aff-promise  # for one of the samples ...
    ];

}

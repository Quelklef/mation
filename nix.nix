{ system }:

let

pkgs =
  let
    rev = "37cc765b36b0f74e4f18800e466496bacb366a35";
    src =
      builtins.fetchTarball
        "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  in
    import src {};

get-flake =
  import
    (pkgs.fetchFromGitHub
      { owner = "ursi";
        repo = "get-flake";
        rev = "703f15558daa56dfae19d1858bb3046afe68831a";
        sha256 = "1crp9fpvwg53ldkfxnh0wyxx470lm8bs025rck2bn5jn8jqmhj6f";
      });

purs-nix =
  get-flake
    (pkgs.fetchFromGitHub
      { owner = "ursi";
        repo = "purs-nix";
        rev = "66427405d2a3e0c2491646a6efc1716ce3731f3d";
        sha256 = "sha256-aArvsmkMc+LF2wEdLryiX/kqzMjtLsbcniIuSfFKgxg=";
      }
    ) { inherit system; };

nixed = purs-nix.purs
  { srcs = [ ./src ];
    dependencies =
      with purs-nix.ps-pkgs;
      [ console
        effect
        psci-support
        ordered-collections
        lists
        maybe
        newtype
        refs
        argonaut-core
        argonaut-codecs
        argonaut-generic
        either
        foldable-traversable
        profunctor-lenses
        partial
        prelude
        strings
        transformers
        tuples
        bifunctors
        integers
        numbers
        control
        arrays
        lazy
        node-fs
        stringutils
        random
        aff
        aff-promise
        quickcheck
      ];
  };

purs-nix-bundle-args = {
  esbuild.format = "iife";
  # ^ necessary in some cases due to js bizareness
  #   compare 'var top = 5; console.log(top);'
  #   with '(function() { var top = 5; console.log(top); })'
  module = "Mation.Example";
};

in {

  deriv = pkgs.stdenv.mkDerivation {
    src = ./.;
    name = "mation";

    installPhase = ''
      mkdir $out

      cp $src/index.html $out/
      cp ${nixed.modules.Main.bundle purs-nix-bundle-args} $out/main.js

      echo "${pkgs.python3}/bin/python3.8 -m http.server -d \$(dirname \$(readlink -f \$0))" > $out/run.sh
      chmod +x $out/run.sh
    '';
  };

  shell = pkgs.mkShell {
    buildInputs = [
      (nixed.command {
        srcs = [ "$PWD/src" ];
        bundle = purs-nix-bundle-args;
      })
      pkgs.python3
      pkgs.entr
    ];

    shellHook = ''
    '';
  };

}

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

nixed = purs-nix.purs (import ./package.nix purs-nix);

purs-nix-bundle-args = {
  esbuild.format = "iife";
  # ^ necessary in some cases due to js bizareness
  #   compare 'var top = 5; console.log(top);'
  #   with '(function() { var top = 5; console.log(top); })'
  module = "Mation.Examples.AllExamples";
};

in {

  deriv = null;

  shell = pkgs.mkShell {
    buildInputs = [
      (nixed.command {
        srcs = [ "$PWD/src" ];
        bundle = purs-nix-bundle-args;
      })
      pkgs.python3
      pkgs.nodejs
      pkgs.entr
    ];

    shellHook = ''

      root=$PWD

      function mation.build {(
        cd "$root"
        node ./src/Mation/Gen/generate.js &&
        purs-nix bundle
      )}

      function mation.devt {(
        export -f mation.build
        { find . -name '*.purs';
          find src -name '*.js';
        } | entr -crs 'mation.build && python3 -m http.server'
      )}

    '';
  };

}

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
  };
  outputs = { self, ... }@inputs: let

    system = "x86_64-linux";
    pkgs = inputs.nixpkgs.legacyPackages.${system};
    purs-nix = inputs.purs-nix { inherit system; };

    nixed = purs-nix.purs (import ./package.nix purs-nix);

    purs-nix-bundle-args = {
      esbuild.format = "iife";
      # ^ necessary in some cases due to js bizareness
      #   compare 'var top = 5; console.log(top);'
      #   with '(function() { var top = 5; console.log(top); })'
      module = "Mation.Examples.AllExamples";
    };

    # Used in both development as well as for the demo
    shell-pkgs = [
        (nixed.command {
          srcs = [ "$PWD/src" ];
          bundle = purs-nix-bundle-args;
        })
        pkgs.python3
        pkgs.nodejs
        pkgs.entr
      ];
    shell-hook = ''
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

  in {

    devShells.${system}.default = pkgs.mkShell {
      buildInputs = shell-pkgs;
      shellHook = shell-hook;
    };

    apps.${system}.demo = let

      mation-interactive-demo = pkgs.writeScript
        "mation-interactive-demo"
        ''
          #!${pkgs.bash}/bin/bash

          set -euo pipefail

          demoloc="$(pwd)/mation-demo"

          echo 'Mation interactive demo!'
          echo "This demo will create a (temporary!) directory at $demoloc and serve on localhost:8000"
          read -p 'Is that OK? press enter to continue or Ctrl+C to quit '

          mkdir "$demoloc"
          trap "rm -rf '$demoloc'" EXIT  # remove on quit
          cd "$demoloc"
          cp ${./.}/. -r .
          chmod +w -R .

          export PATH="$PATH:${pkgs.lib.strings.makeBinPath shell-pkgs}"
          ${shell-hook}

          mation.build

          clear
          echo 'Demo is prepared!'
          echo 'Now we will serve the demo on localhost:8000.'
          echo "The demo code is located at $(pwd)/src/Mation/Examples. You are encouraged to poke around and mess around! The module Examples/Demo is a stub file for you to create your own app at. If you edit this file the demo will automatically recompile and reload."
          read -p 'Press enter to run the demo. Press Crtl+C to quit '

          mation.devt
        '';

    in {
      type = "app";
      program = "${mation-interactive-demo}";
    };

  };
}

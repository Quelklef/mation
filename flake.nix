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

  purs-nix-command =
    nixed.command {
      srcs = [ "$PWD/src" ];
      bundle = purs-nix-bundle-args;
      output = "out/purs-cache";
    };

  # For both development and the live demo
  devt-shell = {
    runtime-deps = [
        purs-nix-command
        pkgs.python3
        pkgs.nodejs
        pkgs.entr
        pkgs.findutils  # find
      ];

    shell-hook = ''
      root=$PWD

      function mation.devt {(
        cd "$root" &&
        mkdir -p out/app &&
        python3 -m http.server --directory out/app & trap "kill $!" EXIT
        { find . -name '*.purs';
          find src -name '*.js';
        } | entr -cs "
              node ./src/Mation/Gen/generate.js &&
              purs-nix bundle && mv main.js out/app/main.js &&
              cp index.html out/app/index.html &&
              echo 'You may need to reload your browser'
          "
      )}

      function mation.devt.docs {(
        cd "$root" &&
        mkdir -p out/docs &&
        python3 -m http.server --directory out/docs & trap "kill $!" EXIT
        { find . -name '*.purs';
          find src -name '*.js';
        } | entr -cs "
              node ./src/Mation/Gen/generate.js &&
              purs-nix docs -o out/docs &&
              echo 'Serving on localhost:8000'
          "
      )}
    '';
  };

  docs-deriv = pkgs.stdenv.mkDerivation {
    name = "mation-docs";
    src = ./.;
    buildInputs = [ purs-nix-command ];
    installPhase = ''
      purs-nix docs
      mkdir $out
      cp -r generated-docs/html/. $out
    '';
  };

in {

  # -- development shell -- #

  devShells.${system}.default = pkgs.mkShell {
    buildInputs = devt-shell.runtime-deps;
    shellHook = devt-shell.shell-hook;
  };


  # -- documentation -- #

  packages.${system}.docs =
    docs-deriv;


  apps.${system} = {

    # -- documentation -- #

    docs.type = "app";
    docs.program = builtins.toString (
      pkgs.writeScript
      "mation-serve-docs"
      ''
      ${pkgs.python3}/bin/python3 -m http.server --directory ${docs-deriv}
      ''
    );

    # -- interactive demo -- #

    demo.type = "app";
    demo.program = let
      # Reset shell environment and do not read profile/rc files
      bash-pure = pkgs.writeScript "bash-pure" ''
        #!${pkgs.bash}/bin/bash
        exec env -i ${pkgs.bash}/bin/bash --noprofile --norc "$@"
      '';
      # Inherit shell environment but do not read profile/rc files
      bash-semipure = pkgs.writeScript "bash-semipure" ''
        #!${pkgs.bash}/bin/bash
        exec ${pkgs.bash}/bin/bash --noprofile --norc "$@"
      '';
    in builtins.toString (
      pkgs.writeScript
      "mation-interactive-demo"
      ''
      #!${bash-pure}
      set -euo pipefail

      export PATH="${pkgs.lib.strings.makeBinPath [ pkgs.coreutils ]}:''${PATH:+$PATH}"

      demoloc="$(pwd)/mation-demo"
      examplesloc="$demoloc/src/Mation/Examples"
      moduleloc="$examplesloc/Counter.purs"
      hostloc='localhost:8000'

      original_pwd=$(pwd)
      mkRelative() { echo -n './'; realpath --relative-to="$original_pwd" "$1"; }

      echo
      echo 'Mation interactive demo!'
      echo '========================'
      echo
      echo 'This script will do the following:'
      echo " 1. Create a temporary directory at $(mkRelative "$demoloc")"
      echo " 2. Serve the demo at $hostloc"
      echo
      read -p 'All good? Press enter to continue or Ctrl-C to cancel. '

      mkdir "$demoloc"
      trap "rm -rf '$demoloc'" EXIT  # remove on quit
      cd "$demoloc"
      cp ${./.}/. -r .
      chmod +w -R .

      export PATH="${pkgs.lib.strings.makeBinPath devt-shell.runtime-deps}:''${PATH:+$PATH}"
      ${devt-shell.shell-hook}

      echo
      echo "Demo initialized at $examplesloc"
      echo
      echo 'Now I would recommend:'
      echo " 1. Opening your browser to $hostloc"
      echo " 2. Opening your editor to $moduleloc"
      echo
      read -p 'Press enter when done! '

      # Have entr use bash
      # Don't use bash-pure because we do want it to inherit *this* environment
      export SHELL='${bash-semipure}'

      mation.devt
      ''
    );

  };

};

}

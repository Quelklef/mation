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
    };

  # For both development and the live demo
  devt-shell = {
    runtime-deps = [
        purs-nix-command
        pkgs.python3
        pkgs.nodejs
        pkgs.entr
      ];

    shell-hook = ''
      root=$PWD

      function mation.devt {(
        { find . -name '*.purs';
          find src -name '*.js';
        } | entr -crs "
              cd '$root' &&
              node ./src/Mation/Gen/generate.js &&
              purs-nix bundle &&
              echo 'You may need to reload your browser' &&
              python3 -m http.server
          "
      )}

      function mation.devt.docs {(
        { find . -name '*.purs';
          find src -name '*.js';
        } | entr -crs "
              cd '$root' &&
              node ./src/Mation/Gen/generate.js &&
              purs-nix docs &&
              python3 -m http.server --directory generated-docs/html
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
    demo.program = builtins.toString (
      pkgs.writeScript
      "mation-interactive-demo"
      ''
      #!${pkgs.bash}/bin/bash
      set -euo pipefail

      export PATH="$PATH:${pkgs.lib.strings.makeBinPath [ pkgs.coreutils ]}"

      demoloc="$(pwd)/mation-demo"
      examplesloc="$demoloc/src/Mation/Examples"
      moduleloc="$examplesloc/Counter.purs"
      hostloc='localhost:8000'

      xterm=${pkgs.xterm}/bin/xterm
      terminal=''${TERM:-$xterm}

      vim=${pkgs.vim}/bin/vim
      editor=''${EDITOR:-$vim}
      editor=''${VISUAL:-$editor}

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

      export PATH="$PATH:${pkgs.lib.strings.makeBinPath devt-shell.runtime-deps}"
      ${devt-shell.shell-hook}

      echo
      echo "Demo initialized at $examplesloc"
      echo
      echo 'Now I would recommend:'
      echo " 1. Opening your browser to $hostloc"
      echo " 2. Opening your editor to $moduleloc"
      echo
      read -p 'Press enter when done! '

      mation.devt
      ''
    );

  };

};

}

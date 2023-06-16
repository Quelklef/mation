{

inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
};

outputs = { self, ... }@inputs: let

  system = "x86_64-linux";
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  purs-nix = inputs.purs-nix { inherit system; };

  easy-purescript-nix = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "0c10ff170461aed0c336f5c21ed0f430c2c3574b";
      sha256 = "sha256-LLqaLPJNiap2U8I77K5XVPGJA/Be30Z8lyGOyYXmBlc=";
    }) { inherit pkgs; };

  my-purs-nix = purs-nix.purs (import ./package.nix purs-nix);


  # nb. We compile our codebase with purescript-psa [1] to have better
  # compiler error/warning messages. since purs-nix does not itself
  # support using psa [2], we have to reimplement some of the compilation
  # and bundling logic ourselves
  #
  # the upshot of this is that some compilation/bundling logic is
  # effectively duplicated. this means that changing bundling/compilation
  # details, such as modifying some tool's flags, will require both
  # changing how we invoke purs-nix and changing our own bundling/compilation
  # implementation.
  #
  # [1]: https://www.npmjs.com/package/purescript-psa
  # [2]: https://github.com/purs-nix/purs-nix/issues/45

  esbuild-format = "iife";
    # ^ necessary in some cases due to js bizareness
    #   compare 'var top = 5; console.log(top);'
    #   with '(function() { var top = 5; console.log(top); })'

  main-module = "Mation.Samples.AllSamples";

  # purs-nix command
  #
  # At the time of writing we only use this for 'purs-nix srcs', hence
  # the lack of other options
  purs-nix-command =
    my-purs-nix.command {
      srcs = [ "$PWD/mation" "$PWD/additional" "$PWD/samples" "$PWD/experimental" ];
      test = "/dev/null";  # using nix 'null' value breaks?
    };

  # purs-nix command ignoring non-lib code like samples/
  purs-nix-command-lib-only =
    my-purs-nix.command {
      srcs = [ "$PWD/mation" "$PWD/additional" ];
      test = "/dev/null";
      output = "out/purs-cache";
      name = "purs-nix-lib-only";
      bundle = {
        esbuild.format = esbuild-format;
        module = main-module;
      };
    };

  # Purescript warnings to suppress when compiling the codebase
  censor-warnings = [

    # Suppress warning on use of '_' in types
    "WildcardInferredType"

    # Suppress warning when wildcard-importing into a shared module name, eg.
    #
    #   import Thing as X
    #   import Other (thing) as X
    #
    # We do this several times when we re-exporting entire Mation.Gen.<Name> modules
    "ImplicitQualifiedImport"

    # Suppress warning on variable shadowing
    # Variable shadowing is expected & intended when using 'prune'
    "ShadowedName"

    # Suppress warning on unused values
    # This is a temporary exception for Mation.Lenses
    "UnusedDeclaration"

  ];

  # For both development and the live demo
  devt-shell = pkgs.mkShell {
    buildInputs = [

        # For compiling/bundling the project
        purs-nix-command
        easy-purescript-nix.psa
        easy-purescript-nix.purs
        pkgs.esbuild

        # For compiling the docs
        purs-nix-command-lib-only

        # For serving the result
        pkgs.python3

        # For running generate.js
        pkgs.nodejs

        # For file watching
        pkgs.entr
        pkgs.findutils  # `find`

      ];

    shellHook = ''
      root=$PWD

      function mation.compile {(
        cd "$root" &&
        mkdir -p out/app &&
        node ./mation/Gen/generate.js &&

        # Compile the application. Use 'psa' for sophisticated warning/error handling
        psa \
          --censor-codes=${pkgs.lib.strings.concatStringsSep "," censor-warnings} \
          --stash=out/.psa-stash \
          $(purs-nix srcs) --output out/purs-cache \
          &&

        # The readme sample compiles, so reup the readme
        node ./readme/gen-README.js
      )}

      function mation.bundle {(
        cd "$root" &&
        mation.compile &&
        cp samples/index.html out/app/index.html &&

        # Bundle the application
        echo 'import { main } from "./out/purs-cache/${main-module}/index.js"; main()' \
          | esbuild \
              --bundle \
              --format='${esbuild-format}' \
              --log-level=warning \
              --outfile=out/app/main.js
      )}

      function mation.devt {(
        cd "$root" &&
        mkdir -p out/app &&
        python3 -m http.server --directory out/app & trap "kill $!" EXIT
        export -f mation.compile mation.bundle
        { find . \( -name '*.purs' -o -name '*.js' -o -name '*.html' -o -name '*.md' \) \
                 -a ! -path './out/*'
        } | entr -cs "mation.bundle && echo 'You may need to reload your browser'"
      )}

      function mation.devt.docs {(
        cd "$root" &&
        mkdir -p out/docs &&
        python3 -m http.server --directory out/docs & trap "kill $!" EXIT
        { find ./mation -name '*.purs' -o -name '*.js' -o -name '*.html'
        } | entr -cs "
              node ./mation/Gen/generate.js &&
              purs-nix-lib-only docs -o out/docs &&
              echo 'Serving on localhost:8000'
          "
      )}
    '';
  };

  generated-docs-deriv = pkgs.stdenv.mkDerivation {
    name = "mation-gendocs";
    src = ./.;
    buildInputs = [ purs-nix-command-lib-only ];
    installPhase = ''
      purs-nix-lib-only docs
      mkdir $out
      cp -r generated-docs/html/. $out
    '';
  };

in {

  # -- development shell -- #
  devShells.${system}.default = devt-shell;

  # -- generated documentation -- #
  packages.${system}.generated-docs =
    docs-deriv;

  apps.${system} = {
    # -- generated documentation -- #
    generated-docs.type = "app";
    generated-docs.program = builtins.toString (
      pkgs.writeScript "mation-serve-docs" ''
        ${pkgs.python3}/bin/python3 -m http.server --directory ${docs-deriv}
      ''
    );
  };

};

}

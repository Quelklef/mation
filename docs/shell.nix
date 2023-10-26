{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [ pkgs.mkdocs ];

  shellHook = ''
    echo "Run 'mkdocs serve'"
  '';
}

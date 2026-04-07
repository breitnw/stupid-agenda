{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = with pkgs; [
    racket
    # also requires dependency csv-reading
  ];
}

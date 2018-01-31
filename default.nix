{ pkgs ? import <nixpkgs> {} }:
with pkgs.haskell.lib;
let
  c-dsl = doJailbreak pkgs.haskellPackages.c-dsl;
in
addBuildDepends (pkgs.haskellPackages.callPackage ./cake3.nix {})
  (with pkgs.haskellPackages; [alex happy c-dsl])


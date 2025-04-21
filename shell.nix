# We recommend using the flake.nix file, see https://nixos.wiki/wiki/Flakes for instructions.
# This file is kept to maintain compatibility with tools like lorri until they support flakes (https://github.com/target/lorri/issues/460).
{ system ? builtins.currentSystem }:

(builtins.getFlake (toString ./.)).devShells.${system}

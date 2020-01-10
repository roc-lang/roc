let
  # Look here for information about how to generate `nixpkgs-version.json`.
  #  → https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  pinnedVersion =
    builtins.fromJSON (builtins.readFile ./nix/nixpkgs-version.json);
  pinnedPkgs = import (builtins.fetchGit {
    inherit (pinnedVersion) url rev;

    ref = "nixos-unstable";
  }) { };

  # This allows overriding pkgs by passing `--arg pkgs ...`
in { pkgs ? pinnedPkgs }:

let

  isOsX = builtins.currentSystem == "x86_64-darwin";
  darwin-frameworks = if isOsX then
    with pkgs.darwin.apple_sdk.frameworks; [
      Security
      CoreFoundation
      CoreServices
    ]
  else
    [ ];
  inputs = pkgs.callPackage ./nix/inputs.nix { };
in pkgs.mkShell { buildInputs = inputs ++ darwin-frameworks; }

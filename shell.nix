let
  # Look here for information about how pin version of nixpkgs
  #  → https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  pinnedPkgs = import (builtins.fetchGit {
    name = "nixpkgs-20.03";
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/release-20.03";
  }) { };

  # This allows overriding pkgs by passing `--arg pkgs ...`
in { pkgs ? pinnedPkgs }:

let
  isOsX = builtins.currentSystem == "x86_64-darwin";
  darwin-frameworks =
    if isOsX then
      with pkgs.darwin.apple_sdk.frameworks; [
        AppKit
        CoreFoundation
        CoreServices
        CoreVideo
        Foundation
        Metal
        Security
      ]
    else
      [ ];
  llvm = pkgs.llvm_10;
  inputs =
    [
      pkgs.rustup
      pkgs.cargo
      llvm
      # libraries for llvm
      pkgs.libffi
      pkgs.libxml2
      pkgs.zlib
    ];
in pkgs.mkShell {
  buildInputs = inputs ++ darwin-frameworks;
  LLVM_SYS_100_PREFIX = "${llvm}";
}


{ }:

with {
  # Look here for information about how pin version of nixpkgs
  #  → https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  pkgs = import (builtins.fetchGit {
    name = "nixpkgs-2020-10-24";
    url = "https://github.com/nixos/nixpkgs-channels/";
    ref = "refs/heads/nixpkgs-unstable";
    rev = "502845c3e31ef3de0e424f3fcb09217df2ce6df6";
  }) { };

  isMacOS = builtins.currentSystem == "x86_64-darwin";
};

let
  darwin-frameworks =
    if isMacOS then
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
  lld = pkgs.lld_10; # this should match llvm's version
  zig = import ./nix/zig.nix { inherit pkgs isMacOS; };
  inputs =
    [
      # build libraries
      pkgs.rustup
      pkgs.cargo
      llvm
      pkgs.valgrind
      zig
      # llb deps
      pkgs.libffi
      pkgs.libxml2
      pkgs.zlib
      # faster builds - see https://github.com/rtfeldman/roc/blob/trunk/BUILDING_FROM_SOURCE.md#use-lld-for-the-linker
      lld
      # dev tools
      pkgs.rust-analyzer
      # (import ./nix/zls.nix { inherit pkgs zig; })
      pkgs.ccls
    ];
in pkgs.mkShell {
  buildInputs = inputs ++ darwin-frameworks;
  LLVM_SYS_100_PREFIX = "${llvm}";
  shellHook = ''
    alias llvm-as-10="llvm-as"
    export PATH="$PATH:$PWD/nix/bin"
  '';
}


{ }:

let
  splitSystem = builtins.split "-" builtins.currentSystem;
  currentArch = builtins.elemAt splitSystem 0;
  currentOS = builtins.elemAt splitSystem 2;
in with {
  # Look here for information about how pin version of nixpkgs
  #  → https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  pkgs = import (builtins.fetchGit {
    name = "nixpkgs-2020-11-24";
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/nixpkgs-unstable";
    rev = "6625284c397b44bc9518a5a1567c1b5aae455c08";
  }) { };

  isMacOS = currentOS == "darwin";
  isAarch64 = currentArch == "aarch64";
};

with (pkgs);

let
  macos-only = if isMacOS then
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

  linux-only = if !isMacOS then [
    # vulkan-headers
    # vulkan-loader
    # vulkan-tools
    # vulkan-validation-layers
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi
  ] else
    [ ];

  llvmPkgs = pkgs.llvmPackages_10;
  zig = import ./nix/zig.nix { inherit pkgs isMacOS isAarch64; };
  inputs = [
    # build libraries
    rustc
    cargo
    clippy
    rustfmt
    cmake
    git
    python3
    llvmPkgs.llvm
    llvmPkgs.clang
    valgrind
    pkg-config
    zig
    # llb deps
    libffi
    libxml2
    zlib
    llvmPkgs.libcxx
    llvmPkgs.libcxxabi
    libunwind
    # faster builds - see https://github.com/rtfeldman/roc/blob/trunk/BUILDING_FROM_SOURCE.md#use-lld-for-the-linker
    llvmPkgs.lld
    # dev tools
    rust-analyzer
    # (import ./nix/zls.nix { inherit pkgs zig; })
    ccls
  ];

in mkShell {
  buildInputs = inputs ++ macos-only ++ linux-only;
  LLVM_SYS_100_PREFIX = "${llvmPkgs.llvm}";

  APPEND_LIBRARY_PATH = stdenv.lib.makeLibraryPath
    ([ pkg-config llvmPkgs.libcxx llvmPkgs.libcxxabi libunwind ] ++ linux-only);

  # Aliases don't work cross shell, so we do this
  shellHook = ''
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$APPEND_LIBRARY_PATH"
    export PATH="$PATH:$PWD/nix/bin"
  '';
}


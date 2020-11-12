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

with (pkgs);

let
  darwin-frameworks = if isMacOS then
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
    vulkan-headers
    vulkan-loader
    vulkan-tools
    vulkan-validation-layers
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi
  ] else
    [ ];

  llvmPkg = pkgs.llvm_10;
  lldPkg = pkgs.lld_10; # this should match llvm's version
  clangPkg = pkgs.clang_10; # this should match llvm's version
  zig = import ./nix/zig.nix { inherit pkgs isMacOS; };
  inputs = [
    # build libraries
    rustc
    cargo
    clippy
    rustfmt
    cmake
    git
    python3
    llvmPkg
    clangPkg
    valgrind
    pkg-config
    zig
    # llb deps
    libffi
    libxml2
    zlib
    # faster builds - see https://github.com/rtfeldman/roc/blob/trunk/BUILDING_FROM_SOURCE.md#use-lld-for-the-linker
    lldPkg
    # dev tools
    rust-analyzer
    # (import ./nix/zls.nix { inherit pkgs zig; })
    ccls
  ];

in mkShell {
  buildInputs = inputs ++ darwin-frameworks ++ linux-only;
  LLVM_SYS_100_PREFIX = "${llvmPkg}";

  APPEND_LIBRARY_PATH = stdenv.lib.makeLibraryPath
    ([ pkgconfig libcxx libcxxabi libunwind ] ++ linux-only);

  # Aliases don't work cross shell, so we do this
  shellHook = ''
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$APPEND_LIBRARY_PATH"
    export PATH="$PATH:$PWD/nix/bin"
  '';
}


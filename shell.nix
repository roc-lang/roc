{}:

let
  # Look here for information about how pin version of nixpkgs
  #  → https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  # TODO: We should probably use flakes at somepoint
  pkgs = import (
    builtins.fetchGit {
      # name = "nixpkgs-2021-04-23";
      url = "https://github.com/nixos/nixpkgs/";
      ref = "refs/heads/nixpkgs-unstable";
      rev = "8d0340aee5caac3807c58ad7fa4ebdbbdd9134d6";
    }
  ) {};

  darwinInputs =
    with pkgs;
    lib.optionals stdenv.isDarwin (
      with pkgs.darwin.apple_sdk.frameworks; [
        AppKit
        CoreFoundation
        CoreServices
        CoreVideo
        Foundation
        Metal
        Security
      ]
    );

  linuxInputs =
    with pkgs;
    lib.optionals stdenv.isLinux [
      valgrind
      vulkan-headers
      vulkan-loader
      vulkan-tools
      vulkan-validation-layers
      xorg.libX11
      xorg.libXcursor
      xorg.libXrandr
      xorg.libXi
      xorg.libxcb
    ];

  llvmPkgs = pkgs.llvmPackages_10;

  # zig = import ./nix/zig-unstable.nix { inherit pkgs; };
  zig = import ./nix/zig.nix { inherit pkgs; };

  inputs = with pkgs;[
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
    pkg-config
    zig

    # lib deps
    llvmPkgs.libcxx
    llvmPkgs.libcxxabi
    libffi
    libunwind
    libxml2
    ncurses
    zlib
    libiconv

    # faster builds - see https://github.com/rtfeldman/roc/blob/trunk/BUILDING_FROM_SOURCE.md#use-lld-for-the-linker
    llvmPkgs.lld
  ];
in
pkgs.mkShell
  {
    buildInputs = inputs ++ darwinInputs ++ linuxInputs;

    # Additional Env vars
    LLVM_SYS_100_PREFIX = "${llvmPkgs.llvm}";
    LD_LIBRARY_PATH =
      with pkgs;
      lib.makeLibraryPath
        (
          [
            pkg-config
            stdenv.cc.cc.lib
            llvmPkgs.libcxx
            llvmPkgs.libcxxabi
            libunwind
            libffi
            ncurses
            zlib
          ]
          ++ linuxInputs
        );

    # Non Nix llvm installs names the bin llvm-as-${version}, so we
    # alias `llvm` to `llvm-as-${version}` here.
    # This the name of the file in nix/bin will need to be updated whenever llvm is updated
    shellHook = ''
      export PATH="$PATH:$PWD/nix/bin"
    '';
  }

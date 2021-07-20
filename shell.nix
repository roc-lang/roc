{}:

let
  sources = import nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

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

  llvmPkgs = pkgs.llvmPackages_12;

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
    llvmPkgs.llvm.dev
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
    LLVM_SYS_120_PREFIX = "${llvmPkgs.llvm.dev}";
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
  }

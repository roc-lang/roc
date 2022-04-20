{
  description = "Roc flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable = { url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };
    zig = { url = "github:roarkanize/zig-overlay"; };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, zig, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        unstable-pkgs = nixpkgs-unstable.legacyPackages.${system};
        llvmPkgs = pkgs.llvmPackages_12;

        linuxInputs = with pkgs;
          lib.optionals stdenv.isLinux [
            valgrind # used in cli tests, see cli/tests/cli_run.rs
            vulkan-headers
            vulkan-loader
            vulkan-tools
            vulkan-validation-layers
            xorg.libX11
            xorg.libXcursor
            xorg.libXrandr
            xorg.libXi
            xorg.libxcb
            alsa-lib
          ];

        darwinInputs = with pkgs;
          lib.optionals stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
            AppKit
            CoreFoundation
            CoreServices
            CoreVideo
            Foundation
            Metal
            Security
      ]);

        # zig 0.8.1 from pkgs is broken on aarch64-darwin, hence the workaround
        zig-toolchain = zig.packages.${system}."0.8.1";

        sharedInputs = (with pkgs; [
          # build libraries
          cmake
          git
          python3
          llvmPkgs.llvm.dev
          llvmPkgs.clang
          libxkbcommon
          pkg-config
          zig-toolchain # roc builtins are implemented in zig, see compiler/builtins/bitcode/

          # lib deps
          libffi
          libxml2
          ncurses
          zlib
          libiconv

          # faster builds - see https://github.com/rtfeldman/roc/blob/trunk/BUILDING_FROM_SOURCE.md#use-lld-for-the-linker
          llvmPkgs.lld
          # debugir
        ]) ++ (with unstable-pkgs; [
          rustc
          cargo
          clippy
          rustfmt
        ]);
      in {
        devShell = pkgs.mkShell {
          buildInputs = sharedInputs ++ darwinInputs ++ linuxInputs;

          LLVM_SYS_120_PREFIX = "${llvmPkgs.llvm.dev}";
          NIX_GLIBC_PATH = if pkgs.stdenv.isLinux then "${pkgs.glibc_multi.out}/lib" else "";
          LD_LIBRARY_PATH = with pkgs;
            lib.makeLibraryPath
            ([ pkg-config stdenv.cc.cc.lib libffi ncurses zlib ]);
        };
      }
  );  
}

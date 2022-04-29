{
  description = "Roc flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    nixpkgs-unstable = { url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    # zig = { url = "github:roarkanize/zig-overlay"; };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, rust-overlay, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        unstable-pkgs = nixpkgs-unstable.legacyPackages.${system};
        #llvmPkgs = pkgs.llvmPackages_12;
        
        # get current working directory
        cwd = builtins.toString ./.;
        rust = pkgs.rust-bin.fromRustupToolchainFile "${cwd}/rust-toolchain.toml";

        linuxInputs = with pkgs; [
          valgrind # used in cli tests, see cli/tests/cli_run.rs
          #vulkan-headers
          #vulkan-loader
          #vulkan-tools
          #vulkan-validation-layers
          #xorg.libX11
          #xorg.libXcursor
          #xorg.libXrandr
          #xorg.libXi
          xorg.libxcb
          #alsa-lib
        ];

        

        sharedInputs = (with pkgs; [
          # build libraries
          #cmake
          #git
          #python3
          #llvmPkgs.llvm.dev
          #llvmPkgs.clang
          #libxkbcommon
          #pkg-config
          #zig # roc builtins are implemented in zig, see compiler/builtins/bitcode/

          # lib deps
          libffi
          libxml2
          ncurses
          zlib
          #libiconv

          # faster builds - see https://github.com/rtfeldman/roc/blob/trunk/BUILDING_FROM_SOURCE.md#use-lld-for-the-linker
          # llvmPkgs.lld
          # debugir
          rust
        ]);
      in {

        devShell = pkgs.mkShell {
          buildInputs = sharedInputs ++ linuxInputs;

          #LLVM_SYS_120_PREFIX = "${llvmPkgs.llvm.dev}";
          #NIX_GLIBC_PATH = if pkgs.stdenv.isLinux then "${pkgs.glibc_multi.out}/lib" else "";
          # LD_LIBRARY_PATH = with pkgs;
          #  lib.makeLibraryPath
          #  ([ stdenv.cc.cc.lib]);
          #  ([ pkg-config stdenv.cc.cc.lib libffi ncurses zlib ] ++ linuxInputs);
        };

      }
    );
}

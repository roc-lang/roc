{
  description = "Roc flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    rust-overlay.url = "github:oxalica/rust-overlay"; # rust from nixpkgs has some libc problems, this is patched in the rust-overlay
    zig.url = "github:roarkanize/zig-overlay"; # using an overlay allows for quick updates after zig releases
    flake-utils.url = "github:numtide/flake-utils"; # to easily make configs for all architectures
  };

  outputs = { self, nixpkgs, rust-overlay, zig, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { 
          inherit system overlays;
        };
        llvmPkgs = pkgs.llvmPackages_13;

        # get current working directory
        cwd = builtins.toString ./.;
        rust = pkgs.rust-bin.fromRustupToolchainFile "${cwd}/rust-toolchain.toml";

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

        zig-toolchain = zig.packages.${system}."0.9.1";

        # For debugging LLVM IR
        debugir = pkgs.stdenv.mkDerivation {
          name = "debugir";
          src = pkgs.fetchFromGitHub {
            owner = "vaivaswatha";
            repo = "debugir";
            rev = "b981e0b74872d9896ba447dd6391dfeb63332b80";
            sha256 = "Gzey0SF0NZkpiObk5e29nbc41dn4Olv1dx+6YixaZH0=";
          };
          buildInputs = with pkgs; [ cmake libxml2 llvmPackages_13.llvm.dev ];
          buildPhase = ''
            mkdir build
            cd build
            cmake -DLLVM_DIR=${pkgs.llvmPackages_13.llvm.dev} -DCMAKE_BUILD_TYPE=Release ../
            cmake --build ../
            cp ../debugir .
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp debugir $out/bin
          '';
        };

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
          debugir
          rust
        ]);
      in {

        devShell = pkgs.mkShell {
          buildInputs = sharedInputs ++ darwinInputs ++ linuxInputs;

          LLVM_SYS_130_PREFIX = "${llvmPkgs.llvm.dev}";
          NIX_GLIBC_PATH = if pkgs.stdenv.isLinux then "${pkgs.glibc_multi.out}/lib" else "";
          LD_LIBRARY_PATH = with pkgs;
            lib.makeLibraryPath
            ([ pkg-config stdenv.cc.cc.lib libffi ncurses zlib ] ++ linuxInputs);
          NIXPKGS_ALLOW_UNFREE = 1; # to run the editor with NVIDIA's closed source drivers
        };

    }
  );
}

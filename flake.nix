{
  description = "Roc flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=676fe5e01b9a41fa14aaa48d87685677664104b1";

    # rust from nixpkgs has some libc problems, this is patched in the rust-overlay
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    # to easily make configs for multiple architectures
    flake-utils.url = "github:numtide/flake-utils";
    # to be able to use vulkan system libs for graphics in examples/gui
    nixgl = {
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, nixgl }:
    let supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" "aarch64-linux" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ (import rust-overlay) ]
          ++ (if system == "x86_64-linux" then [ nixgl.overlay ] else [ ]);
        pkgs = import nixpkgs { inherit system overlays; };

        # When updating the zig or llvm version, make sure they stay in sync.
        # Also update in default.nix (TODO: maybe we can use nix code to sync this)
        zigPkg = pkgs.zig_0_11;
        llvmPkgs = pkgs.llvmPackages_16;
        llvmVersion = builtins.splitVersion llvmPkgs.release_version;
        llvmMajorMinorStr = builtins.elemAt llvmVersion 0 + builtins.elemAt llvmVersion 1;

        # get current working directory
        cwd = builtins.toString ./.;
        rust =
          pkgs.rust-bin.fromRustupToolchainFile "${cwd}/rust-toolchain.toml";

        # DevInputs are not necessary to build roc as a user 
        linuxDevInputs = with pkgs;
          lib.optionals stdenv.isLinux [
            valgrind # used in cli tests, see cli/tests/cli_run.rs
            vulkan-headers # here and below is all graphics stuff for examples/gui
            vulkan-loader
            vulkan-tools
            vulkan-validation-layers
            xorg.libX11
            xorg.libXcursor
            xorg.libXrandr
            xorg.libXi
            xorg.libxcb
          ];

        darwinInputs = with pkgs;
          lib.optionals stdenv.isDarwin
          (with pkgs.darwin.apple_sdk.frameworks; [
            AppKit
            CoreFoundation
            CoreServices
            Foundation
            Security
          ]);

        # DevInputs are not necessary to build roc as a user 
        darwinDevInputs = with pkgs;
          lib.optionals stdenv.isDarwin
          (with pkgs.darwin.apple_sdk.frameworks; [
            CoreVideo # for examples/gui
            Metal # for examples/gui
            curl # for wasm-bindgen-cli libcurl (see ./ci/www-repl.sh)
          ]);

        # For debugging LLVM IR
        debugir = pkgs.stdenv.mkDerivation {
          name = "debugir";
          src = pkgs.fetchFromGitHub {
            owner = "vaivaswatha";
            repo = "debugir";
            rev = "b981e0b74872d9896ba447dd6391dfeb63332b80";
            sha256 = "Gzey0SF0NZkpiObk5e29nbc41dn4Olv1dx+6YixaZH0=";
          };
          buildInputs = with pkgs; [ cmake libxml2 llvmPkgs.llvm.dev ];
          buildPhase = ''
            mkdir build
            cd build
            cmake -DLLVM_DIR=${llvmPkgs.llvm.dev} -DCMAKE_BUILD_TYPE=Release ../
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
          llvmPkgs.llvm.dev
          llvmPkgs.clang
          pkg-config
          zigPkg # roc builtins are implemented in zig, see compiler/builtins/bitcode/
          # lib deps
          libffi
          libxml2
          ncurses
          zlib
          # faster builds - see https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md#use-lld-for-the-linker
          llvmPkgs.lld
          rust
        ]);

        sharedDevInputs = (with pkgs; [
          git
          python3
          libiconv # for examples/gui
          libxkbcommon # for examples/gui
          # debugir needs to be updated to llvm 15
          # debugir # used in crates/compiler/build/src/program.rs
          cargo-criterion # for benchmarks
          simple-http-server # to view roc website when trying out edits
          wasm-pack # for repl_wasm
          jq # used in several bash scripts
          cargo-nextest # used to give more info for segfaults for gen tests
        ]);

        aliases = ''
          alias clippy='cargo clippy --workspace --tests --release -- --deny warnings'
          alias fmt='cargo fmt --all'
          alias fmtc='cargo fmt --all -- --check'
        '';

      in {

        devShell = pkgs.mkShell {
          buildInputs = sharedInputs ++ sharedDevInputs ++ darwinInputs ++ darwinDevInputs++ linuxDevInputs
            ++ (if system == "x86_64-linux" then
              [ pkgs.nixgl.nixVulkanIntel ]
            else
              [ ]);

          # nix does not store libs in /usr/lib or /lib
          # for libgcc_s.so.1
          NIX_LIBGCC_S_PATH =
            if pkgs.stdenv.isLinux then "${pkgs.stdenv.cc.cc.lib}/lib" else "";
          # for crti.o, crtn.o, and Scrt1.o
          NIX_GLIBC_PATH =
            if pkgs.stdenv.isLinux then "${pkgs.glibc.out}/lib" else "";

          LD_LIBRARY_PATH = with pkgs;
            lib.makeLibraryPath
            ([ pkg-config stdenv.cc.cc.lib libffi ncurses zlib ]
              ++ linuxDevInputs);
          NIXPKGS_ALLOW_UNFREE =
            1; # to run the GUI examples with NVIDIA's closed source drivers
          
          shellHook = ''
            export LLVM_SYS_${llvmMajorMinorStr}_PREFIX="${llvmPkgs.llvm.dev}"
            ${aliases}
          '';
        };

        formatter = pkgs.nixpkgs-fmt;

        # You can build this package (the roc CLI) with the `nix build` command.
        packages.default = import ./. { inherit pkgs; };
      });
}

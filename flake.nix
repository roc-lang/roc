{
  description = "Roc flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=821c72743ceae44bdd09718d47cab98fd5fd90af";

    # rust from nixpkgs has some libc problems, this is patched in the rust-overlay
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # to easily make configs for multiple architectures
    flake-utils.url = "github:numtide/flake-utils";
    # to be able to use vulkan system libs for editor graphics
    nixgl = {
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, nixgl }:
    let supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ (import rust-overlay) ]
          ++ (if system == "x86_64-linux" then [ nixgl.overlay ] else [ ]);
        pkgs = import nixpkgs { inherit system overlays; };
        llvmPkgs = pkgs.llvmPackages_13;

        # get current working directory
        cwd = builtins.toString ./.;
        rust =
          pkgs.rust-bin.fromRustupToolchainFile "${cwd}/rust-toolchain.toml";

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
          ];

        darwinInputs = with pkgs;
          lib.optionals stdenv.isDarwin
          (with pkgs.darwin.apple_sdk.frameworks; [
            AppKit
            CoreFoundation
            CoreServices
            CoreVideo
            Foundation
            Metal
            Security
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
          zig_0_9 # roc builtins are implemented in zig, see compiler/builtins/bitcode/

          # lib deps
          libffi
          libxml2
          ncurses
          zlib
          libiconv

          # faster builds - see https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md#use-lld-for-the-linker
          llvmPkgs.lld
          debugir
          rust
          cargo-criterion # for benchmarks
          simple-http-server # to view roc website when trying out edits
          wasm-pack # for repl_wasm
          jq
        ]);

        aliases = ''
          alias clippy='cargo clippy --workspace --tests --release -- --deny warnings'
          alias fmt='cargo fmt --all'
          alias fmtc='cargo fmt --all -- --check'
        '';

      in {

        devShell = pkgs.mkShell {
          buildInputs = sharedInputs ++ darwinInputs ++ linuxInputs
            ++ (if system == "x86_64-linux" then
              [ pkgs.nixgl.nixVulkanIntel ]
            else
              [ ]);

          LLVM_SYS_130_PREFIX = "${llvmPkgs.llvm.dev}";
          # nix does not store libs in /usr/lib or /lib
          NIX_GLIBC_PATH =
            if pkgs.stdenv.isLinux then "${pkgs.glibc.out}/lib" else "";
          LD_LIBRARY_PATH = with pkgs;
            lib.makeLibraryPath
            ([ pkg-config stdenv.cc.cc.lib libffi ncurses zlib ]
              ++ linuxInputs);
          NIXPKGS_ALLOW_UNFREE =
            1; # to run the editor with NVIDIA's closed source drivers
          
          shellHook = ''
            source <(echo "${aliases}")
          '';
        };

        formatter = pkgs.nixpkgs-fmt;

        # You can build this package (the roc CLI) with the `nix build` command.
        packages.default = import ./. { inherit pkgs; };
      });
}

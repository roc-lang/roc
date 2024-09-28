{
  description = "Roc flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=fd281bd6b7d3e32ddfa399853946f782553163b5";

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

    # for non flake backwards compatibility
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, nixgl, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" "aarch64-linux" ];

      templates = import ./nix/templates { };
    in
    { inherit templates; } //
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ (import rust-overlay) ]
        ++ (if system == "x86_64-linux" then [ nixgl.overlay ] else [ ]);
        pkgs = import nixpkgs { inherit system overlays; };

        rocBuild = import ./nix { inherit pkgs; };

        compile-deps = rocBuild.compile-deps;
        inherit (compile-deps) zigPkg llvmPkgs llvmVersion
          llvmMajorMinorStr glibcPath libGccSPath darwinInputs;

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
            cargo-llvm-cov # to visualize code coverage
          ];

        # DevInputs are not necessary to build roc as a user
        darwinDevInputs = with pkgs;
          lib.optionals stdenv.isDarwin
            (with pkgs.darwin.apple_sdk.frameworks; [
              CoreVideo # for examples/gui
              Metal # for examples/gui
              curl # for wasm-bindgen-cli libcurl (see ./ci/www-repl.sh)
            ]);

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
          rocBuild.rust-shell
          perl # ./ci/update_basic_cli_url.sh
        ]);

        sharedDevInputs = (with pkgs; [
          git
          python3
          libiconv # for examples/gui
          libxkbcommon # for examples/gui
          cargo-criterion # for benchmarks
          simple-http-server # to view roc website when trying out edits
          wasm-pack # for repl_wasm
          jq # used in several bash scripts
          cargo-nextest # used to give more info for segfaults for gen tests
          zls # zig language server
          # cargo-udeps # to find unused dependencies
        ]);

        aliases = ''
          alias clippy='cargo clippy --workspace --tests --release -- --deny warnings'
          alias fmt='cargo fmt --all'
          alias fmtc='cargo fmt --all -- --check'
        '';

      in
      {

        devShell = pkgs.mkShell {
          buildInputs = sharedInputs ++ sharedDevInputs ++ darwinInputs ++ darwinDevInputs ++ linuxDevInputs
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
          '' + pkgs.lib.optionalString (system == "aarch64-darwin") ''
            export RUSTFLAGS="-C link-arg=-lc++abi"
          ''; # lc++abi as workaround for github.com/NixOS/nixpkgs/issues/166205, see also github.com/roc-lang/roc/issues/6303
        };

        formatter = pkgs.nixpkgs-fmt;

        # You can build this package (the roc CLI) with the `nix build` command.
        packages = {
          default = rocBuild.roc-cli;

          # all rust crates in workspace.members of Cargo.toml
          full = rocBuild.roc-full;
          # only the CLI crate = executable provided in nightly releases
          cli = rocBuild.roc-cli;
          cli-debug = rocBuild.roc-cli-debug;

          lang-server = rocBuild.roc-lang-server;
          lang-server-debug = rocBuild.roc-lang-server-debug;
        };

        apps = {
          default = {
            type = "app";
            program = "${rocBuild.roc-cli}/bin/roc";
          };
        };
      });
}

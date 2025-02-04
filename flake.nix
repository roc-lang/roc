{
  description = "Roc flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=184957277e885c06a505db112b35dfbec7c60494";

    # rust from nixpkgs has some libc problems, this is patched in the rust-overlay
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
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
    in {
      inherit templates;
      lib = { buildRocPackage = import ./nix/buildRocPackage.nix; };
    } //
    flake-utils.lib.eachSystem supportedSystems (system:
      let

        overlays = [ (import rust-overlay) ]
        ++ (if system == "x86_64-linux" then [ nixgl.overlay ] else [ ])
        ++ [(final: prev: {
          # using a custom simple-http-server fork because of github.com/TheWaWaR/simple-http-server/issues/111
          # the server is used for local testing of the roc website
          simple-http-server = final.callPackage ./nix/simple-http-server.nix { };
        })];
        pkgs = import nixpkgs { inherit system overlays; };

        rocBuild = import ./nix { inherit pkgs; };

        compile-deps = rocBuild.compile-deps;
        inherit (compile-deps) zigPkg llvmPkgs llvmVersion
          llvmMajorMinorStr glibcPath libGccSPath darwinInputs;

        # DevInputs are not necessary to build roc as a user
        linuxDevInputs = with pkgs;
          lib.optionals stdenv.isLinux [
            valgrind # used in cli tests, see cli/tests/cli_tests.rs
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
            curl # used by www/build.sh
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
          # provides llvm
          llvmPkgs.dev
          # for debugging:
          # lldb
          # faster builds - see https://github.com/roc-lang/roc/blob/main/BUILDING_FROM_SOURCE.md#use-lld-for-the-linker
          # provides lld
          pkgs.lld_18
          # provides clang
          pkgs.clang_18

          pkg-config

          zigPkg # roc builtins are implemented in zig, see compiler/builtins/bitcode/

          # lib deps
          libffi
          libxml2
          ncurses
          zlib
          rocBuild.rust-shell
          perl # ./ci/update_basic_cli_url.sh
        ]);

        sharedDevInputs = (with pkgs; [
          git
          python3
          libiconv # for examples/gui
          libxkbcommon # for examples/gui
          cargo-criterion # for benchmarks
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

          LD_LIBRARY_PATH =  with pkgs;
            lib.makeLibraryPath
              ([ pkg-config stdenv.cc.cc.lib libffi ncurses zlib ]
              ++ linuxDevInputs);

          NIXPKGS_ALLOW_UNFREE =
            1; # to run the GUI examples with NVIDIA's closed source drivers

          shellHook = ''
            export LLVM_SYS_180_PREFIX="${llvmPkgs.dev}"
            ${aliases}

            # https://github.com/ziglang/zig/issues/18998
            unset NIX_CFLAGS_COMPILE
            unset NIX_LDFLAGS
          '';
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

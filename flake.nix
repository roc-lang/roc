{
  description = "Roc flake";

  inputs = {
    cargo2nix.url = "github:cargo2nix/cargo2nix/master"; # improves caching of dependent rust crates
    flake-utils.url = "github:numtide/flake-utils"; # to easily make configs for all architectures
    rust-overlay.url = "github:oxalica/rust-overlay"; # rust from nixpkgs has some libc problems, this is patched in the rust-overlay
    zig.url = "github:roarkanize/zig-overlay"; # zig 8.1 is broken on nixpkgs for M1 macs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
  };

  outputs = { self, nixpkgs, cargo2nix, rust-overlay, zig, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
                        (import "${cargo2nix}/overlay") # contains a custom rustBuilder
                        rust-overlay.overlay
                      ];
        };

        llvmPkgs = pkgs.llvmPackages_13;

        # create the workspace & dependencies package set
        rustPkgs = pkgs.rustBuilder.makePackageSet' {
          rustChannel = "1.60.0"; # TODO get this from rust-toolchain file
          packageFun = import ./Cargo.nix;
          packageOverrides = pkgs: pkgs.rustBuilder.overrides.all ++ [
            (pkgs.rustBuilder.rustLib.makeOverride {
              name = "wasi_libc_sys";
              overrideAttrs = drv: {
                buildInputs = drv.buildInputs or [ ] ++ [
                  pkgs.zig
                ];
              };
            })
          ] ++ [
            (pkgs.rustBuilder.rustLib.makeOverride {
              name = "llvm-sys";
              overrideAttrs = drv: {
                buildInputs = drv.buildInputs or [ ] ++ [
                  llvmPkgs.llvm.dev
                ];
              };
            })
          ];
        };        

        # get current working directory
        cwd = builtins.toString ./.;
        #rust = pkgs.rust-bin.fromRustupToolchainFile "${cwd}/rust-toolchain.toml";

        linuxInputs = with pkgs; [
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

        # zig 0.9.1 from pkgs is broken on aarch64-darwin, hence the workaround
        zig-toolchain = zig.packages.${system}."0.9.1";

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

          rust-bin
        ]);

        workspaceShell = rustPkgs.workspaceShell {
          buildInputs = sharedInputs ++ linuxInputs;
        };
      in rec {

        devShell = pkgs.mkShell {
          buildInputs = sharedInputs ++ linuxInputs;

          LLVM_SYS_130_PREFIX = "${llvmPkgs.llvm.dev}";
          NIX_GLIBC_PATH = if pkgs.stdenv.isLinux then "${pkgs.glibc_multi.out}/lib" else "";
          LD_LIBRARY_PATH = with pkgs;
            lib.makeLibraryPath
            ([ pkg-config stdenv.cc.cc.lib libffi ncurses zlib ] ++ linuxInputs);
          NIXPKGS_ALLOW_UNFREE = 1; # to run the editor with NVIDIA's closed source drivers
        };

        packages = {
          # nix build .#roc
          # nix build .#packages.x86_64-linux.roc
          roc = (rustPkgs.workspace.roc_cli {}).bin;
        };

        # nix build
        defaultPackage = packages.roc;
    }
  );
}

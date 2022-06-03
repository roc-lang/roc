{
  description = "Roc flake";

  inputs = {
    cargo2nix.url = "github:cargo2nix/cargo2nix/master"; # improves caching of dependent rust crates
    flake-utils.url = "github:numtide/flake-utils"; # to easily make configs for all architectures
    zig.url = "github:roarkanize/zig-overlay"; # zig 8.1 is broken on nixpkgs for M1 macs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
  };

  outputs = { self, nixpkgs, cargo2nix, zig, flake-utils }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ cargo2nix.overlay ];
        };

        llvmPkgs = pkgs.llvmPackages_13;
        zig-toolchain = zig.packages.${system}."0.9.1";
        common-deps = [
                  pkgs.libffi
                  pkgs.libxml2
                  pkgs.zlib
                  pkgs.ncurses
                ];

        # create the workspace & dependencies package set
        rustPkgs = pkgs.rustBuilder.makePackageSet {
          rustChannel = "1.60.0"; # TODO get this from rust-toolchain file
          packageFun = import ./Cargo.nix;
          packageOverrides = pkgs: pkgs.rustBuilder.overrides.all ++ [
            (pkgs.rustBuilder.rustLib.makeOverride {
              name = "wasi_libc_sys";
              overrideAttrs = drv: {
                buildInputs = drv.buildInputs or [ ] ++ [
                  zig-toolchain
                ];
              };
            })
          ] ++ [
            (pkgs.rustBuilder.rustLib.makeOverride {
              name = "roc_builtins";
              overrideAttrs = drv: {
                buildInputs = drv.buildInputs or [ ] ++ [
                  zig-toolchain
                ];
                XDG_CACHE_HOME = "xdg_cache"; # prevents zig AccessDenied error github.com/ziglang/zig/issues/6810
              };
            })
          ] ++ [
            (pkgs.rustBuilder.rustLib.makeOverride {
              name = "roc_linker";
              overrideAttrs = drv: {
                buildInputs = drv.buildInputs or [ ] ++ common-deps;
              };
            })
          ] ++ [
            (pkgs.rustBuilder.rustLib.makeOverride {
              name = "roc_cli";
              overrideAttrs = drv: {
                buildInputs = drv.buildInputs or [ ] ++ common-deps ++ [
                  pkgs.xorg.libxcb
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

        workspaceShell = rustPkgs.workspaceShell {
          NIXPKGS_ALLOW_UNFREE = 1; # to run the editor with NVIDIA's closed source drivers
          LLVM_SYS_130_PREFIX = "${llvmPkgs.llvm.dev}";
          NIX_GLIBC_PATH = if pkgs.stdenv.isLinux then "${pkgs.glibc.out}/lib" else ""; # see https://github.com/rtfeldman/roc/pull/1841
          LD_LIBRARY_PATH = with pkgs;
            lib.makeLibraryPath
            ([ 
               vulkan-loader # linux-only
               xorg.libXcursor # linux-only
               xorg.libXrandr #linux-only
               xorg.libX11 # linux-only
              ]);
        };

      in rec {
        devShell = workspaceShell;

        packages = {
          # nix build .#roc
          # nix build .#packages.x86_64-linux.roc
          roc = (rustPkgs.workspace.roc_cli {}).bin;
        };

        # nix build
        defaultPackage = packages.roc;

        formatter = pkgs.nixpkgs-fmt;
    }
  );
}
